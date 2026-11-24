# Se prueban en orden: la primera que funcione es la que se usa.
CODIFICACIONES <- c("UTF-8", "latin1")

# Caracteres que se aceptan como separador de columnas.
SEPARADORES <- c(",", ";", "\t", "|")

#' Lee el archivo como texto UTF-8, probando las codificaciones
#'
#' Quita la marca BOM que Excel agrega al inicio de los CSV en UTF-8.
#'
#' @param ruta Ruta del archivo.
#' @return El contenido del archivo como un solo texto en UTF-8.
#' @noRd
leer_texto <- function(ruta) {
  bytes <- readBin(ruta, "raw", n = file.size(ruta))
  if (length(bytes) >= 3 && identical(bytes[1:3], as.raw(c(0xEF, 0xBB, 0xBF)))) {
    bytes <- bytes[-(1:3)]
  }
  texto <- rawToChar(bytes)

  for (codificacion in CODIFICACIONES) {
    convertido <- iconv(texto, from = codificacion, to = "UTF-8")
    if (!is.na(convertido)) {
      return(convertido)
    }
  }
  error_estadistica("No se pudo reconocer la codificación del archivo")
}

#' Detecta el separador de columnas
#'
#' Elige el primero de SEPARADORES que aparece el mismo número de veces
#' (y al menos una) en las primeras 20 líneas. Si ninguno cumple, el
#' archivo tiene una sola columna y se usa la coma.
#'
#' @param texto Contenido del archivo.
#' @return El separador detectado.
#' @noRd
detectar_separador <- function(texto) {
  lineas <- utils::head(strsplit(texto, "\r?\n")[[1]], 20)
  lineas <- lineas[lineas != ""]

  for (separador in SEPARADORES) {
    cuentas <- lengths(regmatches(lineas, gregexpr(separador, lineas, fixed = TRUE)))
    if (cuentas[1] > 0 && all(cuentas == cuentas[1])) {
      return(separador)
    }
  }
  ","
}

#' Lee un archivo CSV y lo devuelve como tabla
#'
#' Detecta el separador (coma, punto y coma, tabulador o barra vertical)
#' y prueba las codificaciones UTF-8 y Latin-1, que cubren los CSV
#' exportados por Excel.
#'
#' @param ruta Ruta del archivo, como la columna `datapath` que entrega
#'   `shiny::fileInput`.
#' @return `data.frame` con una columna por variable del archivo. Las
#'   columnas no se filtran ni se convierten a número.
#' @export
leer_csv <- function(ruta) {
  texto <- leer_texto(ruta)
  if (!nzchar(trimws(texto))) {
    error_estadistica("El archivo está vacío o no tiene formato CSV válido")
  }

  tabla <- tryCatch(
    utils::read.csv(
      text = texto, sep = detectar_separador(texto), check.names = FALSE,
      # Las celdas vacías cuentan como dato faltante, igual que en pandas.
      na.strings = c("", "NA")
    ),
    error = function(e) {
      error_estadistica("El archivo está vacío o no tiene formato CSV válido")
    }
  )

  if (nrow(tabla) == 0) {
    error_estadistica("El archivo no tiene filas de datos")
  }
  tabla
}
