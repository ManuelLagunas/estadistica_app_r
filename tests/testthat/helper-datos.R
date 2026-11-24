# Datos compartidos por las pruebas. testthat carga los archivos helper-*.R
# antes de cualquier prueba. Son los mismos casos del reporte en Python.

# Conjunto usado en la mayoría de las pruebas de una variable.
D <- c(2, 4, 4, 4, 5, 5, 7, 9)

# Parejas para las medidas de relación: sin empates (x, y) y con empates.
X <- c(2, 4, 6, 8)
Y <- c(1, 3, 2, 5)
XE <- c(1, 2, 2, 3)
YE <- c(1, 2, 3, 3)

#' Lee un archivo de datos_prueba con leer_csv
#'
#' La carpeta está fuera del paquete (.Rbuildignore), así que la prueba se
#' omite si no existe, por ejemplo al revisar el paquete ya construido.
leer <- function(nombre) {
  ruta <- testthat::test_path("..", "..", "datos_prueba", nombre)
  testthat::skip_if_not(file.exists(ruta), paste("No se encontró", nombre))
  leer_csv(ruta)
}

#' Escribe un texto en un archivo temporal y devuelve su ruta
archivo_temporal <- function(contenido, codificacion = "UTF-8") {
  ruta <- tempfile(fileext = ".csv")
  bytes <- if (is.raw(contenido)) contenido else charToRaw(iconv(contenido, "UTF-8", codificacion))
  writeBin(bytes, ruta)
  ruta
}
