#' Convierte valores a número; lo que no se pueda queda como NA
#'
#' Equivale a `pd.to_numeric(errors = "coerce")`.
#'
#' @param x Vector de cualquier tipo.
#' @return Vector numérico del mismo largo.
#' @noRd
a_numero <- function(x) {
  suppressWarnings(as.numeric(as.character(x)))
}

#' Convierte los datos a número y descarta los que no lo son
#'
#' @param datos Vector con los valores capturados, de cualquier tipo
#'   (texto, números o vacíos).
#' @return Lista con `datos`, un vector solo con los valores numéricos, y
#'   `descartados`, la cantidad de valores que no se pudieron convertir.
#' @export
limpiar_serie <- function(datos) {
  convertida <- a_numero(datos)
  validos <- !is.na(convertida)
  limpia <- convertida[validos]

  if (length(limpia) == 0) {
    error_estadistica("No hay datos numéricos válidos para calcular")
  }

  list(datos = limpia, descartados = sum(!validos))
}

#' Identifica las filas cuyos valores son todos numéricos
#'
#' @param datos `data.frame` con valores de cualquier tipo.
#' @return Vector con los números de las filas que no tienen valores
#'   vacíos ni valores que no puedan convertirse a número.
#' @export
filas_completas <- function(datos) {
  convertida <- datos
  convertida[] <- lapply(datos, a_numero)
  which(stats::complete.cases(convertida))
}

#' Convierte las columnas a número y descarta las filas incompletas
#'
#' Una fila se descarta completa si alguna de sus columnas tiene un valor
#' no numérico o vacío, para que todas las variables conserven sus pares.
#'
#' @param datos `data.frame` con las columnas a analizar.
#' @return Lista con `datos`, la tabla solo con filas completas y
#'   numéricas, y `descartadas`, la cantidad de filas eliminadas.
#' @export
limpiar_tabla <- function(datos) {
  limpia <- datos[filas_completas(datos), , drop = FALSE]
  limpia[] <- lapply(limpia, a_numero)
  rownames(limpia) <- NULL

  if (nrow(limpia) == 0) {
    error_estadistica("No hay filas con datos numéricos válidos en todas las columnas")
  }

  list(datos = limpia, descartadas = nrow(datos) - nrow(limpia))
}
