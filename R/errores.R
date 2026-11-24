#' Lanza un error de datos con la clase `error_estadistica`
#'
#' Equivale a `raise ValueError(...)` de Python: marca los errores causados
#' por los datos (por ejemplo, una media armónica con un 0), para que la
#' aplicación los distinga de un error del programa.
#'
#' @param mensaje Texto que se mostrará al usuario.
#' @keywords internal
error_estadistica <- function(mensaje) {
  stop(errorCondition(mensaje, class = "error_estadistica", call = sys.call(-1)))
}
