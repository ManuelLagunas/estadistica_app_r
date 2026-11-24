# Una columna sirve como grupo si tiene a lo más estos valores distintos.
LIMITE_GRUPOS <- 20

#' Lista las columnas que sirven para formar grupos
#'
#' @param tabla `data.frame` con los datos.
#' @return Nombres de las columnas con entre 2 y `LIMITE_GRUPOS` valores
#'   distintos (sin contar los vacíos).
#' @noRd
columnas_grupo <- function(tabla) {
  distintos <- vapply(tabla, function(columna) {
    length(unique(columna[!is.na(columna)]))
  }, numeric(1))
  names(tabla)[distintos >= 2 & distintos <= LIMITE_GRUPOS]
}

#' Lista las columnas con al menos 2 valores numéricos
#'
#' @param tabla `data.frame` con los datos.
#' @return Nombres de las columnas que pueden usarse como variables
#'   numéricas.
#' @noRd
columnas_numericas <- function(tabla) {
  numericos <- vapply(tabla, function(columna) sum(!is.na(a_numero(columna))), numeric(1))
  names(tabla)[numericos >= 2]
}
