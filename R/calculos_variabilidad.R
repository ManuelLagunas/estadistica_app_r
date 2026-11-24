#' Rango de los datos
#'
#' @param datos Vector numérico ya validado.
#' @return Diferencia entre el valor máximo y el mínimo.
#' @export
rango <- function(datos) {
  max(datos) - min(datos)
}

#' Rango intercuartílico de los datos
#'
#' @param datos Vector numérico ya validado.
#' @return Diferencia entre el tercer cuartil y el primero. Cuartiles por
#'   interpolación lineal (`type = 7`, igual que pandas y `CUARTIL.INC`
#'   de Excel).
#' @export
rango_intercuartilico <- function(datos) {
  stats::IQR(datos, type = 7)
}

#' Varianza de los datos
#'
#' @param datos Vector numérico ya validado.
#' @param muestral Si es `TRUE`, divide entre n - 1 (muestra); si es
#'   `FALSE`, entre n (población).
#' @return Promedio de los cuadrados de las desviaciones respecto a la
#'   media, expresado en unidades al cuadrado.
#' @section Errores: lanza `error_estadistica` si es muestral y hay menos
#'   de 2 datos.
#' @export
varianza <- function(datos, muestral = TRUE) {
  n <- length(datos)

  if (muestral && n < 2) {
    error_estadistica("La varianza muestral requiere al menos 2 datos")
  }

  # stats::var() siempre divide entre n - 1; con ddof se elige el divisor,
  # como en pandas.
  ddof <- if (muestral) 1 else 0
  sum((datos - mean(datos))^2) / (n - ddof)
}

#' Desviación estándar de los datos
#'
#' @inheritParams varianza
#' @return Raíz cuadrada de la varianza, en las mismas unidades que los datos.
#' @export
desviacion_estandar <- function(datos, muestral = TRUE) {
  sqrt(varianza(datos, muestral = muestral))
}
