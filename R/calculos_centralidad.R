#' Media de los datos
#'
#' @param datos Vector numérico ya validado.
#' @return Promedio aritmético: suma de los valores entre la cantidad de datos.
#' @export
media <- function(datos) {
  mean(datos)
}

#' Mediana de los datos
#'
#' @param datos Vector numérico ya validado.
#' @return Valor central de los datos ordenados. Con cantidad par de datos,
#'   el promedio de los dos valores centrales.
#' @export
mediana <- function(datos) {
  stats::median(datos)
}

#' Valor o valores más frecuentes
#'
#' @param datos Vector numérico ya validado.
#' @return Vector con la moda o modas, ordenadas. Vector vacío si ningún
#'   valor se repite.
#' @export
moda <- function(datos) {
  valores <- unique(datos)
  conteo <- tabulate(match(datos, valores))

  if (max(conteo) == 1) {
    return(numeric(0))
  }
  sort(valores[conteo == max(conteo)])
}

#' Media armónica de los datos
#'
#' @param datos Vector numérico ya validado.
#' @return Cantidad de datos entre la suma de los valores recíprocos.
#' @section Errores: lanza `error_estadistica` si algún valor es menor o
#'   igual a cero.
#' @export
media_armonica <- function(datos) {
  if (any(datos <= 0)) {
    error_estadistica("La media armónica requiere valores mayores que cero")
  }
  length(datos) / sum(1 / datos)
}

#' Media geométrica de los datos
#'
#' @param datos Vector numérico ya validado.
#' @return Raíz n-ésima del producto de los datos.
#' @section Errores: lanza `error_estadistica` si algún valor es menor o
#'   igual a cero.
#' @export
media_geometrica <- function(datos) {
  if (any(datos <= 0)) {
    error_estadistica("La media geométrica requiere valores mayores que cero")
  }
  # Por logaritmos para evitar desbordamiento con muchos datos
  exp(mean(log(datos)))
}
