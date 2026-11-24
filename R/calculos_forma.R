# Umbrales prácticos: valores dentro de ±0.5 se consideran cercanos a 0.
UMBRAL_ASIMETRIA <- 0.5
UMBRAL_CURTOSIS <- 0.5

#' Momentos centrales de orden 2, 3 y 4
#'
#' Promedio de las desviaciones respecto a la media elevadas a cada
#' potencia. Los usan la asimetría y la curtosis.
#'
#' @param datos Vector numérico ya validado.
#' @return Lista con `m2`, `m3` y `m4`.
#' @noRd
momentos_centrales <- function(datos) {
  desviaciones <- datos - mean(datos)
  list(
    m2 = mean(desviaciones^2),
    m3 = mean(desviaciones^3),
    m4 = mean(desviaciones^4)
  )
}

#' Asimetría de los datos
#'
#' @param datos Vector numérico ya validado.
#' @param muestral Si es `TRUE`, aplica la corrección muestral (igual que
#'   `COEFICIENTE.ASIMETRIA` de Excel); si es `FALSE`, calcula la asimetría
#'   poblacional.
#' @return Positiva si la cola larga está a la derecha (pocos valores muy
#'   altos), negativa si está a la izquierda y cercana a 0 si la
#'   distribución es simétrica.
#' @section Errores: lanza `error_estadistica` si es muestral y hay menos
#'   de 3 datos, o si todos los valores son iguales.
#' @export
asimetria <- function(datos, muestral = TRUE) {
  n <- length(datos)

  if (muestral && n < 3) {
    error_estadistica("La asimetría muestral requiere al menos 3 datos")
  }
  if (length(unique(datos)) == 1) {
    error_estadistica("La asimetría no está definida si todos los valores son iguales")
  }

  m <- momentos_centrales(datos)
  g1 <- m$m3 / m$m2^1.5

  if (!muestral) {
    return(g1)
  }
  # Corrección muestral (Fisher-Pearson ajustada), igual que pandas y Excel
  g1 * sqrt(n * (n - 1)) / (n - 2)
}

#' Curtosis en exceso de los datos
#'
#' @param datos Vector numérico ya validado.
#' @param muestral Si es `TRUE`, aplica la corrección muestral (igual que
#'   `CURTOSIS` de Excel); si es `FALSE`, calcula la curtosis poblacional.
#' @return Curtosis en exceso (normal = 0): positiva si los datos tienen más
#'   valores extremos que una normal (leptocúrtica), negativa si tienen
#'   menos (platicúrtica).
#' @section Errores: lanza `error_estadistica` si es muestral y hay menos
#'   de 4 datos, o si todos los valores son iguales.
#' @export
curtosis <- function(datos, muestral = TRUE) {
  n <- length(datos)

  if (muestral && n < 4) {
    error_estadistica("La curtosis muestral requiere al menos 4 datos")
  }
  if (length(unique(datos)) == 1) {
    error_estadistica("La curtosis no está definida si todos los valores son iguales")
  }

  m <- momentos_centrales(datos)
  g2 <- m$m4 / m$m2^2 - 3

  if (!muestral) {
    return(g2)
  }
  # Corrección muestral, igual que pandas y CURTOSIS de Excel
  ((n + 1) * g2 + 6) * (n - 1) / ((n - 2) * (n - 3))
}

#' Describe en palabras el valor de la asimetría
#'
#' @param valor Asimetría calculada con [asimetria()].
#' @return Texto con la lectura del valor. Dentro de ±`UMBRAL_ASIMETRIA` se
#'   considera aproximadamente simétrica.
#' @export
interpretar_asimetria <- function(valor) {
  if (valor > UMBRAL_ASIMETRIA) {
    return("Sesgo a la derecha: cola larga hacia valores altos")
  }
  if (valor < -UMBRAL_ASIMETRIA) {
    return("Sesgo a la izquierda: cola larga hacia valores bajos")
  }
  "Aproximadamente simétrica"
}

#' Describe en palabras el valor de la curtosis en exceso
#'
#' @param valor Curtosis en exceso calculada con [curtosis()].
#' @return Texto con la lectura del valor. Dentro de ±`UMBRAL_CURTOSIS` se
#'   considera mesocúrtica.
#' @export
interpretar_curtosis <- function(valor) {
  if (valor > UMBRAL_CURTOSIS) {
    return("Leptocúrtica: más valores extremos que una normal")
  }
  if (valor < -UMBRAL_CURTOSIS) {
    return("Platicúrtica: menos valores extremos que una normal")
  }
  "Mesocúrtica: similar a una normal"
}
