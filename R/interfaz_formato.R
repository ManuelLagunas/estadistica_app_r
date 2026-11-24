#' Redondea a 2 decimales y quita los ceros sobrantes
#'
#' @param valor Número o vector de números a mostrar.
#' @return Texto con a lo más 2 decimales y sin ceros finales: 4.50 se
#'   muestra como 4.5 y 4.00 como 4.
#' @noRd
formatear <- function(valor) {
  texto <- sprintf("%.2f", valor)
  texto <- sub("0+$", "", texto)
  sub("\\.$", "", texto)
}
