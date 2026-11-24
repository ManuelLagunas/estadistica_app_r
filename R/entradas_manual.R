#' Separa el texto capturado en un vector de valores
#'
#' @param texto Valores escritos por el usuario, separados por comas,
#'   punto y coma, espacios o saltos de línea.
#' @return Vector de texto con un elemento por valor, sin fragmentos
#'   vacíos. Los valores aún no se convierten a número.
#' @export
desde_texto <- function(texto) {
  fragmentos <- strsplit(trimws(texto), "[,;[:space:]]+")[[1]]
  fragmentos[fragmentos != ""]
}
