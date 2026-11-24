# Umbrales (moderada, fuerte) según |coeficiente|. Los de Kendall son los
# equivalentes aproximados de 0.3 y 0.7 en Pearson para datos normales;
# gamma no tiene equivalencia establecida y usa los de Pearson.
UMBRALES_CORRELACION <- list(
  pearson = c(0.3, 0.7),
  spearman = c(0.3, 0.7),
  kendall = c(0.2, 0.5),
  gamma = c(0.3, 0.7)
)

# Formas de expresar una tabla de contingencia como porcentaje.
PORCENTAJES <- c("fila", "columna", "total")

# Variantes disponibles del coeficiente de Kendall.
VARIANTES_KENDALL <- c("a", "b", "c")

#' Revisa que dos variables se puedan correlacionar
#'
#' @param x,y Vectores numéricos ya validados.
#' @return `TRUE` de forma invisible si la pareja es válida.
#' @section Errores: lanza `error_estadistica` si tienen longitudes
#'   distintas, hay menos de 2 pares o alguna variable no varía.
#' @noRd
validar_pareja <- function(x, y) {
  if (length(x) != length(y)) {
    error_estadistica("Las dos variables deben tener la misma cantidad de datos")
  }
  if (length(x) < 2) {
    error_estadistica("La correlación requiere al menos 2 pares de datos")
  }
  if (length(unique(x)) == 1 || length(unique(y)) == 1) {
    error_estadistica("La correlación no está definida si una variable no varía")
  }
  invisible(TRUE)
}

#' Clasifica cada par de observaciones según cómo cambian x y y
#'
#' Para cada observación i compara, en una sola operación, su cambio
#' contra todas las observaciones j > i. Un par empatado en ambas
#' variables cuenta en Tx y en Ty.
#'
#' @param x,y Vectores ya revisados con `validar_pareja()`.
#' @return Lista con la cantidad de pares: `C` concordantes (x y y cambian
#'   en el mismo sentido), `D` discordantes (en sentido contrario), `Tx`
#'   empatados en x y `Ty` empatados en y.
#' @noRd
contar_pares <- function(x, y) {
  n <- length(x)
  C <- D <- Tx <- Ty <- 0

  for (i in seq_len(n - 1)) {
    j <- (i + 1):n
    cambio_x <- x[j] - x[i]
    cambio_y <- y[j] - y[i]
    producto <- cambio_x * cambio_y

    C <- C + sum(producto > 0)
    D <- D + sum(producto < 0)
    Tx <- Tx + sum(cambio_x == 0)
    Ty <- Ty + sum(cambio_y == 0)
  }

  list(C = C, D = D, Tx = Tx, Ty = Ty)
}

#' Covarianza entre dos variables
#'
#' @param x,y Vectores numéricos ya validados, de la misma longitud.
#' @param muestral Si es `TRUE`, divide entre n - 1 (muestra); si es
#'   `FALSE`, entre n (población).
#' @return Positiva si las variables tienden a subir juntas, negativa si
#'   una sube cuando la otra baja y cercana a 0 si no hay relación lineal.
#'   Su valor depende de las unidades de x y y.
#' @section Errores: lanza `error_estadistica` si x y y tienen longitudes
#'   distintas, o si es muestral y hay menos de 2 datos.
#' @export
covarianza <- function(x, y, muestral = TRUE) {
  n <- length(x)

  if (n != length(y)) {
    error_estadistica("Las dos variables deben tener la misma cantidad de datos")
  }
  if (muestral && n < 2) {
    error_estadistica("La covarianza muestral requiere al menos 2 datos")
  }

  ddof <- if (muestral) 1 else 0
  sum((x - mean(x)) * (y - mean(y))) / (n - ddof)
}

#' Matriz de covarianza entre varias variables
#'
#' @param datos `data.frame` de valores numéricos ya validados, con una
#'   columna por variable y una fila por observación.
#' @inheritParams covarianza
#' @return Matriz cuadrada con una fila y una columna por variable. La
#'   diagonal contiene las varianzas y el resto las covarianzas entre cada
#'   par; es simétrica.
#' @section Errores: lanza `error_estadistica` si hay menos de 2 columnas,
#'   o si es muestral y hay menos de 2 filas.
#' @export
matriz_covarianza <- function(datos, muestral = TRUE) {
  if (ncol(datos) < 2) {
    error_estadistica("La matriz de covarianza requiere al menos 2 variables")
  }
  if (muestral && nrow(datos) < 2) {
    error_estadistica("La matriz de covarianza muestral requiere al menos 2 filas")
  }

  ddof <- if (muestral) 1 else 0
  # Resta a cada columna su media; crossprod(X) es t(X) %*% X, que suma los
  # productos de las desviaciones para cada par de columnas a la vez.
  centrados <- scale(as.matrix(datos), center = TRUE, scale = FALSE)
  matriz <- crossprod(centrados) / (nrow(datos) - ddof)
  attr(matriz, "scaled:center") <- NULL
  matriz
}

#' Coeficiente de correlación de Pearson
#'
#' @inheritParams covarianza
#' @return Coeficiente entre -1 y 1 que mide la relación lineal entre las
#'   variables. No depende de las unidades.
#' @section Errores: lanza `error_estadistica` si las variables tienen
#'   longitudes distintas, hay menos de 2 pares o alguna no varía.
#' @export
correlacion_pearson <- function(x, y) {
  validar_pareja(x, y)

  # El factor n - 1 aparece arriba y abajo, así que se cancela.
  covarianza(x, y) / (desviacion_estandar(x) * desviacion_estandar(y))
}

#' Coeficiente de correlación de Spearman
#'
#' Es la correlación de Pearson aplicada a los rangos de los datos. A los
#' valores empatados se les asigna el rango promedio.
#'
#' @param x,y Vectores numéricos u ordinales ya validados, de la misma
#'   longitud.
#' @return Coeficiente entre -1 y 1 que mide la relación monótona. No le
#'   afectan los valores extremos.
#' @section Errores: los mismos que [correlacion_pearson()].
#' @export
correlacion_spearman <- function(x, y) {
  correlacion_pearson(rank(x), rank(y))
}

#' Coeficiente tau de Kendall en su variante a, b o c
#'
#' Las tres variantes comparten el numerador C - D y difieren en cómo
#' tratan los empates. Sin empates, las tres dan el mismo resultado.
#'
#' @inheritParams correlacion_spearman
#' @param variante `"a"` divide entre el total de pares (datos sin
#'   empates); `"b"` corrige por los empates de cada variable (tablas con
#'   cantidades parecidas de categorías); `"c"` corrige según la variable
#'   con menos categorías (tablas rectangulares).
#' @return Coeficiente entre -1 y 1 basado en la diferencia entre pares
#'   concordantes y discordantes.
#' @section Errores: lanza `error_estadistica` si la variante no existe o
#'   en los mismos casos que [correlacion_pearson()].
#' @export
correlacion_kendall <- function(x, y, variante = "b") {
  if (!variante %in% VARIANTES_KENDALL) {
    error_estadistica(sprintf(
      "Variante desconocida: %s. Opciones: %s",
      variante, paste(VARIANTES_KENDALL, collapse = ", ")
    ))
  }

  validar_pareja(x, y)
  conteo <- contar_pares(x, y)

  n <- length(x)
  total_pares <- n * (n - 1) / 2
  diferencia <- conteo$C - conteo$D

  if (variante == "a") {
    return(diferencia / total_pares)
  }
  if (variante == "b") {
    return(diferencia / sqrt((total_pares - conteo$Tx) * (total_pares - conteo$Ty)))
  }

  # Variante c: m es la cantidad de categorías de la variable con menos.
  m <- min(length(unique(x)), length(unique(y)))
  2 * m * diferencia / (n^2 * (m - 1))
}

#' Coeficiente gamma de Goodman-Kruskal
#'
#' @inheritParams correlacion_spearman
#' @return Coeficiente entre -1 y 1: (C - D) / (C + D). Solo considera los
#'   pares sin empate, por lo que es útil con datos ordinales que se
#'   repiten mucho, como escalas de 1 a 5.
#' @section Errores: los mismos que [correlacion_pearson()].
#' @export
correlacion_gamma <- function(x, y) {
  validar_pareja(x, y)
  conteo <- contar_pares(x, y)

  pares_sin_empate <- conteo$C + conteo$D

  # Protección: si validar_pareja confirma que x y y varían, siempre hay
  # al menos un par sin empate, así que hoy esta condición no se cumple.
  if (pares_sin_empate == 0) {
    error_estadistica("Gamma no está definida si todos los pares tienen algún empate")
  }

  (conteo$C - conteo$D) / pares_sin_empate
}

#' Describe en palabras la fuerza y el sentido de una correlación
#'
#' @param valor Coeficiente calculado con alguna de las funciones de
#'   correlación.
#' @param metodo Coeficiente que se calculó: `"pearson"`, `"spearman"`,
#'   `"kendall"` (cualquier variante) o `"gamma"`. Cada uno usa sus propios
#'   umbrales de `UMBRALES_CORRELACION`.
#' @return Texto con la fuerza (débil, moderada o fuerte) y el sentido
#'   (positiva o negativa) de la relación.
#' @section Errores: lanza `error_estadistica` si el método no existe.
#' @export
interpretar_correlacion <- function(valor, metodo = "pearson") {
  if (!metodo %in% names(UMBRALES_CORRELACION)) {
    error_estadistica(sprintf(
      "Método desconocido: %s. Opciones: %s",
      metodo, paste(names(UMBRALES_CORRELACION), collapse = ", ")
    ))
  }

  umbrales <- UMBRALES_CORRELACION[[metodo]]
  sentido <- if (valor > 0) "positiva" else "negativa"
  fuerza <- abs(valor)

  if (fuerza >= umbrales[2]) {
    return(sprintf("Correlación %s fuerte", sentido))
  }
  if (fuerza >= umbrales[1]) {
    return(sprintf("Correlación %s moderada", sentido))
  }
  "Correlación débil o nula"
}

# Métodos disponibles para la matriz de correlación.
METODOS_CORRELACION <- list(
  pearson = correlacion_pearson,
  spearman = correlacion_spearman,
  kendall = correlacion_kendall,
  gamma = correlacion_gamma
)

#' Matriz de correlación con el coeficiente elegido
#'
#' @inheritParams matriz_covarianza
#' @param metodo Coeficiente a usar: `"pearson"`, `"spearman"`,
#'   `"kendall"` o `"gamma"`.
#' @param variante Variante de Kendall (`"a"`, `"b"` o `"c"`). Solo se usa
#'   cuando `metodo` es `"kendall"`.
#' @return Matriz cuadrada con una fila y una columna por variable. La
#'   diagonal vale 1 y es simétrica.
#' @section Errores: lanza `error_estadistica` si el método o la variante
#'   no existen, hay menos de 2 columnas o alguna columna no varía.
#' @export
matriz_correlacion <- function(datos, metodo = "pearson", variante = "b") {
  if (!metodo %in% names(METODOS_CORRELACION)) {
    error_estadistica(sprintf(
      "Método desconocido: %s. Opciones: %s",
      metodo, paste(names(METODOS_CORRELACION), collapse = ", ")
    ))
  }
  if (ncol(datos) < 2) {
    error_estadistica("La matriz de correlación requiere al menos 2 variables")
  }

  no_varian <- vapply(datos, function(columna) length(unique(columna)) == 1, logical(1))
  if (any(no_varian)) {
    error_estadistica(paste0(
      "Estas columnas no varían y no se pueden correlacionar: ",
      paste(names(datos)[no_varian], collapse = ", ")
    ))
  }

  funcion <- METODOS_CORRELACION[[metodo]]
  coeficiente <- function(x, y) {
    if (metodo == "kendall") funcion(x, y, variante = variante) else funcion(x, y)
  }

  # Matriz identidad: la diagonal ya vale 1. Solo se calcula la mitad de
  # arriba y se copia a la de abajo, porque la matriz es simétrica.
  k <- ncol(datos)
  matriz <- diag(k)
  dimnames(matriz) <- list(names(datos), names(datos))

  for (i in seq_len(k - 1)) {
    for (j in (i + 1):k) {
      valor <- coeficiente(datos[[i]], datos[[j]])
      matriz[i, j] <- valor
      matriz[j, i] <- valor
    }
  }
  matriz
}

#' Recta de mínimos cuadrados y = a + b·x
#'
#' La pendiente es b = cov(x, y) / var(x) y la intersección es
#' a = ȳ - b·x̄. El coeficiente de determinación r² es el cuadrado de la
#' correlación de Pearson.
#'
#' @param x Variable independiente, valores numéricos ya validados.
#' @param y Variable dependiente, de la misma longitud que x.
#' @return Lista con `pendiente`, `interseccion` y `r2`.
#' @section Errores: los mismos que [correlacion_pearson()].
#' @export
regresion_lineal <- function(x, y) {
  validar_pareja(x, y)

  # El factor n - 1 de la covarianza y de la varianza se cancela.
  pendiente <- covarianza(x, y) / varianza(x)

  list(
    pendiente = pendiente,
    interseccion = mean(y) - pendiente * mean(x),
    r2 = correlacion_pearson(x, y)^2
  )
}

#' Cruza dos variables categóricas y cuenta cada combinación
#'
#' Las filas con un valor vacío en cualquiera de las dos variables no se
#' cuentan.
#'
#' @param datos `data.frame` con las variables.
#' @param fila Variable cuyos valores forman las filas.
#' @param columna Variable cuyos valores forman las columnas.
#' @param porcentaje `NULL` para frecuencias; `"fila"` divide cada fila
#'   entre su total, `"columna"` cada columna entre su total y `"total"`
#'   todas las celdas entre el gran total. Se expresa de 0 a 100.
#' @return Tabla (clase `table`) con una fila y una columna por valor, más
#'   la fila y la columna `"Total"`.
#' @section Errores: lanza `error_estadistica` si alguna variable no
#'   existe, si son la misma, si el porcentaje no es una opción válida o si
#'   no hay datos.
#' @export
tabla_contingencia <- function(datos, fila, columna, porcentaje = NULL) {
  for (variable in c(fila, columna)) {
    if (!variable %in% names(datos)) {
      error_estadistica(sprintf("La columna '%s' no existe en los datos", variable))
    }
  }
  if (fila == columna) {
    error_estadistica("Elige dos variables distintas para las filas y las columnas")
  }
  if (!is.null(porcentaje) && !porcentaje %in% PORCENTAJES) {
    error_estadistica(sprintf(
      "Porcentaje desconocido: %s. Opciones: %s",
      porcentaje, paste(PORCENTAJES, collapse = ", ")
    ))
  }

  # dnn pone los nombres de las variables como encabezados de la tabla.
  conteos <- table(datos[[fila]], datos[[columna]], dnn = c(fila, columna))

  if (sum(conteos) == 0) {
    error_estadistica("No hay filas con valores en ambas variables")
  }

  # addmargins agrega la fila y la columna de totales, que llama "Sum".
  tabla <- stats::addmargins(conteos)
  dimnames(tabla) <- lapply(dimnames(tabla), function(nombres) {
    replace(nombres, nombres == "Sum", "Total")
  })

  if (is.null(porcentaje)) {
    return(tabla)
  }
  if (porcentaje == "fila") {
    tabla <- sweep(tabla, 1, tabla[, "Total"], "/") * 100
  } else if (porcentaje == "columna") {
    tabla <- sweep(tabla, 2, tabla["Total", ], "/") * 100
  } else {
    tabla <- tabla / tabla["Total", "Total"] * 100
  }
  tabla
}
