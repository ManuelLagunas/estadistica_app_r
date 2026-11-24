# Paletas basadas en Okabe-Ito: distinguibles también con daltonismo.
# plotly para R no trae las plantillas plotly_white y plotly_dark de
# Python, así que el texto y la rejilla también se definen aquí.
PALETA_CLARA <- list(
  texto = "#2A3F5F",
  rejilla = "#EBF0F8",
  barras = "#C9D1D9",
  borde = "#FFFFFF",
  densidad = "#333333",
  media = "#D55E00",
  mediana = "#0072B2",
  moda = "#009E73",
  armonica = "#CC79A7",
  geometrica = "#E69F00",
  grupos = c("#0072B2", "#D55E00", "#009E73", "#CC79A7", "#E69F00", "#56B4E9")
)

PALETA_OSCURA <- list(
  texto = "#F2F5FA",
  rejilla = "#283442",
  barras = "#4A5562",
  borde = "#0E1117",
  densidad = "#E6E6E6",
  media = "#F28E2B",
  mediana = "#56B4E9",
  moda = "#3CC49A",
  armonica = "#E89BC5",
  geometrica = "#F0E442",
  grupos = c("#56B4E9", "#F28E2B", "#3CC49A", "#E89BC5", "#F0E442", "#9ED8F5")
)

# Escala RdBu de plotly para Python: rojo en -1, blanco en 0 y azul en 1.
# El nombre "RdBu" en plotly para R usa la escala de plotly.js, que va al
# revés (azul en el mínimo), por eso se escribe completa.
ESCALA_CORRELACION <- list(
  list(0.0, "rgb(103,0,31)"), list(0.1, "rgb(178,24,43)"),
  list(0.2, "rgb(214,96,77)"), list(0.3, "rgb(244,165,130)"),
  list(0.4, "rgb(253,219,199)"), list(0.5, "rgb(247,247,247)"),
  list(0.6, "rgb(209,229,240)"), list(0.7, "rgb(146,197,222)"),
  list(0.8, "rgb(67,147,195)"), list(0.9, "rgb(33,102,172)"),
  list(1.0, "rgb(5,48,97)")
)

#' Elige la paleta según el tema
#'
#' @param oscuro Si es `TRUE`, la paleta para fondo oscuro.
#' @return Una de `PALETA_CLARA` o `PALETA_OSCURA`.
#' @noRd
elegir_paleta <- function(oscuro) {
  if (isTRUE(oscuro)) PALETA_OSCURA else PALETA_CLARA
}

#' Aplica el estilo común: fondo transparente, colores del tema y leyenda abajo
#'
#' @param fig Figura de plotly.
#' @param paleta Paleta elegida con `elegir_paleta()`.
#' @param leyenda_abajo Si es `TRUE`, la leyenda va en horizontal debajo.
#' @return La figura con el estilo aplicado.
#' @noRd
aplicar_estilo <- function(fig, paleta, leyenda_abajo = TRUE) {
  eje <- list(gridcolor = paleta$rejilla, zerolinecolor = paleta$rejilla)
  fig <- plotly::layout(
    fig,
    paper_bgcolor = "rgba(0,0,0,0)",
    plot_bgcolor = "rgba(0,0,0,0)",
    font = list(color = paleta$texto),
    xaxis = eje,
    yaxis = eje
  )
  if (leyenda_abajo) {
    fig <- plotly::layout(fig, legend = list(orientation = "h", yanchor = "top", y = -0.2, x = 0))
  }
  plotly::config(fig, displaylogo = FALSE)
}

#' Intervalos del histograma con la regla "auto" de numpy
#'
#' Ancho = el menor entre Sturges y Freedman-Diaconis (este último sin
#' bajar de la mitad de la regla de la raíz), en intervalos iguales del
#' mínimo al máximo. Con datos enteros el ancho no baja de 1. Así el
#' histograma coincide con el de la versión en Python.
#'
#' @param datos Vector numérico ya validado.
#' @return Bordes de los intervalos.
#' @noRd
intervalos_auto <- function(datos) {
  n <- length(datos)
  minimo <- min(datos)
  maximo <- max(datos)

  if (minimo == maximo) {
    return(c(minimo - 0.5, maximo + 0.5))
  }

  amplitud <- maximo - minimo
  ancho_sturges <- amplitud / (log2(n) + 1)
  ancho_fd <- 2 * stats::IQR(datos) * n^(-1 / 3)
  ancho_raiz <- amplitud / sqrt(n)
  ancho <- min(max(ancho_fd, ancho_raiz / 2), ancho_sturges)

  # Como numpy con datos enteros: el ancho no baja de 1.
  if (all(datos == round(datos))) {
    ancho <- max(ancho, 1)
  }

  seq(minimo, maximo, length.out = ceiling(amplitud / ancho) + 1)
}

#' Densidad estimada con núcleo gaussiano y la regla de Scott
#'
#' Equivale a `scipy.stats.gaussian_kde`: ancho de banda
#' h = s · n^(-1/5), con s la desviación muestral. Se evalúa la fórmula
#' exacta en cada punto, sin la aproximación de `stats::density()`.
#'
#' @param datos Vector numérico ya validado.
#' @param puntos Valores de x donde se evalúa.
#' @return Densidad en cada punto.
#' @noRd
densidad_scott <- function(datos, puntos) {
  h <- stats::sd(datos) * length(datos)^(-1 / 5)
  vapply(puntos, function(p) mean(stats::dnorm(p, mean = datos, sd = h)), numeric(1))
}

#' Agrega a la figura una línea vertical con su valor en la leyenda
#'
#' @param fig Figura de plotly.
#' @param valor Posición de la línea en el eje x.
#' @param nombre Nombre de la medida.
#' @param color Color de la línea.
#' @param estilo Tipo de trazo ("dash", "dot", "dashdot"...).
#' @param altura Hasta dónde llega la línea en el eje y.
#' @return La figura con la línea agregada.
#' @noRd
linea_vertical <- function(fig, valor, nombre, color, estilo, altura) {
  etiqueta <- paste0(nombre, ": ", formatear(valor))
  plotly::add_trace(
    fig,
    x = c(valor, valor), y = c(0, altura),
    type = "scatter", mode = "lines", name = etiqueta,
    line = list(color = color, dash = estilo, width = 2),
    hovertemplate = paste0(etiqueta, "<extra></extra>")
  )
}

#' Histograma interactivo con las medidas de tendencia central
#'
#' @param datos Vector numérico ya validado.
#' @param oscuro Si es `TRUE`, usa colores pensados para fondo oscuro.
#' @return Figura de plotly con el histograma de frecuencias, la curva de
#'   densidad escalada a frecuencias (si hay al menos 3 datos y no todos
#'   son iguales) y una línea vertical por cada medida de tendencia
#'   central. Las medias armónica y geométrica se omiten si no aplican.
#' @noRd
grafica_distribucion <- function(datos, oscuro = FALSE) {
  paleta <- elegir_paleta(oscuro)
  n <- length(datos)

  bordes <- intervalos_auto(datos)
  k <- length(bordes) - 1
  # Cada intervalo incluye su borde izquierdo; el último, también el derecho.
  conteos <- tabulate(
    findInterval(datos, bordes, rightmost.closed = TRUE, all.inside = TRUE), k
  )
  ancho <- bordes[2] - bordes[1]
  centros <- (bordes[-1] + bordes[-length(bordes)]) / 2
  altura <- max(conteos)

  fig <- plotly::plot_ly()

  fig <- plotly::add_bars(
    fig,
    x = centros, y = conteos, width = ancho, name = "Frecuencia",
    marker = list(color = paleta$barras, line = list(color = paleta$borde, width = 1)),
    text = sprintf("Intervalo: %.2f – %.2f", bordes[-length(bordes)], bordes[-1]),
    textposition = "none",
    hovertemplate = "%{text}<br>Frecuencia: %{y}<extra></extra>"
  )

  # Curva de densidad escalada: área = n × ancho, igual que el histograma.
  if (n >= 3 && length(unique(datos)) > 1) {
    margen <- stats::sd(datos)
    x <- seq(min(datos) - margen, max(datos) + margen, length.out = 200)
    y <- densidad_scott(datos, x) * n * ancho
    altura <- max(altura, y)
    fig <- plotly::add_trace(
      fig,
      x = x, y = y, type = "scatter", mode = "lines", name = "Densidad estimada",
      line = list(color = paleta$densidad, width = 2),
      hovertemplate = "Valor: %{x:.2f}<br>Frecuencia estimada: %{y:.2f}<extra></extra>"
    )
  }

  # Las líneas llegan un poco más arriba que la barra o curva más alta.
  altura <- altura * 1.05

  medidas <- list(
    list("Media", media, "media", "dash"),
    list("Mediana", mediana, "mediana", "dot"),
    list("Media armónica", media_armonica, "armonica", "longdash"),
    list("Media geométrica", media_geometrica, "geometrica", "longdashdot")
  )
  for (medida in medidas) {
    valor <- tryCatch(medida[[2]](datos), error_estadistica = function(e) NULL)
    if (is.null(valor)) {
      next # No aplica con estos datos (ceros o negativos).
    }
    fig <- linea_vertical(fig, valor, medida[[1]], paleta[[medida[[3]]]], medida[[4]], altura)
  }
  for (valor_moda in moda(datos)) {
    fig <- linea_vertical(fig, valor_moda, "Moda", paleta$moda, "dashdot", altura)
  }

  # Eje y en enteros: con frecuencias bajas, una marca por unidad.
  eje_y <- list(title = "Frecuencia", rangemode = "tozero", tickformat = "d")
  if (max(conteos) <= 10) {
    eje_y$dtick <- 1
  }
  fig <- plotly::layout(fig, bargap = 0, xaxis = list(title = "Valor"), yaxis = eje_y)
  aplicar_estilo(fig, paleta)
}

#' Mapa de calor interactivo de una matriz de correlación
#'
#' @param matriz Matriz de correlación cuadrada, con valores entre -1 y 1.
#' @param oscuro Si es `TRUE`, usa colores pensados para fondo oscuro.
#' @return Figura de plotly con una celda por par de variables. La escala
#'   de color es fija de -1 (rojo) a 1 (azul), con blanco en 0, y cada
#'   celda muestra su valor con 4 decimales.
#' @noRd
mapa_calor_correlacion <- function(matriz, oscuro = FALSE) {
  paleta <- elegir_paleta(oscuro)
  variables <- colnames(matriz)

  fig <- plotly::plot_ly(
    x = variables, y = variables, z = unname(matriz), type = "heatmap",
    zmin = -1, zmax = 1, colorscale = ESCALA_CORRELACION,
    texttemplate = "%{z:.4f}",
    hovertemplate = "%{y} – %{x}: %{z:.4f}<extra></extra>",
    colorbar = list(title = "Coeficiente")
  )

  # La primera variable arriba, como se lee una tabla.
  fig <- plotly::layout(fig, yaxis = list(autorange = "reversed"))
  aplicar_estilo(fig, paleta, leyenda_abajo = FALSE)
}

#' Gráfica de dispersión interactiva de dos variables
#'
#' @param x,y Valores de los ejes, ya validados y de la misma longitud.
#' @param nombre_x,nombre_y Títulos de los ejes. En Python salían del
#'   nombre de la Series; los vectores de R no tienen nombre propio.
#' @param grupos Categoría de cada punto, alineada con x y y, o `NULL`.
#'   Si se da, cada grupo se dibuja con su color y aparece en la leyenda.
#' @param recta Resultado de [regresion_lineal()], o `NULL`. Si se da, se
#'   dibuja la recta con su ecuación y r² en la leyenda.
#' @param oscuro Si es `TRUE`, usa colores pensados para fondo oscuro.
#' @return Figura de plotly con un punto por observación.
#' @noRd
grafica_dispersion <- function(x, y, nombre_x = "x", nombre_y = "y",
                               grupos = NULL, recta = NULL, oscuro = FALSE) {
  paleta <- elegir_paleta(oscuro)
  plantilla_punto <- paste0(nombre_x, ": %{x}<br>", nombre_y, ": %{y}<extra></extra>")

  fig <- plotly::plot_ly()

  if (is.null(grupos)) {
    fig <- plotly::add_trace(
      fig,
      x = x, y = y, type = "scatter", mode = "markers", name = "Datos",
      marker = list(color = paleta$mediana, size = 8, opacity = 0.75),
      hovertemplate = plantilla_punto
    )
  } else {
    valores <- sort(unique(as.character(grupos)))
    for (i in seq_along(valores)) {
      filas <- as.character(grupos) == valores[i]
      color <- paleta$grupos[(i - 1) %% length(paleta$grupos) + 1]
      fig <- plotly::add_trace(
        fig,
        x = x[filas], y = y[filas], type = "scatter", mode = "markers",
        name = valores[i],
        marker = list(color = color, size = 8, opacity = 0.75),
        hovertemplate = paste0(valores[i], "<br>", plantilla_punto)
      )
    }
  }

  if (!is.null(recta)) {
    extremos <- c(min(x), max(x))
    signo <- if (recta$interseccion >= 0) "+" else "−"
    ecuacion <- sprintf(
      "y = %sx %s %s (r² = %.4f)",
      formatC(recta$pendiente, digits = 4, format = "g"), signo,
      formatC(abs(recta$interseccion), digits = 4, format = "g"), recta$r2
    )
    fig <- plotly::add_trace(
      fig,
      x = extremos, y = recta$interseccion + recta$pendiente * extremos,
      type = "scatter", mode = "lines", name = ecuacion,
      line = list(color = paleta$densidad, width = 2, dash = "dash"),
      hovertemplate = paste0(ecuacion, "<extra></extra>")
    )
  }

  fig <- plotly::layout(
    fig,
    xaxis = list(title = nombre_x),
    yaxis = list(title = nombre_y)
  )
  aplicar_estilo(fig, paleta)
}
