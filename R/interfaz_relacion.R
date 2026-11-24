# Coeficientes que se ofrecen, con el texto que ve el usuario.
OPCIONES_CORRELACION <- c(
  "Pearson" = "pearson",
  "Spearman" = "spearman",
  "Kendall" = "kendall",
  "Gamma" = "gamma"
)

#' Pantalla de relación entre variables: controles
#'
#' @param id Identificador del módulo.
#' @return Interfaz de la pestaña.
#' @noRd
pantalla_relacion_ui <- function(id) {
  ns <- shiny::NS(id)

  bslib::layout_sidebar(
    sidebar = bslib::sidebar(
      width = 320,
      shiny::radioButtons(
        ns("origen"), "Origen de los datos", c("Manual", "Archivo CSV"), inline = TRUE
      ),
      shiny::conditionalPanel(
        "input.origen == 'Manual'", ns = ns,
        shiny::textInput(
          ns("nombres"), "Nombres de las variables (separados por comas)", "x, y"
        ),
        shiny::helpText(
          "Si cambias los nombres, los cuadros se reinician y se pierden los datos."
        ),
        shiny::uiOutput(ns("cuadros"))
      ),
      shiny::conditionalPanel(
        "input.origen == 'Archivo CSV'", ns = ns,
        shiny::fileInput(ns("archivo"), "Archivo CSV", accept = ".csv"),
        shiny::uiOutput(ns("elegir_columnas"))
      ),
      shiny::radioButtons(
        ns("tipo"), "Tipo de datos", c("Muestra", "Población"), inline = TRUE
      ),
      shiny::helpText("Afecta a la covarianza. La correlación da lo mismo en ambos casos."),
      shiny::selectInput(ns("metodo"), "Coeficiente de correlación", OPCIONES_CORRELACION),
      shiny::conditionalPanel(
        "input.metodo == 'kendall'", ns = ns,
        shiny::radioButtons(
          ns("variante"), "Variante de Kendall",
          stats::setNames(VARIANTES_KENDALL, paste0("tau-", VARIANTES_KENDALL)),
          selected = "b", inline = TRUE
        )
      ),
      shiny::actionButton(ns("calcular"), "Calcular", class = "btn-primary")
    ),
    shiny::conditionalPanel(
      "input.origen == 'Archivo CSV'", ns = ns,
      vista_previa_ui(ns)
    ),
    shiny::uiOutput(ns("resultados"))
  )
}

#' Pantalla de relación entre variables: lógica
#'
#' @param id Identificador del módulo.
#' @param oscuro Expresión reactiva: `TRUE` si el tema es oscuro.
#' @noRd
pantalla_relacion_server <- function(id, oscuro) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # ---------- Captura manual: un cuadro de texto por variable ----------
    nombres <- shiny::reactive({
      separados <- trimws(strsplit(input$nombres, ",")[[1]])
      separados[separados != ""]
    })

    output$cuadros <- shiny::renderUI({
      lapply(seq_along(nombres()), function(i) {
        shiny::textAreaInput(ns(paste0("variable_", i)), nombres()[i], rows = 2)
      })
    })

    # ---------- Captura desde CSV ----------
    tabla_csv <- shiny::reactive({
      shiny::req(input$archivo)
      validar_datos(leer_csv(input$archivo$datapath))
    })

    output$elegir_columnas <- shiny::renderUI({
      shiny::tagList(
        shiny::selectInput(
          ns("columnas"), "Columnas a analizar", names(tabla_csv()), multiple = TRUE
        ),
        shiny::selectInput(
          ns("color"), "Colorear la dispersión por (opcional)",
          c("Ninguna", columnas_grupo(tabla_csv()))
        )
      )
    })

    vista_previa_server(output, tabla_csv)

    # ---------- Cálculo al presionar Calcular ----------
    resultados <- shiny::eventReactive(input$calcular, {
      if (input$origen == "Manual") {
        tabla <- tabla_manual(nombres(), input)
        grupos <- NULL
      } else {
        shiny::validate(shiny::need(input$archivo, "Sube un archivo CSV para calcular"))
        shiny::validate(shiny::need(
          length(input$columnas) >= 2, "Elige al menos 2 columnas"
        ))
        tabla <- tabla_csv()[input$columnas]
        grupos <- if (input$color == "Ninguna") NULL else tabla_csv()[[input$color]]
      }

      limpia <- validar_datos(limpiar_tabla(tabla))

      list(
        tabla = limpia$datos,
        descartadas = limpia$descartadas,
        # Los grupos de las mismas filas que conservó limpiar_tabla.
        grupos = if (!is.null(grupos)) grupos[filas_completas(tabla)],
        nombre_grupos = if (!is.null(grupos)) input$color,
        muestral = input$tipo == "Muestra",
        metodo = input$metodo,
        variante = input$variante
      )
    })

    output$resultados <- shiny::renderUI({
      r <- resultados()
      shiny::tagList(
        if (r$descartadas > 0) {
          aviso(sprintf(
            "Se descartaron %d filas con valores no numéricos o vacíos", r$descartadas
          ))
        },
        seccion_covarianza(ns),
        seccion_correlacion(ns, r$metodo, r$variante),
        seccion_dispersion(ns, names(r$tabla), r$nombre_grupos)
      )
    })

    # ---------- Covarianza ----------
    output$covarianza <- shiny::renderTable(
      {
        r <- resultados()
        validar_datos(matriz_covarianza(r$tabla, muestral = r$muestral))
      },
      rownames = TRUE,
      digits = 4
    )

    # ---------- Correlación ----------
    correlacion <- shiny::reactive({
      r <- resultados()
      validar_datos(matriz_correlacion(r$tabla, metodo = r$metodo, variante = r$variante))
    })

    output$mapa_calor <- plotly::renderPlotly({
      mapa_calor_correlacion(correlacion(), oscuro = oscuro())
    })

    output$pares <- shiny::renderTable(
      tabla_pares(correlacion(), resultados()$metodo),
      digits = 4
    )

    # ---------- Dispersión: se actualiza al cambiar sus controles ----------
    output$dispersion <- plotly::renderPlotly({
      r <- resultados()
      shiny::req(input$eje_x, input$eje_y)
      shiny::validate(shiny::need(
        input$eje_x != input$eje_y, "Elige variables distintas para los ejes x y y"
      ))

      x <- r$tabla[[input$eje_x]]
      y <- r$tabla[[input$eje_y]]

      recta <- NULL
      if (isTRUE(input$recta)) {
        recta <- tryCatch(regresion_lineal(x, y), error_estadistica = function(e) NULL)
      }
      grupos <- if (isTRUE(input$colorear)) r$grupos

      grafica_dispersion(
        x, y, input$eje_x, input$eje_y,
        grupos = grupos, recta = recta, oscuro = oscuro()
      )
    })

    output$nota_recta <- shiny::renderUI({
      if (isTRUE(input$recta) && isTRUE(input$colorear)) {
        shiny::helpText(paste(
          "La recta usa todos los puntos juntos. Si dentro de cada grupo la",
          "tendencia es distinta, los grupos pueden ocultarla."
        ))
      }
    })
  })
}

#' Arma la tabla de la captura manual, una columna por variable
#'
#' Si las variables tienen distinta cantidad de valores, las faltantes
#' quedan vacías y limpiar_tabla descarta esas filas.
#'
#' @param nombres Nombres de las variables.
#' @param input Entradas del módulo, con un cuadro `variable_i` por nombre.
#' @return `data.frame` con los valores como texto.
#' @noRd
tabla_manual <- function(nombres, input) {
  shiny::validate(shiny::need(
    length(nombres) >= 2, "Escribe al menos 2 nombres de variables"
  ))
  shiny::validate(shiny::need(
    !anyDuplicated(nombres), "Los nombres de las variables no pueden repetirse"
  ))

  valores <- lapply(seq_along(nombres), function(i) {
    texto <- input[[paste0("variable_", i)]]
    if (is.null(texto)) character(0) else desde_texto(texto)
  })
  filas <- max(lengths(valores))
  shiny::validate(shiny::need(filas > 0, "Escribe los valores de cada variable"))

  # Completa con NA las variables más cortas.
  columnas <- lapply(valores, function(v) c(v, rep(NA, filas - length(v))))
  stats::setNames(as.data.frame(columnas), nombres)
}

#' Lista cada par de variables con su coeficiente e interpretación
#'
#' @param matriz Matriz de correlación cuadrada.
#' @param metodo Coeficiente con el que se calculó.
#' @return `data.frame` con una fila por par, de la correlación más fuerte
#'   a la más débil según el valor absoluto.
#' @noRd
tabla_pares <- function(matriz, metodo) {
  variables <- colnames(matriz)
  pares <- utils::combn(seq_along(variables), 2)

  valores <- matriz[t(pares)]
  tabla <- data.frame(
    "Variable 1" = variables[pares[1, ]],
    "Variable 2" = variables[pares[2, ]],
    "Coeficiente" = valores,
    "Interpretación" = vapply(valores, interpretar_correlacion, character(1), metodo = metodo),
    check.names = FALSE
  )
  tabla[order(abs(tabla$Coeficiente), decreasing = TRUE), ]
}

#' Sección de la matriz de covarianza
#'
#' @param ns Espacio de nombres del módulo.
#' @noRd
seccion_covarianza <- function(ns) {
  seccion(
    "Covarianza",
    tabla_con_desplazamiento(ns("covarianza")),
    shiny::helpText(
      "La diagonal contiene las varianzas. Los valores dependen de las unidades de cada variable."
    )
  )
}

#' Sección del mapa de calor y la tabla de pares
#'
#' @param ns Espacio de nombres del módulo.
#' @param metodo,variante Coeficiente elegido.
#' @noRd
seccion_correlacion <- function(ns, metodo, variante) {
  nombre <- names(OPCIONES_CORRELACION)[OPCIONES_CORRELACION == metodo]
  if (metodo == "kendall") {
    nombre <- paste0(nombre, " (tau-", variante, ")")
  }

  seccion(
    paste("Correlación de", nombre),
    plotly::plotlyOutput(ns("mapa_calor")),
    tabla_con_desplazamiento(ns("pares")),
    shiny::helpText("Una correlación alta no implica que una variable cause la otra.")
  )
}

#' Sección de la gráfica de dispersión, con sus propios controles
#'
#' Los controles están junto a la gráfica: al cambiarlos solo se vuelve a
#' dibujar la dispersión, sin repetir la covarianza ni la correlación.
#'
#' @param ns Espacio de nombres del módulo.
#' @param variables Nombres de las variables calculadas.
#' @param nombre_grupos Columna para colorear, o `NULL`.
#' @noRd
seccion_dispersion <- function(ns, variables, nombre_grupos) {
  seccion(
    "Gráfica de dispersión",
    bslib::layout_columns(
      shiny::selectInput(ns("eje_x"), "Eje x", variables, selected = variables[1]),
      shiny::selectInput(ns("eje_y"), "Eje y", variables, selected = variables[2])
    ),
    shiny::checkboxInput(ns("recta"), "Mostrar recta de regresión"),
    if (!is.null(nombre_grupos)) {
      shiny::checkboxInput(ns("colorear"), paste("Colorear por", nombre_grupos), value = TRUE)
    },
    plotly::plotlyOutput(ns("dispersion")),
    shiny::uiOutput(ns("nota_recta"))
  )
}
