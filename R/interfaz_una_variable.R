#' Pantalla de una variable: controles
#'
#' @param id Identificador del módulo.
#' @return Interfaz de la pestaña.
#' @noRd
pantalla_una_variable_ui <- function(id) {
  ns <- shiny::NS(id)

  bslib::layout_sidebar(
    sidebar = bslib::sidebar(
      width = 320,
      shiny::radioButtons(
        ns("origen"), "Origen de los datos",
        c("Manual", "Archivo CSV"), inline = TRUE
      ),
      shiny::conditionalPanel(
        "input.origen == 'Manual'", ns = ns,
        shiny::textAreaInput(
          ns("texto"), "Datos (separados por comas o espacios)", rows = 6,
          placeholder = "Usa punto para los decimales: 7.5"
        )
      ),
      shiny::conditionalPanel(
        "input.origen == 'Archivo CSV'", ns = ns,
        shiny::fileInput(ns("archivo"), "Archivo CSV", accept = ".csv"),
        shiny::uiOutput(ns("elegir_columna"))
      ),
      shiny::radioButtons(
        ns("tipo"), "Tipo de datos", c("Muestra", "Población"), inline = TRUE
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

#' Pantalla de una variable: lógica
#'
#' @param id Identificador del módulo.
#' @param oscuro Expresión reactiva: `TRUE` si el tema es oscuro.
#' @noRd
pantalla_una_variable_server <- function(id, oscuro) {
  shiny::moduleServer(id, function(input, output, session) {
    tabla <- shiny::reactive({
      shiny::req(input$archivo)
      validar_datos(leer_csv(input$archivo$datapath))
    })

    output$elegir_columna <- shiny::renderUI({
      shiny::selectInput(session$ns("columna"), "Columna a analizar", names(tabla()))
    })

    vista_previa_server(output, tabla)

    # Se calcula solo al presionar Calcular; los controles que lee se toman
    # con el valor que tenían en ese momento.
    resultados <- shiny::eventReactive(input$calcular, {
      if (input$origen == "Manual") {
        entrada <- desde_texto(input$texto)
      } else {
        shiny::validate(shiny::need(
          input$archivo, "Sube un archivo CSV válido para calcular"
        ))
        shiny::req(input$columna)
        entrada <- tabla()[[input$columna]]
      }

      limpios <- validar_datos(limpiar_serie(entrada))
      c(limpios, muestral = input$tipo == "Muestra")
    })

    output$resultados <- shiny::renderUI({
      r <- resultados()
      shiny::tagList(
        if (r$descartados > 0) {
          aviso(sprintf("Se descartaron %d valores no numéricos", r$descartados))
        },
        seccion_centralidad(r$datos),
        seccion_variabilidad(r$datos, r$muestral),
        seccion_forma(r$datos, r$muestral),
        seccion("Gráfica de distribución", plotly::plotlyOutput(session$ns("grafica")))
      )
    })

    # La gráfica depende del tema: al cambiarlo se vuelve a dibujar sin
    # repetir los cálculos.
    output$grafica <- plotly::renderPlotly({
      grafica_distribucion(resultados()$datos, oscuro = oscuro())
    })
  })
}

#' Tarjetas de tendencia central
#'
#' @param datos Valores numéricos ya limpios.
#' @noRd
seccion_centralidad <- function(datos) {
  modas <- moda(datos)
  texto_moda <- if (length(modas) > 0) paste(formatear(modas), collapse = ", ") else "Sin moda"

  seccion(
    "Tendencia central",
    bslib::layout_columns(
      metrica("Media", formatear(media(datos))),
      metrica("Mediana", formatear(mediana(datos))),
      metrica("Moda", texto_moda)
    ),
    bslib::layout_columns(
      metrica_o_motivo("Media armónica", medida_o_motivo(media_armonica(datos))),
      metrica_o_motivo("Media geométrica", medida_o_motivo(media_geometrica(datos)))
    )
  )
}

#' Tarjetas de variabilidad
#'
#' @param datos Valores numéricos ya limpios.
#' @param muestral Si es `TRUE`, la varianza y la desviación dividen entre
#'   n - 1; si es `FALSE`, entre n.
#' @noRd
seccion_variabilidad <- function(datos, muestral) {
  seccion(
    "Variabilidad",
    bslib::layout_columns(
      metrica("Rango", formatear(rango(datos))),
      metrica_o_motivo(
        "Varianza", medida_o_motivo(varianza(datos, muestral)), "En unidades al cuadrado"
      ),
      metrica_o_motivo(
        "Desviación estándar", medida_o_motivo(desviacion_estandar(datos, muestral))
      ),
      metrica("Rango intercuartílico", formatear(rango_intercuartilico(datos)))
    )
  )
}

#' Tarjetas de forma, con su interpretación
#'
#' @param datos Valores numéricos ya limpios.
#' @param muestral Si es `TRUE`, aplica la corrección muestral.
#' @noRd
seccion_forma <- function(datos, muestral) {
  valor_asimetria <- medida_o_motivo(asimetria(datos, muestral))
  valor_curtosis <- medida_o_motivo(curtosis(datos, muestral))

  seccion(
    "Forma",
    bslib::layout_columns(
      metrica_o_motivo(
        "Asimetría", valor_asimetria,
        if (!is.null(valor_asimetria$valor)) interpretar_asimetria(valor_asimetria$valor)
      ),
      metrica_o_motivo(
        "Curtosis (en exceso)", valor_curtosis,
        if (!is.null(valor_curtosis$valor)) interpretar_curtosis(valor_curtosis$valor)
      )
    )
  )
}
