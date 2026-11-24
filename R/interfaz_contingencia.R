# Texto de cada opción y el porcentaje que le corresponde en tabla_contingencia.
OPCIONES_PORCENTAJE <- c(
  "Frecuencias" = "ninguno",
  "% por fila" = "fila",
  "% por columna" = "columna",
  "% del total" = "total"
)

#' Pantalla de tablas de contingencia: controles
#'
#' @param id Identificador del módulo.
#' @return Interfaz de la pestaña.
#' @noRd
pantalla_contingencia_ui <- function(id) {
  ns <- shiny::NS(id)

  bslib::layout_sidebar(
    sidebar = bslib::sidebar(
      width = 320,
      shiny::fileInput(ns("archivo"), "Archivo CSV", accept = ".csv"),
      shiny::uiOutput(ns("controles"))
    ),
    vista_previa_ui(ns),
    shiny::uiOutput(ns("resultado"))
  )
}

#' Pantalla de tablas de contingencia: lógica
#'
#' La tabla se actualiza al cambiar cualquier control, sin botón: no hay
#' azar ni cálculos pesados.
#'
#' @param id Identificador del módulo.
#' @noRd
pantalla_contingencia_server <- function(id) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns

    datos <- shiny::reactive({
      shiny::req(input$archivo)
      validar_datos(leer_csv(input$archivo$datapath))
    })

    vista_previa_server(output, datos)

    grupos <- shiny::reactive({
      disponibles <- columnas_grupo(datos())
      shiny::validate(shiny::need(
        length(disponibles) >= 2,
        sprintf(
          "Se necesitan al menos 2 columnas con entre 2 y %d valores distintos.",
          LIMITE_GRUPOS
        )
      ))
      disponibles
    })

    output$controles <- shiny::renderUI({
      shiny::tagList(
        shiny::selectInput(ns("fila"), "Variable de las filas", grupos()),
        shiny::selectInput(
          ns("columna"), "Variable de las columnas", grupos(), selected = grupos()[2]
        ),
        shiny::radioButtons(ns("mostrar"), "Mostrar", OPCIONES_PORCENTAJE)
      )
    })

    output$resultado <- shiny::renderUI({
      # Al subir otro archivo, las variables elegidas pueden ya no existir.
      shiny::req(
        input$fila %in% names(datos()), input$columna %in% names(datos()), input$mostrar
      )
      porcentaje <- if (input$mostrar == "ninguno") NULL else input$mostrar
      tabla <- validar_datos(
        tabla_contingencia(datos(), input$fila, input$columna, porcentaje)
      )

      completas <- stats::complete.cases(datos()[c(input$fila, input$columna)])
      excluidas <- sum(!completas)

      shiny::tagList(
        if (excluidas > 0) {
          aviso(sprintf("No se contaron %d filas con valores vacíos", excluidas))
        },
        seccion(
          paste(input$fila, "×", input$columna),
          shiny::div(
            style = "max-height: 450px; overflow: auto;",
            shiny::renderTable(tabla_visible(tabla, input$fila, porcentaje), align = "r")
          ),
          shiny::helpText(explicar_porcentaje(porcentaje, input$fila, input$columna))
        )
      )
    })
  })
}

#' Prepara la tabla de contingencia para mostrarla
#'
#' @param tabla Resultado de [tabla_contingencia()].
#' @param fila Variable de las filas, que encabeza la primera columna.
#' @param porcentaje `NULL` para frecuencias, o el tipo de porcentaje.
#' @return `data.frame` con los valores ya formateados como texto.
#' @noRd
tabla_visible <- function(tabla, fila, porcentaje) {
  valores <- unclass(tabla)
  texto <- if (is.null(porcentaje)) {
    sprintf("%d", as.integer(valores))
  } else {
    sprintf("%.1f %%", valores)
  }
  matriz <- matrix(texto, nrow = nrow(valores), dimnames = dimnames(valores))

  visible <- data.frame(rownames(matriz), matriz, check.names = FALSE)
  names(visible)[1] <- fila
  visible
}

#' Explica cómo leer la tabla según el tipo de porcentaje
#'
#' @param porcentaje `NULL`, `"fila"`, `"columna"` o `"total"`.
#' @param fila,columna Variables de la tabla.
#' @return Texto de la explicación.
#' @noRd
explicar_porcentaje <- function(porcentaje, fila, columna) {
  if (is.null(porcentaje)) {
    return("Cantidad de casos en cada combinación.")
  }
  switch(porcentaje,
    fila = sprintf(
      "Cada fila suma 100 %%: de los casos con cada valor de %s, qué porcentaje tiene cada valor de %s.",
      fila, columna
    ),
    columna = sprintf(
      "Cada columna suma 100 %%: de los casos con cada valor de %s, qué porcentaje tiene cada valor de %s.",
      columna, fila
    ),
    total = "Todas las celdas suman 100 %."
  )
}
