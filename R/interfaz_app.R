#' Abre la calculadora estadística
#'
#' Crea la aplicación Shiny con una pestaña por tipo de análisis. Cada
#' pestaña es un módulo con su propio espacio de nombres, por lo que
#' conserva sus controles y resultados al cambiar de pestaña.
#'
#' @param ... Opciones que se pasan a [shiny::shinyApp()], por ejemplo
#'   `options = list(port = 8080)`.
#' @return Un objeto de aplicación Shiny. En la consola, al imprimirse, la
#'   aplicación se abre en el navegador.
#' @export
ejecutar <- function(...) {
  shiny::shinyApp(ui = interfaz_ui(), server = interfaz_server, ...)
}

#' Interfaz: barra superior con las pestañas y el interruptor de tema
#'
#' @return Página de bslib.
#' @noRd
interfaz_ui <- function() {
  bslib::page_navbar(
    title = "Calculadora estadística",
    theme = bslib::bs_theme(version = 5),
    bslib::nav_panel("Una variable", pantalla_una_variable_ui("una_variable")),
    bslib::nav_panel("Relación entre variables", pantalla_relacion_ui("relacion")),
    bslib::nav_panel("Tablas de contingencia", pantalla_contingencia_ui("contingencia")),
    bslib::nav_panel("Muestreo", pantalla_muestreo_ui("muestreo")),
    bslib::nav_spacer(),
    bslib::nav_item(bslib::input_dark_mode(id = "modo"))
  )
}

#' Servidor: conecta cada pestaña con su módulo
#'
#' `oscuro` es una expresión reactiva: cada gráfica que la lea se vuelve a
#' dibujar sola cuando el usuario cambia el tema.
#'
#' @param input,output,session Argumentos que Shiny da a todo servidor.
#' @noRd
interfaz_server <- function(input, output, session) {
  oscuro <- shiny::reactive(identical(input$modo, "dark"))
  pantalla_una_variable_server("una_variable", oscuro)
  pantalla_relacion_server("relacion", oscuro)
  pantalla_contingencia_server("contingencia")
  pantalla_muestreo_server("muestreo")
}
