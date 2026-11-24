#' Convierte un error de datos en un mensaje de validación de Shiny
#'
#' Equivale al `except ValueError as error: st.error(str(error))` de
#' Python: si la expresión lanza `error_estadistica`, la salida que la
#' usa muestra el mensaje y Shiny detiene lo que depende de ella. Los
#' demás errores no se atrapan, porque serían errores del programa.
#'
#' @param expresion Código que puede lanzar `error_estadistica`.
#' @return El valor de la expresión.
#' @noRd
validar_datos <- function(expresion) {
  tryCatch(
    expresion,
    error_estadistica = function(e) {
      shiny::validate(shiny::need(FALSE, conditionMessage(e)))
    }
  )
}

#' Calcula una medida o devuelve el motivo por el que no aplica
#'
#' @param calculo Expresión que calcula la medida.
#' @return Lista con `valor` (número o `NULL`) y `motivo` (texto o `NULL`).
#' @noRd
medida_o_motivo <- function(calculo) {
  tryCatch(
    list(valor = calculo, motivo = NULL),
    error_estadistica = function(e) list(valor = NULL, motivo = conditionMessage(e))
  )
}

#' Tarjeta con el nombre de una medida, su valor y una nota opcional
#'
#' Equivale a `st.metric(..., border=True)` más su `st.caption`.
#'
#' @param titulo Nombre de la medida.
#' @param valor Texto del valor.
#' @param nota Texto pequeño debajo del valor, o `NULL`.
#' @return Tarjeta de bslib.
#' @noRd
metrica <- function(titulo, valor, nota = NULL) {
  bslib::card(
    bslib::card_body(
      gap = "0.25rem",
      shiny::div(class = "text-body-secondary small", titulo),
      shiny::div(class = "fs-3", valor),
      if (!is.null(nota)) shiny::div(class = "text-body-secondary small", nota)
    )
  )
}

#' Tarjeta de una medida que puede no aplicar a los datos
#'
#' @param titulo Nombre de la medida.
#' @param resultado Lista de `medida_o_motivo()`.
#' @param nota Nota que se muestra si la medida sí aplica.
#' @return Tarjeta de bslib con el valor, o "No aplica" con su motivo.
#' @noRd
metrica_o_motivo <- function(titulo, resultado, nota = NULL) {
  if (is.null(resultado$valor)) {
    return(metrica(titulo, "No aplica", resultado$motivo))
  }
  metrica(titulo, formatear(resultado$valor), nota)
}

#' Sección con título y borde, como `st.container(border=True)`
#'
#' @param titulo Encabezado de la sección.
#' @param ... Contenido.
#' @return Tarjeta de bslib.
#' @noRd
seccion <- function(titulo, ...) {
  bslib::card(bslib::card_header(titulo), bslib::card_body(...))
}

#' Mensaje de advertencia en amarillo, como `st.warning`
#'
#' @param texto Mensaje.
#' @return Elemento HTML.
#' @noRd
aviso <- function(texto) {
  shiny::div(class = "alert alert-warning py-2", texto)
}

#' Tabla con barra de desplazamiento y altura fija, como `st.dataframe`
#'
#' @param id Identificador de la salida (`tableOutput`).
#' @param altura Altura máxima del recuadro.
#' @return Elemento HTML.
#' @noRd
tabla_con_desplazamiento <- function(id, altura = "300px") {
  shiny::div(
    style = paste0("max-height: ", altura, "; overflow: auto;"),
    shiny::tableOutput(id)
  )
}

#' Vista previa plegable de un archivo: tamaño y primeras 10 filas
#'
#' @param ns Función de espacio de nombres del módulo.
#' @param id Identificador base de las salidas.
#' @return Acordeón cerrado de bslib.
#' @noRd
vista_previa_ui <- function(ns, id = "vista_previa") {
  bslib::accordion(
    open = FALSE,
    bslib::accordion_panel(
      "Vista previa del archivo",
      shiny::textOutput(ns(paste0(id, "_tamano"))),
      tabla_con_desplazamiento(ns(paste0(id, "_tabla")))
    )
  )
}

#' Llena las salidas de `vista_previa_ui()`
#'
#' @param output Salidas del módulo.
#' @param tabla Expresión reactiva con la tabla leída.
#' @param id Identificador base usado en `vista_previa_ui()`.
#' @noRd
vista_previa_server <- function(output, tabla, id = "vista_previa") {
  output[[paste0(id, "_tamano")]] <- shiny::renderText({
    sprintf("%d filas, %d columnas", nrow(tabla()), ncol(tabla()))
  })
  output[[paste0(id, "_tabla")]] <- shiny::renderTable(como_texto(utils::head(tabla(), 10)))
}

#' Convierte las columnas numéricas a texto para mostrarlas tal cual
#'
#' `renderTable` redondea los números a 2 decimales por defecto (5.10 en
#' lugar de 5.1, o 2.62 en lugar de 2.620). Como texto, cada valor se ve
#' igual que en el archivo, como en `st.dataframe`.
#'
#' @param tabla `data.frame`.
#' @return La misma tabla con las columnas numéricas como texto.
#' @noRd
como_texto <- function(tabla) {
  tabla[] <- lapply(tabla, function(columna) {
    if (is.numeric(columna)) as.character(columna) else columna
  })
  tabla
}
