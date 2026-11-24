# Técnicas que se ofrecen en la pantalla.
TECNICAS <- c(
  "Aleatorio simple",
  "Sistemático",
  "Estratificado",
  "Por conglomerados",
  "Por cuotas (no probabilístico)"
)

# Técnicas que piden el tamaño de muestra n.
TECNICAS_CON_N <- c("Aleatorio simple", "Sistemático", "Estratificado")

#' Pantalla de muestreo: controles
#'
#' @param id Identificador del módulo.
#' @return Interfaz de la pestaña.
#' @noRd
pantalla_muestreo_ui <- function(id) {
  ns <- shiny::NS(id)
  con_n <- paste0("['", paste(TECNICAS_CON_N, collapse = "','"), "'].includes(input.tecnica)")

  bslib::layout_sidebar(
    sidebar = bslib::sidebar(
      width = 340,
      shiny::fileInput(ns("archivo"), "Archivo CSV de la población", accept = ".csv"),
      shiny::selectInput(ns("tecnica"), "Técnica de muestreo", TECNICAS),
      # n está fuera de los controles de cada técnica para que conserve su
      # valor al cambiar de técnica y para que la calculadora pueda llenarlo.
      shiny::conditionalPanel(
        con_n, ns = ns,
        shiny::numericInput(ns("n"), "Tamaño de muestra n", value = 10, min = 1, step = 1)
      ),
      shiny::uiOutput(ns("parametros")),
      shiny::numericInput(
        ns("semilla"), "Semilla (opcional)", value = NA, min = 0, step = 1
      ),
      shiny::helpText(
        "Vacía: una muestra distinta cada vez. Con la misma semilla se obtiene siempre la misma muestra."
      ),
      shiny::actionButton(ns("tomar"), "Tomar muestra", class = "btn-primary")
    ),
    bslib::accordion(
      open = FALSE,
      bslib::accordion_panel(
        "Vista previa de la población",
        shiny::textOutput(ns("vista_previa_tamano")),
        tabla_con_desplazamiento(ns("vista_previa_tabla"))
      ),
      bslib::accordion_panel(
        "Calcular el tamaño de muestra",
        shiny::uiOutput(ns("calculadora"))
      )
    ),
    shiny::uiOutput(ns("resultados"))
  )
}

#' Pantalla de muestreo: lógica
#'
#' @param id Identificador del módulo.
#' @noRd
pantalla_muestreo_server <- function(id) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns

    poblacion <- shiny::reactive({
      shiny::req(input$archivo)
      validar_datos(leer_csv(input$archivo$datapath))
    })

    vista_previa_server(output, poblacion)

    # La muestra se guarda aquí; equivale a st.session_state en Python.
    estado <- shiny::reactiveVal(NULL)

    # Con otra población, la muestra anterior ya no corresponde y n vuelve
    # a su valor inicial.
    shiny::observeEvent(poblacion(), {
      estado(NULL)
      shiny::updateNumericInput(session, "n", value = min(10, nrow(poblacion())))
    })

    # ---------- Calculadora de tamaño de muestra ----------
    output$calculadora <- shiny::renderUI({
      numericas <- columnas_numericas(poblacion())
      shiny::tagList(
        shiny::radioButtons(
          ns("objetivo"), "Se quiere estimar", c("Una media", "Una proporción"),
          inline = TRUE
        ),
        shiny::selectInput(
          ns("confianza"), "Nivel de confianza",
          c("90 %" = 0.90, "95 %" = 0.95, "99 %" = 0.99), selected = 0.95
        ),
        shiny::conditionalPanel(
          "input.objetivo == 'Una media'", ns = ns,
          if (length(numericas) == 0) {
            shiny::helpText("El archivo no tiene columnas numéricas para estimar una media")
          } else {
            shiny::tagList(
              shiny::selectInput(ns("referencia"), "Variable de referencia para σ", numericas),
              shiny::uiOutput(ns("parametros_media"))
            )
          }
        ),
        shiny::conditionalPanel(
          "input.objetivo == 'Una proporción'", ns = ns,
          bslib::layout_columns(
            shiny::numericInput(
              ns("proporcion"), "Proporción esperada p", 0.5, min = 0, max = 1, step = 0.05
            ),
            shiny::numericInput(
              ns("error_proporcion"), "Margen de error E (proporción)", 0.05,
              min = 0.001, max = 0.999, step = 0.01
            )
          ),
          shiny::helpText("Si no se conoce p, 0.5 da el tamaño más grande.")
        ),
        shiny::checkboxInput(
          ns("finita"),
          sprintf("Corrección para población finita (N = %d)", nrow(poblacion())),
          value = TRUE
        ),
        shiny::uiOutput(ns("tamano_sugerido")),
        shiny::actionButton(ns("usar_tamano"), "Usar este tamaño")
      )
    })

    # σ y E se proponen a partir de la columna elegida.
    output$parametros_media <- shiny::renderUI({
      shiny::req(input$referencia)
      datos <- validar_datos(limpiar_serie(poblacion()[[input$referencia]]))$datos
      sigma <- desviacion_estandar(datos, muestral = FALSE)
      bslib::layout_columns(
        shiny::numericInput(ns("sigma"), "Desviación estándar σ", round(sigma, 4), min = 0),
        shiny::numericInput(
          ns("error_media"), "Margen de error E (en unidades de la variable)",
          round(max(sigma / 4, 1e-4), 4), min = 1e-4
        )
      )
    })

    tamano_sugerido <- shiny::reactive({
      shiny::req(input$objetivo, input$confianza)
      confianza <- as.numeric(input$confianza)
      n_poblacion <- if (isTRUE(input$finita)) nrow(poblacion())

      if (input$objetivo == "Una media") {
        shiny::req(input$sigma, input$error_media)
        validar_datos(
          tamano_muestra_media(input$sigma, input$error_media, confianza, n_poblacion)
        )
      } else {
        shiny::req(input$proporcion, input$error_proporcion)
        validar_datos(tamano_muestra_proporcion(
          input$error_proporcion, confianza, input$proporcion, n_poblacion
        ))
      }
    })

    output$tamano_sugerido <- shiny::renderUI({
      formula <- if (identical(input$objetivo, "Una media")) {
        "Fórmula: n = (z · σ / E)²"
      } else {
        "Fórmula: n = z² · p(1 − p) / E²"
      }
      metrica("Tamaño de muestra sugerido", tamano_sugerido(), formula)
    })

    # Copia el tamaño sugerido al campo n de la técnica.
    shiny::observeEvent(input$usar_tamano, {
      shiny::updateNumericInput(session, "n", value = tamano_sugerido())
    })

    # ---------- Controles propios de cada técnica ----------
    output$parametros <- shiny::renderUI({
      tecnica <- input$tecnica
      if (tecnica == "Aleatorio simple") {
        return(shiny::checkboxInput(ns("reemplazo"), "Con reemplazo"))
      }
      if (tecnica == "Sistemático") {
        return(NULL)
      }

      grupos <- columnas_grupo(poblacion())
      if (length(grupos) == 0) {
        return(shiny::helpText(sprintf(
          "Ninguna columna tiene entre 2 y %d valores distintos para formar grupos.",
          LIMITE_GRUPOS
        )))
      }

      if (tecnica == "Estratificado") {
        return(shiny::tagList(
          shiny::selectInput(ns("estratos"), "Columna de estratos", grupos),
          shiny::radioButtons(
            ns("asignacion"), "Asignación",
            stats::setNames(ASIGNACIONES, c("Proporcional", "Igual", "Neyman")),
            inline = TRUE
          ),
          shiny::conditionalPanel(
            "input.asignacion == 'neyman'", ns = ns,
            shiny::selectInput(
              ns("variable_neyman"), "Variable para la asignación de Neyman",
              columnas_numericas(poblacion())
            )
          ),
          shiny::uiOutput(ns("reparto"))
        ))
      }

      if (tecnica == "Por conglomerados") {
        return(shiny::tagList(
          shiny::selectInput(ns("conglomerado"), "Columna de conglomerados", grupos),
          shiny::uiOutput(ns("cantidad_conglomerados"))
        ))
      }

      shiny::tagList(
        shiny::selectInput(ns("cuotas_columna"), "Columna de grupos", grupos),
        shiny::helpText("Cuota de cada grupo (se toman los primeros, en el orden del archivo):"),
        shiny::uiOutput(ns("cuotas"))
      )
    })

    # Reparto entre estratos, para ver la asignación antes de muestrear.
    output$reparto <- shiny::renderUI({
      shiny::req(input$estratos, input$asignacion, input$n)
      variable <- if (input$asignacion == "neyman") input$variable_neyman
      tryCatch(
        {
          reparto <- asignar_estratos(
            poblacion(), input$estratos, input$n, input$asignacion, variable
          )
          shiny::helpText(paste(
            "Elementos por estrato:",
            paste(names(reparto), reparto, sep = ": ", collapse = ", ")
          ))
        },
        error_estadistica = function(e) aviso(conditionMessage(e))
      )
    })

    output$cantidad_conglomerados <- shiny::renderUI({
      shiny::req(input$conglomerado)
      columna <- poblacion()[[input$conglomerado]]
      disponibles <- length(unique(columna[!is.na(columna)]))
      shiny::numericInput(
        ns("conglomerados"), sprintf("Conglomerados a elegir (de %d)", disponibles),
        value = 1, min = 1, max = disponibles, step = 1
      )
    })

    grupos_cuotas <- shiny::reactive({
      shiny::req(input$cuotas_columna)
      tamanos <- contar_grupos(poblacion()[[input$cuotas_columna]])
      tamanos[orden_grupos(names(tamanos), poblacion()[[input$cuotas_columna]])]
    })

    output$cuotas <- shiny::renderUI({
      tamanos <- grupos_cuotas()
      lapply(seq_along(tamanos), function(i) {
        shiny::numericInput(
          ns(paste0("cuota_", i)), sprintf("%s (de %d)", names(tamanos)[i], tamanos[[i]]),
          value = 0, min = 0, max = tamanos[[i]], step = 1
        )
      })
    })

    # ---------- Tomar la muestra ----------
    shiny::observeEvent(input$tomar, {
      semilla <- if (is.na(input$semilla)) NULL else as.integer(input$semilla)
      resultado <- tryCatch(
        tomar_muestra(poblacion(), input, grupos_cuotas, semilla),
        error_estadistica = function(e) list(error = conditionMessage(e))
      )
      estado(resultado)
    })

    output$resultados <- shiny::renderUI({
      r <- estado()
      shiny::req(r)
      if (!is.null(r$error)) {
        return(shiny::div(class = "alert alert-danger py-2", r$error))
      }

      grupos <- columnas_grupo(poblacion())
      numericas <- columnas_numericas(poblacion())

      shiny::tagList(
        seccion(
          "Muestra",
          shiny::helpText(r$descripcion),
          bslib::layout_columns(
            metrica("Población (N)", nrow(poblacion())),
            metrica("Muestra (n)", nrow(r$muestra)),
            metrica(
              "Fracción de muestreo",
              sprintf("%.1f %%", 100 * nrow(r$muestra) / nrow(poblacion()))
            )
          ),
          tabla_con_desplazamiento(ns("tabla_muestra")),
          shiny::div(shiny::downloadButton(ns("descargar"), "Descargar la muestra (CSV)"))
        ),
        if (length(numericas) > 0) {
          seccion(
            "Comparación numérica",
            shiny::selectInput(ns("comparar"), "Columna a comparar", numericas),
            shiny::uiOutput(ns("comparacion_numerica"))
          )
        } else {
          seccion(
            "Comparación numérica",
            shiny::helpText("La población no tiene columnas numéricas para comparar.")
          )
        },
        if (length(grupos) > 0) {
          seleccion <- if (!is.null(r$columna) && r$columna %in% grupos) r$columna else grupos[1]
          seccion(
            "Comparación de grupos",
            shiny::selectInput(
              ns("comparar_grupos"), "Columna de grupos a comparar", grupos,
              selected = seleccion
            ),
            tabla_con_desplazamiento(ns("comparacion_grupos")),
            shiny::helpText(paste(
              "Porcentaje de elementos de cada grupo. Una diferencia cercana a 0",
              "indica que la muestra conserva la composición de la población."
            ))
          )
        }
      )
    })

    output$tabla_muestra <- shiny::renderTable({
      muestra <- estado()$muestra
      data.frame("Fila original" = rownames(muestra), como_texto(muestra), check.names = FALSE)
    })

    output$descargar <- shiny::downloadHandler(
      filename = "muestra.csv",
      content = function(archivo) {
        utils::write.csv(estado()$muestra, archivo, row.names = FALSE, fileEncoding = "UTF-8")
      }
    )

    output$comparacion_numerica <- shiny::renderUI({
      r <- estado()
      shiny::req(r$muestra, input$comparar)
      comparacion_numerica(poblacion(), r$muestra, input$comparar, r$estratos)
    })

    output$comparacion_grupos <- shiny::renderTable(
      {
        shiny::req(input$comparar_grupos %in% names(poblacion()))
        comparacion_grupos(poblacion(), estado()$muestra, input$comparar_grupos)
      },
      align = "lrrr"
    )
  })
}

#' Ordena los grupos como `sort_index()` de pandas
#'
#' Si la columna es numérica, el orden es numérico (4, 6, 8 y no "4",
#' "6", "8" como texto, que pondría "10" antes que "4").
#'
#' @param nombres Nombres de los grupos, como texto.
#' @param columna Columna original.
#' @return Posiciones que ordenan `nombres`.
#' @noRd
orden_grupos <- function(nombres, columna) {
  if (is.numeric(columna)) order(as.numeric(nombres)) else order(nombres)
}

#' Aplica la técnica elegida a la población
#'
#' @param poblacion `data.frame` de la población.
#' @param input Entradas del módulo.
#' @param grupos_cuotas Expresión reactiva con los grupos de las cuotas.
#' @param semilla Semilla o `NULL`.
#' @return Lista con `muestra`, `descripcion`, `columna` (columna de grupos
#'   usada, o `NULL`) y `estratos` (columna de estratos si la muestra es
#'   estratificada, o `NULL`).
#' @noRd
tomar_muestra <- function(poblacion, input, grupos_cuotas, semilla) {
  tecnica <- input$tecnica
  columna <- NULL
  detalle <- NULL

  if (tecnica == "Aleatorio simple") {
    muestra <- muestreo_aleatorio_simple(poblacion, input$n, isTRUE(input$reemplazo), semilla)
    if (isTRUE(input$reemplazo)) detalle <- "con reemplazo"
  } else if (tecnica == "Sistemático") {
    muestra <- muestreo_sistematico(poblacion, input$n, semilla)
  } else {
    columna <- switch(tecnica,
      "Estratificado" = input$estratos,
      "Por conglomerados" = input$conglomerado,
      input$cuotas_columna
    )
    if (is.null(columna)) {
      error_estadistica("No hay una columna adecuada para formar grupos")
    }

    if (tecnica == "Estratificado") {
      variable <- if (input$asignacion == "neyman") input$variable_neyman
      muestra <- muestreo_estratificado(
        poblacion, columna, input$n, input$asignacion, variable, semilla
      )
      detalle <- paste0("(", input$asignacion, ")")
    } else if (tecnica == "Por conglomerados") {
      muestra <- muestreo_conglomerados(poblacion, columna, input$conglomerados, semilla)
    } else {
      tamanos <- grupos_cuotas()
      cuotas <- vapply(seq_along(tamanos), function(i) {
        valor <- input[[paste0("cuota_", i)]]
        if (is.null(valor) || is.na(valor)) 0 else valor
      }, numeric(1))
      names(cuotas) <- names(tamanos)
      if (sum(cuotas) == 0) {
        error_estadistica("Asigna una cuota mayor que cero al menos a un grupo")
      }
      muestra <- muestreo_cuotas(poblacion, columna, cuotas)
    }
  }

  descripcion <- paste(c(tecnica, detalle, if (!is.null(columna)) paste("según", columna)),
    collapse = " "
  )
  if (!startsWith(tecnica, "Por cuotas")) {
    descripcion <- paste0(
      descripcion,
      if (is.null(semilla)) ", sin semilla" else paste0(", semilla ", semilla)
    )
  }

  list(
    muestra = muestra,
    descripcion = descripcion,
    columna = columna,
    estratos = if (tecnica == "Estratificado") columna
  )
}

#' Tabla que compara medidas de una columna en la población y la muestra
#'
#' La población usa la desviación poblacional (σ, divide entre N) y la
#' muestra la muestral (s, divide entre n - 1), porque s estima σ.
#'
#' @param poblacion,muestra `data.frame` de la población y de la muestra.
#' @param columna Columna numérica a comparar.
#' @param estratos Columna de estratos si la muestra es estratificada, o
#'   `NULL`; en ese caso se agrega la media estratificada.
#' @return Elementos HTML con la tabla y sus notas.
#' @noRd
comparacion_numerica <- function(poblacion, muestra, columna, estratos = NULL) {
  pob <- validar_datos(limpiar_serie(poblacion[[columna]]))$datos
  limpia <- validar_datos(limpiar_serie(muestra[[columna]]))
  mue <- limpia$datos

  o_nada <- function(calculo) tryCatch(calculo, error_estadistica = function(e) NA)

  filas <- list(c("Media", media(pob), media(mue)))
  if (!is.null(estratos)) {
    filas <- c(filas, list(c(
      "Media estratificada", media(pob),
      o_nada(media_estratificada(muestra, poblacion, estratos, columna))
    )))
  }
  filas <- c(filas, list(
    c("Mediana", mediana(pob), mediana(mue)),
    c(
      "Desviación estándar (σ / s)",
      desviacion_estandar(pob, muestral = FALSE),
      o_nada(desviacion_estandar(mue, muestral = TRUE))
    ),
    c("Mínimo", min(pob), min(mue)),
    c("Máximo", max(pob), max(mue))
  ))

  medida <- vapply(filas, `[`, character(1), 1)
  valor_pob <- as.numeric(vapply(filas, `[`, character(1), 2))
  valor_mue <- as.numeric(vapply(filas, `[`, character(1), 3))
  diferencia <- ifelse(valor_pob != 0, (valor_mue - valor_pob) / abs(valor_pob) * 100, NA)

  formato <- function(x, plantilla) ifelse(is.na(x), "", sprintf(plantilla, x))
  tabla <- data.frame(
    "Medida" = medida,
    "Población" = formato(valor_pob, "%.4f"),
    "Muestra" = formato(valor_mue, "%.4f"),
    "Diferencia" = formato(diferencia, "%.2f %%"),
    check.names = FALSE
  )

  shiny::tagList(
    if (limpia$descartados > 0) {
      aviso(sprintf(
        "En la muestra se descartaron %d valores no numéricos", limpia$descartados
      ))
    },
    shiny::renderTable(tabla, align = "lrrr"),
    shiny::helpText(paste(
      "La desviación de la población divide entre N (σ) y la de la muestra entre",
      "n − 1 (s), porque s se usa para estimar σ. Diferencia: cuánto se aleja la",
      "muestra de la población, en porcentaje."
    )),
    if (!is.null(estratos)) {
      shiny::helpText(paste(
        "Media estratificada: promedia cada estrato y lo pondera por su peso en la",
        "población. Corrige la sobrerrepresentación de algunos estratos en la",
        "asignación igual o de Neyman."
      ))
    }
  )
}

#' Compara qué porcentaje ocupa cada grupo en la población y la muestra
#'
#' @param poblacion,muestra `data.frame` de la población y de la muestra.
#' @param columna Columna de grupos.
#' @return `data.frame` con el porcentaje de cada grupo y su diferencia en
#'   puntos porcentuales.
#' @noRd
comparacion_grupos <- function(poblacion, muestra, columna) {
  en_poblacion <- contar_grupos(poblacion[[columna]])
  en_muestra <- contar_grupos(muestra[[columna]])

  grupos <- names(en_poblacion)[orden_grupos(names(en_poblacion), poblacion[[columna]])]
  pct_poblacion <- 100 * en_poblacion[grupos] / sum(en_poblacion)
  pct_muestra <- 100 * en_muestra[grupos] / sum(en_muestra)
  pct_muestra[is.na(pct_muestra)] <- 0

  data.frame(
    "Grupo" = grupos,
    "Población" = sprintf("%.1f %%", pct_poblacion),
    "Muestra" = sprintf("%.1f %%", pct_muestra),
    "Diferencia" = sprintf("%+.1f", pct_muestra - pct_poblacion),
    check.names = FALSE
  )
}
