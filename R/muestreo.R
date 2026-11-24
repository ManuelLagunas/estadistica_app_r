# Formas de repartir el tamaño de muestra entre estratos.
ASIGNACIONES <- c("proporcional", "igual", "neyman")

# ---------- Utilidades internas ----------

#' Evalúa código aleatorio con una semilla, sin tocar la semilla global
#'
#' Equivale a `np.random.default_rng(semilla)`: si se da una semilla, el
#' resultado se puede repetir; al terminar, el generador de R queda como
#' estaba. Con `NULL`, el resultado es distinto cada vez.
#'
#' @param semilla Número entero o `NULL`.
#' @param codigo Expresión que usa números aleatorios. R la evalúa hasta
#'   que se necesita su valor (evaluación perezosa), por eso se puede
#'   evaluar dentro de `with_seed`.
#' @return El resultado de `codigo`.
#' @noRd
con_semilla <- function(semilla, codigo) {
  if (is.null(semilla)) {
    return(codigo)
  }
  withr::with_seed(semilla, codigo)
}

#' Revisa que la columna exista en la tabla
#'
#' @param datos `data.frame` de la población.
#' @param columna Nombre de la columna.
#' @noRd
validar_columna <- function(datos, columna) {
  if (!columna %in% names(datos)) {
    error_estadistica(sprintf("La columna '%s' no existe en los datos", columna))
  }
  invisible(TRUE)
}

#' Revisa que el tamaño de muestra sea posible
#'
#' @param n Tamaño de muestra pedido.
#' @param poblacion Cantidad de elementos de la población.
#' @param reemplazo Si es `TRUE`, n puede ser mayor que la población.
#' @noRd
validar_tamano <- function(n, poblacion, reemplazo = FALSE) {
  if (n < 1) {
    error_estadistica("El tamaño de muestra debe ser al menos 1")
  }
  if (!reemplazo && n > poblacion) {
    error_estadistica(sprintf(
      "Sin reemplazo, la muestra no puede ser mayor que la población (%d elementos)",
      poblacion
    ))
  }
  invisible(TRUE)
}

#' Cuenta los elementos de cada grupo, en orden de aparición
#'
#' Equivale a `value_counts(sort=False)`. A diferencia de `table()`, no
#' ordena los grupos alfabéticamente ni convierte los valores a texto.
#'
#' @param valores Vector con el grupo de cada fila. Los vacíos se ignoran.
#' @return Vector numérico con nombres: cuántos elementos tiene cada grupo.
#' @noRd
contar_grupos <- function(valores) {
  grupos <- unique(valores[!is.na(valores)])
  tamanos <- vapply(grupos, function(grupo) sum(valores == grupo, na.rm = TRUE), numeric(1))
  stats::setNames(tamanos, as.character(grupos))
}

#' Redondea cuotas fraccionarias para que sumen exactamente n
#'
#' Método de los restos mayores: cada grupo recibe la parte entera de su
#' cuota, y las unidades que faltan van a los grupos con la parte decimal
#' más grande. En un empate gana el grupo que aparece primero.
#'
#' @param cuotas Tamaño ideal de cada grupo, posiblemente con decimales.
#' @param n Total que deben sumar los tamaños.
#' @return Tamaño entero de cada grupo, con los mismos nombres.
#' @noRd
repartir_por_restos <- function(cuotas, n) {
  enteros <- floor(cuotas)
  faltan <- n - sum(enteros)
  restos <- cuotas - enteros

  extra <- order(restos, decreasing = TRUE)[seq_len(faltan)]
  enteros[extra] <- enteros[extra] + 1
  enteros
}

# ---------- Muestreo aleatorio simple ----------

#' Selecciona n filas al azar, todas con la misma probabilidad
#'
#' @param datos `data.frame` de la población.
#' @param n Tamaño de la muestra.
#' @param reemplazo Si es `TRUE`, una fila puede salir más de una vez.
#' @param semilla Número para repetir la misma muestra; `NULL` da una
#'   muestra distinta cada vez.
#' @return Las filas seleccionadas, con su número de fila original como
#'   nombre.
#' @section Errores: lanza `error_estadistica` si n no es posible para el
#'   tamaño de la población.
#' @export
muestreo_aleatorio_simple <- function(datos, n, reemplazo = FALSE, semilla = NULL) {
  validar_tamano(n, nrow(datos), reemplazo)

  filas <- con_semilla(semilla, sample.int(nrow(datos), n, replace = reemplazo))
  datos[filas, , drop = FALSE]
}

# ---------- Muestreo sistemático ----------

#' Selecciona una fila cada k, a partir de un inicio al azar
#'
#' El salto es k = N %/% n. El inicio se elige al azar entre las primeras
#' k filas, y después se toma cada k-ésima fila.
#'
#' @inheritParams muestreo_aleatorio_simple
#' @return Las n filas seleccionadas, en su orden original.
#' @section Errores: los mismos que [muestreo_aleatorio_simple()].
#' @export
muestreo_sistematico <- function(datos, n, semilla = NULL) {
  validar_tamano(n, nrow(datos))

  salto <- nrow(datos) %/% n
  inicio <- con_semilla(semilla, sample.int(salto, 1))
  filas <- inicio + salto * (seq_len(n) - 1)

  datos[filas, , drop = FALSE]
}

# ---------- Muestreo estratificado ----------

#' Calcula cuántos elementos se toman de cada estrato
#'
#' @param datos `data.frame` de la población.
#' @param columna Columna que define los estratos.
#' @param n Tamaño total de la muestra.
#' @param asignacion `"proporcional"` (según el tamaño de cada estrato),
#'   `"igual"` (la misma cantidad en todos) o `"neyman"` (según el tamaño y
#'   la variabilidad de cada estrato).
#' @param variable Columna numérica cuya desviación estándar usa la
#'   asignación de Neyman. Solo se usa con `"neyman"`.
#' @return Vector con nombres: cantidad de elementos por estrato, en orden
#'   de aparición. La suma es n.
#' @section Errores: lanza `error_estadistica` si la columna, la variable o
#'   la asignación no existen, si n no es posible, o si algún estrato no
#'   tiene suficientes elementos.
#' @export
asignar_estratos <- function(datos, columna, n, asignacion = "proporcional",
                             variable = NULL) {
  validar_columna(datos, columna)
  validar_tamano(n, nrow(datos))

  if (!asignacion %in% ASIGNACIONES) {
    error_estadistica(sprintf(
      "Asignación desconocida: %s. Opciones: %s",
      asignacion, paste(ASIGNACIONES, collapse = ", ")
    ))
  }

  tamanos <- contar_grupos(datos[[columna]])

  if (asignacion == "proporcional") {
    cuotas <- n * tamanos / sum(tamanos)
  } else if (asignacion == "igual") {
    cuotas <- stats::setNames(rep(n / length(tamanos), length(tamanos)), names(tamanos))
  } else {
    if (is.null(variable)) {
      error_estadistica("La asignación de Neyman requiere una variable numérica")
    }
    validar_columna(datos, variable)

    # Desviación estándar de la variable dentro de cada estrato; un estrato
    # con un solo elemento no tiene desviación y cuenta como 0.
    estratos <- factor(datos[[columna]], levels = names(tamanos))
    desviaciones <- tapply(a_numero(datos[[variable]]), estratos, stats::sd, na.rm = TRUE)
    desviaciones[is.na(desviaciones)] <- 0

    # tapply devuelve un arreglo; as.vector lo deja como vector simple.
    pesos <- tamanos * as.vector(desviaciones[names(tamanos)])
    if (sum(pesos) == 0) {
      error_estadistica(paste(
        "La asignación de Neyman no aplica: la variable no varía",
        "dentro de ningún estrato"
      ))
    }
    cuotas <- n * pesos / sum(pesos)
  }

  asignados <- repartir_por_restos(cuotas, n)
  names(asignados) <- names(tamanos)

  insuficientes <- names(asignados)[asignados > tamanos]
  if (length(insuficientes) > 0) {
    detalle <- sprintf(
      "%s (pide %d, tiene %d)", insuficientes,
      as.integer(asignados[insuficientes]), as.integer(tamanos[insuficientes])
    )
    error_estadistica(paste0(
      "Estratos sin suficientes elementos: ", paste(detalle, collapse = ", ")
    ))
  }

  asignados
}

#' Divide la población en estratos y muestrea al azar dentro de cada uno
#'
#' @inheritParams asignar_estratos
#' @inheritParams muestreo_aleatorio_simple
#' @return Las filas seleccionadas de todos los estratos, agrupadas por
#'   estrato.
#' @section Errores: los mismos que [asignar_estratos()].
#' @export
muestreo_estratificado <- function(datos, columna, n, asignacion = "proporcional",
                                   variable = NULL, semilla = NULL) {
  asignados <- asignar_estratos(datos, columna, n, asignacion, variable)
  asignados <- asignados[asignados > 0]

  filas <- con_semilla(semilla, unlist(lapply(names(asignados), function(estrato) {
    del_estrato <- which(as.character(datos[[columna]]) == estrato)
    # Se sortean posiciones, no valores: sample(x) con un solo número
    # elegiría entre 1 y ese número.
    del_estrato[sample.int(length(del_estrato), asignados[[estrato]])]
  })))

  datos[filas, , drop = FALSE]
}

#' Estima la media de la población a partir de una muestra estratificada
#'
#' Promedia cada estrato por separado y pondera cada promedio por el peso
#' del estrato en la población: Σ (N_h / N) · x̄_h. Con asignación igual o
#' de Neyman, la media simple de la muestra está sesgada, porque algunos
#' estratos quedan sobrerrepresentados; esta media corrige ese efecto.
#'
#' @param muestra Muestra obtenida con [muestreo_estratificado()].
#' @param poblacion `data.frame` de la población, de donde salen los pesos
#'   N_h / N.
#' @param columna Columna que define los estratos.
#' @param variable Columna numérica cuya media se estima. Los valores que
#'   no son números se ignoran.
#' @return Media estratificada de la variable.
#' @section Errores: lanza `error_estadistica` si alguna columna no existe,
#'   o si un estrato de la población no tiene valores numéricos en la
#'   muestra.
#' @export
media_estratificada <- function(muestra, poblacion, columna, variable) {
  for (nombre in c(columna, variable)) {
    validar_columna(poblacion, nombre)
    validar_columna(muestra, nombre)
  }

  tamanos <- contar_grupos(poblacion[[columna]])
  pesos <- tamanos / sum(tamanos)

  medias <- tapply(
    a_numero(muestra[[variable]]), as.character(muestra[[columna]]),
    mean, na.rm = TRUE
  )
  # Ordena las medias como los pesos; un estrato ausente queda como NA.
  medias <- medias[names(pesos)]

  faltantes <- names(pesos)[is.na(medias)]
  if (length(faltantes) > 0) {
    error_estadistica(paste0(
      "Estratos sin valores en la muestra: ", paste(faltantes, collapse = ", ")
    ))
  }

  sum(pesos * medias)
}

# ---------- Muestreo por conglomerados ----------

#' Elige conglomerados completos al azar y toma todos sus elementos
#'
#' @param datos `data.frame` de la población.
#' @param columna Columna que define a qué conglomerado pertenece cada fila.
#' @param conglomerados Cuántos conglomerados se eligen.
#' @inheritParams muestreo_aleatorio_simple
#' @return Todas las filas de los conglomerados elegidos, en su orden
#'   original.
#' @section Errores: lanza `error_estadistica` si la columna no existe, o
#'   si se piden menos de 1 o más conglomerados de los que hay.
#' @export
muestreo_conglomerados <- function(datos, columna, conglomerados, semilla = NULL) {
  validar_columna(datos, columna)

  disponibles <- unique(datos[[columna]][!is.na(datos[[columna]])])
  if (conglomerados < 1 || conglomerados > length(disponibles)) {
    error_estadistica(sprintf(
      "Se pueden elegir de 1 a %d conglomerados", length(disponibles)
    ))
  }

  elegidos <- con_semilla(
    semilla,
    disponibles[sample.int(length(disponibles), conglomerados)]
  )
  datos[datos[[columna]] %in% elegidos, , drop = FALSE]
}

# ---------- Muestreo por cuotas ----------

#' Toma, de cada grupo, los primeros elementos hasta llenar su cuota
#'
#' No es probabilístico: recorre los datos en el orden del archivo, como un
#' encuestador que entrevista a quien llega. Si los datos están ordenados,
#' la muestra hereda ese orden.
#'
#' @param datos `data.frame` de la población.
#' @param columna Columna que define los grupos.
#' @param cuotas Vector con nombres: cantidad a tomar de cada grupo, por
#'   ejemplo `c(Setosa = 5, Versicolor = 10)`. Los grupos que no aparecen
#'   no se incluyen.
#' @return Las filas seleccionadas, en su orden original.
#' @section Errores: lanza `error_estadistica` si la columna o algún grupo
#'   no existen, si una cuota es negativa o si un grupo no tiene
#'   suficientes elementos.
#' @export
muestreo_cuotas <- function(datos, columna, cuotas) {
  validar_columna(datos, columna)
  tamanos <- contar_grupos(datos[[columna]])

  for (grupo in names(cuotas)) {
    if (!grupo %in% names(tamanos)) {
      error_estadistica(sprintf(
        "El grupo '%s' no existe en la columna '%s'", grupo, columna
      ))
    }
    if (cuotas[[grupo]] < 0) {
      error_estadistica(sprintf("La cuota de '%s' no puede ser negativa", grupo))
    }
    if (cuotas[[grupo]] > tamanos[[grupo]]) {
      error_estadistica(sprintf(
        "El grupo '%s' tiene %d elementos y la cuota pide %d",
        grupo, as.integer(tamanos[[grupo]]), as.integer(cuotas[[grupo]])
      ))
    }
  }

  # Primeras filas de cada grupo, en el orden del archivo.
  valores <- as.character(datos[[columna]])
  filas <- unlist(lapply(names(cuotas), function(grupo) {
    utils::head(which(valores == grupo), cuotas[[grupo]])
  }))

  datos[sort(filas), , drop = FALSE]
}

# ---------- Tamaño de muestra ----------

#' Valor z de la distribución normal para un nivel de confianza
#'
#' @param confianza Nivel de confianza entre 0 y 1, por ejemplo 0.95.
#' @return Valor z de dos colas: 1.96 para 0.95.
#' @noRd
valor_z <- function(confianza) {
  if (!(confianza > 0 && confianza < 1)) {
    error_estadistica("La confianza debe estar entre 0 y 1, por ejemplo 0.95")
  }
  stats::qnorm((1 + confianza) / 2)
}

#' Ajusta un tamaño de muestra para una población finita
#'
#' @param n Tamaño de muestra calculado para una población infinita.
#' @param poblacion Tamaño N de la población.
#' @return Tamaño ajustado: n / (1 + (n - 1) / N), sin redondear.
#' @section Errores: lanza `error_estadistica` si la población es menor
#'   que 1.
#' @export
correccion_finita <- function(n, poblacion) {
  if (poblacion < 1) {
    error_estadistica("La población debe tener al menos 1 elemento")
  }
  n / (1 + (n - 1) / poblacion)
}

#' Tamaño de muestra para estimar una media
#'
#' Fórmula: n = (z · σ / E)², con corrección para población finita si se
#' indica la población.
#'
#' @param desviacion Desviación estándar σ esperada de la variable.
#' @param error Margen de error E aceptado, en las unidades de la variable.
#' @param confianza Nivel de confianza entre 0 y 1.
#' @param poblacion Tamaño N de la población; `NULL` la trata como infinita.
#' @return Tamaño de muestra redondeado hacia arriba.
#' @section Errores: lanza `error_estadistica` si la desviación es
#'   negativa, el error no es positivo o la confianza no está entre 0 y 1.
#' @export
tamano_muestra_media <- function(desviacion, error, confianza = 0.95,
                                 poblacion = NULL) {
  if (desviacion < 0) {
    error_estadistica("La desviación estándar no puede ser negativa")
  }
  if (error <= 0) {
    error_estadistica("El margen de error debe ser mayor que cero")
  }

  n <- (valor_z(confianza) * desviacion / error)^2
  if (!is.null(poblacion)) {
    n <- correccion_finita(n, poblacion)
  }
  max(1, ceiling(n))
}

#' Tamaño de muestra para estimar una proporción
#'
#' Fórmula: n = z² · p(1 - p) / E², con corrección para población finita
#' si se indica la población.
#'
#' @param error Margen de error E aceptado, como proporción (0.05 = 5 %).
#' @param confianza Nivel de confianza entre 0 y 1.
#' @param proporcion Proporción esperada p; 0.5 da el tamaño más grande y
#'   se usa cuando no se conoce.
#' @param poblacion Tamaño N de la población; `NULL` la trata como infinita.
#' @return Tamaño de muestra redondeado hacia arriba.
#' @section Errores: lanza `error_estadistica` si el error o la proporción
#'   no están entre 0 y 1, o la confianza no está entre 0 y 1.
#' @export
tamano_muestra_proporcion <- function(error, confianza = 0.95, proporcion = 0.5,
                                      poblacion = NULL) {
  if (!(error > 0 && error < 1)) {
    error_estadistica("El margen de error debe estar entre 0 y 1, por ejemplo 0.05")
  }
  if (!(proporcion >= 0 && proporcion <= 1)) {
    error_estadistica("La proporción esperada debe estar entre 0 y 1")
  }

  z <- valor_z(confianza)
  n <- z^2 * proporcion * (1 - proporcion) / error^2
  if (!is.null(poblacion)) {
    n <- correccion_finita(n, poblacion)
  }
  max(1, ceiling(n))
}
