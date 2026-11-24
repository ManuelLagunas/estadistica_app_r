# Pruebas de las funciones de apoyo de la interfaz y de los módulos de
# Shiny con shiny::testServer, que simula los controles sin abrir un
# navegador.

# ---------- Formato y columnas ----------

test_that("formatear deja a lo más 2 decimales sin ceros sobrantes", {
  expect_equal(formatear(c(4.5, 4, 3.14159, 100)), c("4.5", "4", "3.14", "100"))
})

test_that("columnas de grupo y numéricas", {
  ir <- leer("iris.csv")
  expect_equal(columnas_grupo(ir), "variety")
  expect_equal(columnas_numericas(ir), names(ir)[1:4])
})

# ---------- Histograma y densidad ----------

test_that("los intervalos del histograma coinciden con numpy", {
  # Conteos de np.histogram(bins="auto") con numpy 2.5.
  contar <- function(datos) {
    bordes <- intervalos_auto(datos)
    tabulate(
      findInterval(datos, bordes, rightmost.closed = TRUE, all.inside = TRUE),
      length(bordes) - 1
    )
  }
  ir <- leer("iris.csv")
  mt <- leer("mtcars.csv")
  expect_equal(contar(ir$sepal.length), c(9, 23, 20, 28, 28, 14, 17, 5, 6))
  expect_equal(contar(mt$hp), c(9, 8, 8, 3, 3, 1))
  expect_equal(contar(mt$cyl), c(11, 0, 7, 14))
  expect_equal(contar(c(1, 2, 2, 3, 10)), c(3, 1, 0, 0, 1))
  expect_equal(intervalos_auto(c(5, 5, 5)), c(4.5, 5.5))
})

test_that("la densidad coincide con scipy.stats.gaussian_kde", {
  longitud <- leer("iris.csv")$sepal.length
  expect_equal(
    densidad_scott(longitud, c(4, 5.5, 7)),
    c(0.02745265, 0.38870211, 0.18338094),
    tolerance = 1e-6
  )
})

test_that("las gráficas se construyen en tema claro y oscuro", {
  ir <- leer("iris.csv")
  for (oscuro in c(FALSE, TRUE)) {
    expect_s3_class(grafica_distribucion(ir$sepal.length, oscuro), "plotly")
    expect_s3_class(mapa_calor_correlacion(matriz_correlacion(ir[1:4]), oscuro), "plotly")
    expect_s3_class(
      grafica_dispersion(
        ir$petal.length, ir$petal.width, "petal.length", "petal.width",
        grupos = ir$variety, recta = regresion_lineal(ir$petal.length, ir$petal.width),
        oscuro = oscuro
      ),
      "plotly"
    )
  }
  # Con ceros, las medias armónica y geométrica se omiten sin error.
  expect_s3_class(grafica_distribucion(c(0, 1, 1, 2)), "plotly")
})

# ---------- Módulos de Shiny ----------

test_that("la aplicación se construye", {
  expect_s3_class(ejecutar(), "shiny.appobj")
})

test_that("una variable: calcula al presionar Calcular", {
  shiny::testServer(
    pantalla_una_variable_server,
    args = list(oscuro = shiny::reactive(FALSE)),
    {
      session$setInputs(origen = "Manual", texto = "4, 8, abc; 15 16", tipo = "Muestra")
      session$setInputs(calcular = 1)
      expect_equal(resultados()$datos, c(4, 8, 15, 16))
      expect_equal(resultados()$descartados, 1)
      expect_true(resultados()$muestral)
    }
  )
})

test_that("relación: arma la tabla manual y descarta la fila incompleta", {
  shiny::testServer(
    pantalla_relacion_server,
    args = list(oscuro = shiny::reactive(FALSE)),
    {
      session$setInputs(
        origen = "Manual", nombres = "x, y",
        variable_1 = "1 2 3 4", variable_2 = "2 4 5",
        tipo = "Muestra", metodo = "pearson", variante = "b"
      )
      session$setInputs(calcular = 1)
      expect_equal(nrow(resultados()$tabla), 3)
      expect_equal(resultados()$descartadas, 1)
      expect_equal(correlacion()["x", "y"], correlacion_pearson(1:3, c(2, 4, 5)))
    }
  )
})

test_that("muestreo: toma la muestra y la borra al cambiar de población", {
  ruta_iris <- testthat::test_path("..", "..", "datos_prueba", "iris.csv")
  skip_if_not(file.exists(ruta_iris))
  archivo <- data.frame(name = "iris.csv", datapath = ruta_iris)

  shiny::testServer(pantalla_muestreo_server, {
    session$setInputs(archivo = archivo, tecnica = "Aleatorio simple", n = 12, semilla = 5)
    session$setInputs(tomar = 1)
    expect_equal(nrow(estado()$muestra), 12)
    expect_match(estado()$descripcion, "semilla 5")

    # Otra población: la muestra anterior ya no corresponde.
    otro <- archivo_temporal("a,b\n1,x\n2,y\n3,x\n")
    session$setInputs(archivo = data.frame(name = "otro.csv", datapath = otro))
    expect_null(estado())
  })
})

test_that("muestreo: avisa si las cuotas están en cero", {
  ruta_iris <- testthat::test_path("..", "..", "datos_prueba", "iris.csv")
  skip_if_not(file.exists(ruta_iris))

  shiny::testServer(pantalla_muestreo_server, {
    session$setInputs(
      archivo = data.frame(name = "iris.csv", datapath = ruta_iris),
      tecnica = "Por cuotas (no probabilístico)", cuotas_columna = "variety",
      cuota_1 = 0, cuota_2 = 0, cuota_3 = 0, semilla = NA
    )
    session$setInputs(tomar = 1)
    expect_match(estado()$error, "cuota mayor que cero")
  })
})
