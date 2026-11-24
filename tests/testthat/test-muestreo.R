# Las pruebas de técnicas aleatorias revisan propiedades (tamaño, saltos,
# reproducibilidad), no filas concretas: el generador de R es distinto al
# de numpy, así que con la misma semilla salen filas distintas.

# ---------- Aleatorio simple y sistemático ----------

test_that("aleatorio simple: tamaño y misma muestra con la misma semilla", {
  ir <- leer("iris.csv")
  expect_equal(nrow(muestreo_aleatorio_simple(ir, 30, semilla = 1)), 30)
  expect_identical(
    muestreo_aleatorio_simple(ir, 30, semilla = 1),
    muestreo_aleatorio_simple(ir, 30, semilla = 1)
  )
})

test_that("aleatorio simple con y sin reemplazo", {
  mt <- leer("mtcars.csv")
  expect_equal(nrow(muestreo_aleatorio_simple(mt, 40, TRUE, 1)), 40)
  expect_error(muestreo_aleatorio_simple(mt, 40), class = "error_estadistica")
  expect_error(muestreo_aleatorio_simple(mt, 0), class = "error_estadistica")
})

test_that("la semilla no cambia el generador aleatorio global", {
  set.seed(123)
  esperado <- stats::runif(1)
  set.seed(123)
  muestreo_aleatorio_simple(data.frame(a = 1:10), 3, semilla = 99)
  expect_equal(stats::runif(1), esperado)
})

test_that("sistemático: una fila cada k = N %/% n", {
  filas <- as.integer(rownames(muestreo_sistematico(leer("iris.csv"), 30, 1)))
  expect_length(filas, 30)
  expect_equal(unique(diff(filas)), 5)
  expect_lte(filas[1], 5)
})

# ---------- Estratificado ----------

test_that("asignación entre estratos igual que en Python", {
  mt <- leer("mtcars.csv")
  # Resultados de la versión en Python, ordenados por cyl.
  ordenar <- function(x) x[order(as.numeric(names(x)))]
  expect_equal(ordenar(asignar_estratos(mt, "cyl", 10)), c("4" = 4, "6" = 2, "8" = 4))
  expect_equal(ordenar(asignar_estratos(mt, "cyl", 10, "igual")), c("4" = 3, "6" = 4, "8" = 3))
  expect_equal(
    ordenar(asignar_estratos(mt, "cyl", 10, "neyman", "hp")),
    c("4" = 2, "6" = 2, "8" = 6)
  )
})

test_that("asignación con estratos insuficientes o datos inválidos", {
  mt <- leer("mtcars.csv")
  expect_error(asignar_estratos(mt, "cyl", 24, "igual"), "6 \\(pide", class = "error_estadistica")
  expect_error(asignar_estratos(mt, "cyl", 10, "otra"), class = "error_estadistica")
  expect_error(asignar_estratos(mt, "cyl", 10, "neyman"), class = "error_estadistica")
  expect_error(asignar_estratos(mt, "zz", 10), class = "error_estadistica")
})

test_that("estratificado proporcional en iris toma 10 de cada especie", {
  muestra <- muestreo_estratificado(leer("iris.csv"), "variety", 30, semilla = 1)
  expect_equal(as.vector(table(muestra$variety)), c(10, 10, 10))
})

test_that("media estratificada", {
  ir <- leer("iris.csv")
  expect_equal(
    media_estratificada(ir, ir, "variety", "petal.length"),
    mean(ir$petal.length)
  )
  sin_setosa <- ir[ir$variety != "Setosa", ]
  expect_error(
    media_estratificada(sin_setosa, ir, "variety", "petal.length"),
    "Setosa",
    class = "error_estadistica"
  )
})

# ---------- Conglomerados y cuotas ----------

test_that("conglomerados: toma grupos completos", {
  mt <- leer("mtcars.csv")
  muestra <- muestreo_conglomerados(mt, "gear", 2, semilla = 1)
  for (grupo in unique(muestra$gear)) {
    expect_equal(sum(muestra$gear == grupo), sum(mt$gear == grupo))
  }
  expect_error(muestreo_conglomerados(mt, "gear", 5), class = "error_estadistica")
})

test_that("conglomerados con un solo grupo numérico (trampa de sample)", {
  datos <- data.frame(g = c(5, 5, 5))
  expect_equal(nrow(muestreo_conglomerados(datos, "g", 1, semilla = 1)), 3)
})

test_that("cuotas: las primeras filas de cada grupo, en orden del archivo", {
  ir <- leer("iris.csv")
  muestra <- muestreo_cuotas(ir, "variety", c(Setosa = 3, Virginica = 2))
  # Python: 0, 1, 2, 100, 101 (numeradas desde 0).
  expect_equal(rownames(muestra), c("1", "2", "3", "101", "102"))
  expect_error(muestreo_cuotas(ir, "variety", c(Rosa = 2)), class = "error_estadistica")
  expect_error(muestreo_cuotas(ir, "variety", c(Setosa = 60)), class = "error_estadistica")
})

# ---------- Tamaño de muestra ----------

test_that("tamaño de muestra para una proporción y una media", {
  expect_equal(tamano_muestra_proporcion(0.05), 385)
  expect_equal(tamano_muestra_proporcion(0.05, poblacion = 500), 218)
  expect_equal(tamano_muestra_media(6, 2), 35)
  expect_error(tamano_muestra_media(6, 2, 1.5), class = "error_estadistica")
  expect_error(tamano_muestra_proporcion(1.2), class = "error_estadistica")
})

test_that("corrección para población finita", {
  expect_equal(correccion_finita(100, 1000), 100 / (1 + 99 / 1000))
  expect_error(correccion_finita(100, 0), class = "error_estadistica")
})
