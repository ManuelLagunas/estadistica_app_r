# ---------- Covarianza ----------

test_that("covarianza muestral y poblacional", {
  expect_equal(covarianza(X, Y), 3.6667, tolerance = 1e-4)
  expect_equal(covarianza(X, Y, muestral = FALSE), 2.75)
  expect_error(covarianza(X, c(1, 2)), class = "error_estadistica")
})

test_that("matriz de covarianza igual a stats::cov y con versión poblacional", {
  datos <- leer("mtcars.csv")[c("mpg", "wt", "hp")]
  expect_equal(matriz_covarianza(datos), stats::cov(datos), ignore_attr = TRUE)
  n <- nrow(datos)
  expect_equal(
    matriz_covarianza(datos, muestral = FALSE),
    stats::cov(datos) * (n - 1) / n,
    ignore_attr = TRUE
  )
  expect_error(matriz_covarianza(datos["mpg"]), class = "error_estadistica")
})

# ---------- Coeficientes de correlación ----------

test_that("conteo de pares concordantes, discordantes y empatados", {
  expect_equal(contar_pares(XE, YE), list(C = 4, D = 0, Tx = 1, Ty = 1))
})

test_that("coeficientes sin empates", {
  expect_equal(correlacion_pearson(X, Y), 0.8315, tolerance = 1e-4)
  expect_equal(correlacion_spearman(X, Y), 0.8)
  expect_equal(correlacion_kendall(X, Y), 0.6667, tolerance = 1e-4)
  expect_equal(correlacion_gamma(X, Y), 0.6667, tolerance = 1e-4)
})

test_that("coeficientes con empates: las variantes de Kendall se separan", {
  expect_equal(correlacion_pearson(XE, YE), 0.8528, tolerance = 1e-4)
  expect_equal(correlacion_spearman(XE, YE), 0.8333, tolerance = 1e-4)
  expect_equal(correlacion_kendall(XE, YE, variante = "a"), 0.6667, tolerance = 1e-4)
  expect_equal(correlacion_kendall(XE, YE, variante = "b"), 0.8)
  expect_equal(correlacion_kendall(XE, YE, variante = "c"), 0.75)
  expect_equal(correlacion_gamma(XE, YE), 1)
  expect_equal(correlacion_gamma(c(1, 1, 2, 2), c(1, 2, 1, 2)), 0)
})

test_that("Pearson, Spearman y Kendall b coinciden con stats::cor en mtcars", {
  mt <- leer("mtcars.csv")
  for (metodo in c("pearson", "spearman", "kendall")) {
    funcion <- METODOS_CORRELACION[[metodo]]
    expect_equal(funcion(mt$mpg, mt$cyl), stats::cor(mt$mpg, mt$cyl, method = metodo))
  }
})

test_that("las correlaciones rechazan datos que no se pueden correlacionar", {
  expect_error(correlacion_kendall(X, Y, variante = "d"), class = "error_estadistica")
  expect_error(correlacion_pearson(c(1, 1, 1), c(1, 2, 3)), class = "error_estadistica")
  expect_error(correlacion_pearson(1, 2), class = "error_estadistica")
})

test_that("la interpretación usa los umbrales de cada coeficiente", {
  expect_equal(interpretar_correlacion(0.55), "Correlación positiva moderada")
  expect_equal(interpretar_correlacion(0.55, "kendall"), "Correlación positiva fuerte")
  expect_equal(interpretar_correlacion(-0.85), "Correlación negativa fuerte")
  expect_equal(interpretar_correlacion(0.1), "Correlación débil o nula")
})

# ---------- Matriz de correlación ----------

test_that("matriz de correlación simétrica con 1 en la diagonal", {
  numericas <- leer("iris.csv")[1:4]
  matriz <- matriz_correlacion(numericas, "kendall", "b")
  expect_equal(unname(diag(matriz)), rep(1, 4))
  expect_equal(matriz, t(matriz))
  expect_equal(matriz["sepal.length", "petal.length"], 0.7185, tolerance = 1e-4)
})

test_that("matriz de correlación nombra las columnas que no varían", {
  datos <- data.frame(a = 1:3, b = c(5, 5, 5), c = c(2, 1, 3))
  expect_error(matriz_correlacion(datos), "b", class = "error_estadistica")
})

# ---------- Regresión y contingencia ----------

test_that("regresión lineal de mpg contra wt en mtcars", {
  mt <- leer("mtcars.csv")
  recta <- regresion_lineal(mt$wt, mt$mpg)
  expect_equal(recta$pendiente, -5.3445, tolerance = 1e-4)
  expect_equal(recta$interseccion, 37.2851, tolerance = 1e-4)
  expect_equal(recta$r2, 0.7528, tolerance = 1e-4)
  # Comprobación cruzada con el modelo lineal de R.
  expect_equal(unname(stats::coef(stats::lm(mpg ~ wt, mt))), c(recta$interseccion, recta$pendiente))
})

test_that("tabla de contingencia de cyl y am en mtcars", {
  mt <- leer("mtcars.csv")
  tabla <- tabla_contingencia(mt, "cyl", "am")
  expect_equal(tabla["Total", "Total"], 32)
  expect_equal(tabla["4", "1"], 8)
  expect_equal(tabla_contingencia(mt, "cyl", "am", "fila")["4", "1"], 72.7273, tolerance = 1e-4)
  expect_equal(unname(tabla_contingencia(mt, "cyl", "am", "columna")["Total", ]), c(100, 100, 100))
  expect_equal(tabla_contingencia(mt, "cyl", "am", "total")["Total", "Total"], 100)
})

test_that("tabla de contingencia rechaza variables repetidas o inexistentes", {
  mt <- leer("mtcars.csv")
  expect_error(tabla_contingencia(mt, "cyl", "cyl"), class = "error_estadistica")
  expect_error(tabla_contingencia(mt, "cyl", "zz"), "zz", class = "error_estadistica")
  expect_error(tabla_contingencia(mt, "cyl", "am", "otro"), class = "error_estadistica")
})
