test_that("rango", {
  expect_equal(rango(D), 7)
})

test_that("varianza muestral y poblacional", {
  expect_equal(varianza(D), 4.5714, tolerance = 1e-4)
  expect_equal(varianza(D, muestral = FALSE), 4)
  expect_error(varianza(7), class = "error_estadistica")
  # Un solo dato: la varianza poblacional es 0 (stats::var daría NA).
  expect_equal(varianza(7, muestral = FALSE), 0)
})

test_that("desviación estándar", {
  expect_equal(desviacion_estandar(D), 2.1381, tolerance = 1e-4)
  expect_equal(desviacion_estandar(D, muestral = FALSE), 2)
})

test_that("rango intercuartílico, sin efecto de los valores extremos", {
  expect_equal(rango_intercuartilico(D), 1.5)
  d_extremo <- c(2, 4, 4, 4, 5, 5, 7, 900)
  expect_equal(rango_intercuartilico(d_extremo), 1.5)
  expect_equal(rango(d_extremo), 898)
})
