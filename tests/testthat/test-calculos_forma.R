test_that("asimetría muestral y poblacional", {
  expect_equal(asimetria(D), 0.8185, tolerance = 1e-4)
  expect_equal(asimetria(D, muestral = FALSE), 0.65625)
  expect_error(asimetria(c(5, 5, 5)), class = "error_estadistica")
  expect_error(asimetria(c(1, 2)), "al menos 3", class = "error_estadistica")
})

test_that("curtosis en exceso muestral y poblacional", {
  expect_equal(curtosis(D), 0.940625)
  expect_equal(curtosis(D, muestral = FALSE), -0.21875)
  expect_error(curtosis(c(1, 2, 3)), class = "error_estadistica")
})

test_that("asimetría y curtosis coinciden con pandas en iris", {
  # Valores de Series.skew() y Series.kurt() de pandas.
  longitud <- leer("iris.csv")$sepal.length
  expect_equal(asimetria(longitud), 0.314910956637, tolerance = 1e-10)
  expect_equal(curtosis(longitud), -0.552064041316, tolerance = 1e-10)
})

test_that("interpretación de la asimetría y la curtosis", {
  expect_match(interpretar_asimetria(0.8), "derecha")
  expect_match(interpretar_asimetria(-0.8), "izquierda")
  expect_equal(interpretar_asimetria(0.2), "Aproximadamente simétrica")
  expect_match(interpretar_curtosis(0.94), "Leptocúrtica")
  expect_match(interpretar_curtosis(-0.22), "Mesocúrtica")
  expect_match(interpretar_curtosis(-1), "Platicúrtica")
})
