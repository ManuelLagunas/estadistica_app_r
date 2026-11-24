test_that("limpiar_serie descarta lo que no es número y cuenta los descartados", {
  r <- limpiar_serie(c("8", "9", "diez", "7", ""))
  expect_equal(r$datos, c(8, 9, 7))
  expect_equal(r$descartados, 2)
})

test_that("limpiar_serie convierte un factor por su valor, no por su código", {
  expect_equal(limpiar_serie(factor(c("10", "20")))$datos, c(10, 20))
})

test_that("limpiar_serie sin ningún número lanza un error de datos", {
  expect_error(limpiar_serie(c("a", "b")), class = "error_estadistica")
})

test_that("limpiar_tabla y filas_completas descartan la fila completa", {
  ejemplo <- leer("ejemplo.csv")[c("calificacion", "edad")]
  r <- limpiar_tabla(ejemplo)
  expect_equal(nrow(r$datos), 6)
  expect_equal(r$descartadas, 1)
  # Python numera desde 0 (0, 1, 2, 4, 5, 6); R desde 1.
  expect_equal(filas_completas(ejemplo), c(1, 2, 3, 5, 6, 7))
})

test_that("limpiar_tabla con una columna de texto lanza un error de datos", {
  iris_csv <- leer("iris.csv")
  expect_error(
    limpiar_tabla(iris_csv[c("sepal.length", "variety")]),
    class = "error_estadistica"
  )
})
