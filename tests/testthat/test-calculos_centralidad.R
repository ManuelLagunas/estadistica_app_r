test_that("media y mediana", {
  expect_equal(media(D), 5)
  expect_equal(mediana(D), 4.5)
})

test_that("moda: una, varias o ninguna", {
  expect_equal(moda(D), 4)
  expect_equal(moda(c(1, 1, 2, 2, 3)), c(1, 2))
  expect_length(moda(c(1, 2, 3)), 0)
})

test_that("media armónica", {
  expect_equal(media_armonica(c(60, 40)), 48)
  expect_error(media_armonica(c(5, 0, 3)), class = "error_estadistica")
})

test_that("media geométrica, también con muchos datos grandes", {
  expect_equal(media_geometrica(c(2, 8)), 4)
  # El producto de 200 valores de 100 desbordaría; por logaritmos no.
  expect_equal(media_geometrica(rep(100, 200)), 100)
  expect_error(media_geometrica(c(-1, 2)), class = "error_estadistica")
})

test_that("para datos positivos, armónica <= geométrica <= aritmética", {
  datos <- c(3, 7, 1.5, 12, 8)
  expect_lte(media_armonica(datos), media_geometrica(datos))
  expect_lte(media_geometrica(datos), media(datos))
})
