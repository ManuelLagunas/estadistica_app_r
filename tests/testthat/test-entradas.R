# ---------- desde_texto ----------

test_that("desde_texto separa por comas, punto y coma, espacios y saltos de línea", {
  expect_equal(desde_texto("8, 9 7.5\n10"), c("8", "9", "7.5", "10"))
  expect_equal(desde_texto("1;2;\t3"), c("1", "2", "3"))
})

test_that("desde_texto ignora espacios al inicio y separadores al final", {
  expect_equal(desde_texto("  8, 9, 7, "), c("8", "9", "7"))
})

test_that("desde_texto con texto vacío devuelve un vector vacío", {
  expect_length(desde_texto(""), 0)
})

# ---------- leer_csv ----------

test_that("leer_csv lee los archivos de prueba con su tamaño correcto", {
  expect_equal(dim(leer("ejemplo.csv")), c(7, 3))
  expect_equal(dim(leer("iris.csv")), c(150, 5))
  expect_equal(dim(leer("mtcars.csv")), c(32, 12))
})

test_that("leer_csv lee un archivo de una sola columna", {
  expect_equal(dim(leer_csv(archivo_temporal("x\n1\n2\n"))), c(2, 1))
})

test_that("leer_csv reconoce los acentos en Latin-1", {
  tabla <- leer_csv(archivo_temporal("nombre\nMaría\n", "latin1"))
  expect_equal(tabla[1, 1], "María")
})

test_that("leer_csv quita la marca BOM que agrega Excel", {
  bom <- c(as.raw(c(0xEF, 0xBB, 0xBF)), charToRaw("a,b\n1,2\n"))
  expect_equal(names(leer_csv(archivo_temporal(bom))), c("a", "b"))
})

test_that("leer_csv detecta punto y coma, tabulador y barra vertical", {
  for (separador in c(";", "\t", "|")) {
    texto <- paste0("a", separador, "b\n1", separador, "2\n")
    expect_equal(dim(leer_csv(archivo_temporal(texto))), c(1, 2))
  }
})

test_that("leer_csv conserva nombres de columna con espacios y símbolos", {
  tabla <- leer_csv(archivo_temporal("peso (kg),edad\n70,30\n"))
  expect_equal(names(tabla), c("peso (kg)", "edad"))
})

test_that("leer_csv trata las celdas vacías como dato faltante, como pandas", {
  tabla <- leer_csv(archivo_temporal("a,b\nx,1\n,2\n"))
  expect_true(is.na(tabla$a[2]))
})

test_that("leer_csv rechaza un archivo vacío o sin filas", {
  expect_error(leer_csv(archivo_temporal("")), class = "error_estadistica")
  expect_error(leer_csv(archivo_temporal("a,b\n")), "no tiene filas", class = "error_estadistica")
})
