# Quarto ejecuta R desde docs/. Este archivo activa el entorno renv del
# proyecto, que está en la carpeta de arriba, para usar sus paquetes.
local({
  carpeta <- setwd("..")
  on.exit(setwd(carpeta))
  source("renv/activate.R")
})
