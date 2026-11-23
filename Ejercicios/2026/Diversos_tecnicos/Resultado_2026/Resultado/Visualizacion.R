library(arrow)
library(dplyr)
#############################################
# Carpeta donde están los feather
ruta <- "D:/Limites maximos de retencion/2026/Output/Simulaciones"
#####################################################
# Lista de archivos
archivos <- list.files(
  path = ruta,
  pattern = "\\.feather$",
  full.names = TRUE
)
#############################################3
# Leer y concatenar todos
df <- archivos |>
  lapply(read_feather) |>
  bind_rows()
