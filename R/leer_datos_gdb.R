# paquetes ----
library(sf)
library(tidyverse)

# revisar contenido de una gdb ----
## (las is.na(geometry_type) corrsponden a tablas sin geometría)
## las que tienen una geometría válida son capas (equivalente a un shapefile)

st_layers("SAGIR.gdb")



# leer datos desde gdb ----
## dsn = define el nombre de la gdb
## layer = define el nombre de la capa o tabla en la gdb a ser importada
iniciativas_fndr <- st_read(dsn = "SAGIR.gdb", layer = "IniciativasFNDR", as_tibble = TRUE)

class(iniciativas_fndr)

## al importar una tabla R advierte que no existe una geometria
intercomunales <- st_read(dsn = "SAGIR.gdb", layer = "Intercomunales", as_tibble = TRUE)
class(intercomunales)








