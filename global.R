# packages ----------------------------------------------------------------
suppressWarnings({
  suppressPackageStartupMessages({
    library(shiny)
    library(tidyverse)
    library(bslib)
    library(bsicons)

    library(scales)
    library(DT)
    library(leaflet)
    library(highcharter)
    library(sf)
    library(cli)

    # reporte
    library(showtext)
    library(ggfittext)
  })
})

source("R/parametros_opciones.R")
source("R/funciones_helpers.R")

# data --------------------------------------------------------------------
# mail: Código lectura gdb
st_layers(dsn = "sagir.gdb")
st_layers(dsn = "sagir_old.gdb/")

data <- st_read(dsn = "sagir.gdb", layer = "Iniciativas", as_tibble = TRUE, quiet = TRUE)
data <- data |>
  janitor::clean_names() |>
  # mutate(ano_de_iniciativa = ifelse(ano_de_iniciativa == 0, "-", ano_de_iniciativa)) |>
  mutate(across(where(is.character), ~as.character(forcats::fct_na_value_to_level(.x, "-")))) |>
  mutate(across(where(is.character), ~ifelse(.x == "", "-", .x))) |>
  mutate(across(where(is.character), str_to_title)) |>
  mutate(across(where(is.character), ~stringi::stri_trans_general(.x,  id = "Latin-ASCII")))

# data |>
#   glimpse()

data <- data |>
  rename(
    # global_id,
    codigo = codigo_1,
    # ano_de_iniciativa,
    # fase,
    # etapa,
    # comuna,
    # provincia,
    # area_dentro_del_eje,
    # eje_programa_de_gobierno,
    # tipologia_dentro_del_eje,
    # magnitud,
    # unidad,
    # costo_total,
    # nombre,
    # sector,
    # sub_sector
  ) |>
  mutate(
    uno = 1,
    costo_total_millones = round(costo_total/1e6),
    costo_total_miles_millones = round(costo_total/1e9)
    )

data$Shape              <- NULL
attr(data, "sf_column") <- NULL
attr(data, "agr")       <- NULL

# intercomunales <- st_read(dsn = "sagir_old.gdb",
#                           layer = "Intercomunales",
#                           as_tibble = TRUE,
#                           quiet = TRUE)
# intercomunales <- janitor::clean_names(intercomunales)
# intercomunales_aux <- intercomunales |>
#   count(codigo, sort = TRUE) |>
#   mutate(intercomunal = TRUE) |>
#   select(-n)
#
# data <- left_join(data, intercomunales_aux, by = join_by(codigo))

dpuntos <- st_read(dsn = "sagir.gdb", layer = "Iniciativas", as_tibble = TRUE, quiet = TRUE)
dpuntos <- dpuntos |>
  st_zm() |>
  st_transform(4326) |>
  st_cast("POINT") |>
  janitor::clean_names() |>
  select(codigo = codigo_1) |>
  mutate(
    x = st_coordinates(Shape)[,1],
    y = st_coordinates(Shape)[,2]
  ) |>
  st_drop_geometry()

# sidebar -----------------------------------------------------------------
data |> count(eje_programa_de_gobierno)

sidebar_content <- tagList(

  accordion(
    open = FALSE,
    multiple = FALSE,

    # accordion_panel(
    #   "Eje & Área Gobierno",
    #   icon = icon("sitemap"),
    #   selectizeInput(
    #     "eje_programa_de_gobierno",
    #     tags$small("Eje de Programa de Gobierno"),
    #     choices = names(sort(table(data$eje_programa_de_gobierno), decreasing = TRUE)),
    #     multiple = TRUE,
    #     selected = NULL,
    #     options = list(placeholder = "Todos")
    #   ),
    #   selectInput(
    #     "area_dentro_del_eje",
    #     tags$small("Área dentro del Eje"),
    #     choices = names(sort(table(data$area_dentro_del_eje), decreasing = TRUE)),
    #     multiple = TRUE
    #     )
    #   ),
    accordion_panel(
      "Periodo",
      icon = icon("calendar-days"),
      selectizeInput(
        "ano_resolucion",
        tags$small("Año Resolución"),
        choices = rev(sort(unique(data$ano_resolucion))),
        multiple = TRUE,
        selected = NULL,
        options = list(placeholder = "")
      ),
      selectizeInput(
        "ano_sesion",
        tags$small("Año Sesión"),
        choices = rev(sort(unique(data$ano_sesion))),
        multiple = TRUE,
        selected = NULL,
        options = list(placeholder = "")
      ),
      selectizeInput(
        "ano_ingreso",
        tags$small("Año Ingreso"),
        choices = rev(sort(unique(data$ano_ingreso))),
        multiple = TRUE,
        selected = NULL,
        options = list(placeholder = "")
      ),
    ),
    accordion_panel(
      "Etapa y Fase",
      icon = icon("forward"),
      selectizeInput(
        "etapa",
        tags$small("Etapa"),
        choices = rev(sort(unique(data$etapa))),
        multiple = TRUE,
        selected = NULL,
        options = list(placeholder = "")
      ),
      selectizeInput(
        "fase",
        tags$small("Fase"),
        choices = rev(sort(unique(data$fase))),
        multiple = TRUE,
        selected = NULL,
        options = list(placeholder = "")
      )
    ),
    accordion_panel(
      "Área",
      icon = icon("location-dot"),
      selectizeInput(
        "alcance",
        tags$small("Alcance"),
        choices = names(sort(table(data$alcance), decreasing = TRUE)),
        multiple = TRUE,
        selected = NULL,
        options = list(placeholder = "")
      ),
      selectizeInput(
        "area",
        tags$small("Área"),
        choices = names(sort(table(data$area), decreasing = TRUE)),
        multiple = TRUE,
        selected = NULL,
        options = list(placeholder = "")
      ),
      selectizeInput(
        "provincia",
        tags$small("Provincia"),
        choices = names(sort(table(data$provincia), decreasing = TRUE)),
        multiple = TRUE,
        selected = NULL,
        options = list(placeholder = "")
      ),
      selectInput(
        "comuna",
        tags$small("Comuna"),
        choices = names(sort(table(data$comuna), decreasing = TRUE)),
        multiple = TRUE
      )
    ),
    accordion_panel(
      "Otros filtros",
      icon = icon("barcode"),
      # codigo bip
      selectizeInput(
        "codigo",
        tags$small("Cógido BIP"),
        choices = sort(unique(data$codigo)),
        multiple = TRUE,
        selected = NULL,
        options = list(placeholder = "Todos")
      ),
      textInput(
        "nombre",
        tags$small("Nombre"),
        placeholder = "Buscar por nombre"
      )
    )
  ),
  tags$small(uiOutput("iniciativas_seleccionadas"), class = "text-muted"),
  tags$br(),
  actionButton(
    "reset_filtros",
    tags$small("Resetar filtros"),
    class = "btn-sm btn-primary",
    icon = icon("redo")
  ),
  tags$br(),
  downloadButton(
    "generar_reporte",
    tags$small("Generar reporte"),
    class = "btn-sm btn-primary",
    icon = icon("file-pdf")
  ),
  tags$br(),
  downloadButton(
    "descargar_datos",
    tags$small("Descargar iniciativas seleccionadas"),
    class = "btn-sm btn-primary",
    icon = icon("file-excel")
    )


)
