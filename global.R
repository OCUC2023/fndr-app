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

data <- st_read(dsn = "sagir.gdb",
                layer = "IniciativasFNDR_edit",
                as_tibble = TRUE,
                quiet = TRUE)
data <- data |>
  janitor::clean_names() |>
  mutate(ano_de_iniciativa = ifelse(ano_de_iniciativa == 0, "-", ano_de_iniciativa)) |>
  mutate(across(where(is.character), ~as.character(forcats::fct_na_value_to_level(.x, "-")))) |>
  mutate(across(where(is.character), str_to_title)) |>
  mutate(across(where(is.character), ~stringi::stri_trans_general(.x,  id = "Latin-ASCII")))

data <- data |>
  select(
    global_id,
    codigo,
    ano_de_iniciativa,
    fase_oficial,
    comuna_s,
    provincia_s,
    area_dentro_del_eje,
    eje_programa_de_gobierno,
    tipologia_dentro_del_eje,
    magnitud,
    unidad,
    costo_total,
    nombre,
    sector,
    sub_sector
  ) |>
  mutate(uno = 1)

data$Shape              <- NULL
attr(data, "sf_column") <- NULL
attr(data, "agr")       <- NULL

intercomunales <- st_read(dsn = "sagir.gdb",
                          layer = "Intercomunales",
                          as_tibble = TRUE,
                          quiet = TRUE)
intercomunales <- janitor::clean_names(intercomunales)
intercomunales_aux <- intercomunales |>
  count(codigo, sort = TRUE) |>
  mutate(intercomunal = TRUE) |>
  select(-n)

data <- left_join(data, intercomunales_aux, by = join_by(codigo))

dpuntos <- st_read(dsn = "sagir.gdb",
        layer = "IniciativasFNDR_edit",
        as_tibble = TRUE,
        quiet = TRUE) |>
  st_zm() |>
  st_transform(4326) |>
  st_cast("POINT") |>
  janitor::clean_names() |>
  select(codigo) |>
  mutate(
    x = st_coordinates(Shape)[,1],
    y = st_coordinates(Shape)[,2]
  ) |>
  st_drop_geometry()

# sidebar -----------------------------------------------------------------
data |> count(eje_programa_de_gobierno)

sidebar_content <- tagList(

  selectizeInput(
    "eje_programa_de_gobierno",
    tags$small(icon("sitemap"), "Eje de Programa de Gobierno"),
    choices = names(sort(table(data$eje_programa_de_gobierno), decreasing = TRUE)),
    multiple = TRUE,
    selected = NULL,
    options = list(placeholder = "Todos")
    ),
  # conditionalPanel(
  #   condition = "(typeof input.eje_programa_de_gobierno !== 'undefined' && input.eje_programa_de_gobierno.length > 0)",
  #   selectInput(
  #     "area_dentro_eje",
  #     "Área dentro del Eje",
  #     choices = NULL,
  #     multiple = TRUE
  #     )
  #   ),

  # anio iniciativa
  selectizeInput(
    "anios",
    tags$small(icon("clock"), "Año de iniciativa"),
    choices = rev(sort(unique(data$ano_de_iniciativa))),
    multiple = TRUE,
    selected = NULL,
    options = list(placeholder = "Todos")
    ),

  # fase
  selectizeInput(
    "fase",
    tags$small(icon("forward"), "Fase"),
    choices = rev(sort(unique(data$fase_oficial))),
    multiple = TRUE,
    selected = NULL,
    options = list(placeholder = "Todos")
  ),

  selectizeInput(
    "provincia_s",
    tags$small(icon("location-dot"), "Provincia"),
    choices = names(sort(table(data$provincia_s), decreasing = TRUE)),
    multiple = TRUE,
    selected = NULL,
    options = list(placeholder = "Todos")
    ),
  # conditionalPanel(
  #   condition = "(typeof input.provincia_s !== 'undefined' && input.provincia_s.length > 0)",
  #   selectInput(
  #     "comuna_s",
  #     "Comuna",
  #     choices = NULL,
  #     multiple = TRUE
  #     )
  #   ),

  # codigo bip
  selectizeInput(
    "codigo",
    tags$small(icon("barcode"), "Cógido BIP"),
    choices = sort(unique(data$codigo)),
    multiple = TRUE,
    selected = NULL,
    options = list(placeholder = "Todos")
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
    class = "btn-sm btn-secondary",
    icon = icon("download")
    )
  )

# sidebar_content <- tagList(
#   accordion(
#     multiple = FALSE,
#     accordion_panel(
#       "Sector & Subsector",
#       icon  = icon("list-check"),
#       selectizeInput(
#         "sector",
#         "Sector",
#         choices = names(sort(table(data$sector), decreasing = TRUE)),
#         multiple = TRUE,
#         selected = NULL,
#         options = list(placeholder = "Todos")
#       ),
#       conditionalPanel(
#         condition = "(typeof input.sector !== 'undefined' && input.sector.length > 0)",
#         selectInput(
#           "subsector",
#           "Subsector",
#           choices = NULL,
#           multiple = TRUE
#         )
#       )
#     ),
#     accordion_panel(
#       "Eje & Área Gobierno",
#       icon = icon("sitemap"),
#       selectizeInput(
#         "eje_programa_de_gobierno",
#         "Eje de Programa de Gobierno",
#         choices = names(sort(table(data$eje_programa_de_gobierno), decreasing = TRUE)),
#         multiple = TRUE,
#         selected = NULL,
#         options = list(placeholder = "Todos")
#       ),
#       conditionalPanel(
#         condition = "(typeof input.eje_programa_de_gobierno !== 'undefined' && input.eje_programa_de_gobierno.length > 0)",
#         selectInput(
#           "area_dentro_eje",
#           "Área dentro del Eje",
#           choices = NULL,
#           multiple = TRUE
#         )
#       ),
#     ),
#     accordion_panel(
#       "Provinca & Comuna",
#       icon = icon("location-dot"),
#       selectizeInput(
#         "provincia_s",
#         "Provincia",
#         choices = names(sort(table(data$provincia_s), decreasing = TRUE)),
#         multiple = TRUE,
#         selected = NULL,
#         options = list(placeholder = "Todos")
#       ),
#       conditionalPanel(
#         condition = "(typeof input.provincia_s !== 'undefined' && input.provincia_s.length > 0)",
#         selectInput(
#           "comuna_s",
#           "Comuna",
#           choices = NULL,
#           multiple = TRUE
#         )
#       )
#     ),
#     accordion_panel(
#       "Año Ingreso & Periodo",
#       icon = icon("clock"),
#       sliderInput(
#         "anios",
#         "Año de ingreso",
#         min = min(data$ano_de_ingreso),
#         max = max(data$ano_de_ingreso),
#         value = c(min(data$ano_de_ingreso), max(data$ano_de_ingreso)),
#         sep = "",
#         ticks = FALSE
#       ),
#       selectInput(
#         "d",
#         "Periodo administrativo",
#         choices = c("Periodo adm. 1", "Periodo adm. 2", "Periodo adm. 3"),
#         multiple = TRUE
#       )
#     ),
#     accordion_panel(
#       "Código BIP",
#       icon = icon("barcode"),
#       selectizeInput(
#         "codigo",
#         "Cógido BIP",
#         choices = sort(unique(data$codigo)),
#         multiple = TRUE,
#         selected = NULL,
#         options = list(placeholder = "Todos")
#       )
#     )
#   ),
#
#   tags$small(uiOutput("aplicar_filtros")),
#
#   tags$br(),
#
#   actionButton(
#     "reset_filtros",
#     tags$small("Resetar filtros"),
#     class = "btn-sm btn-primary",
#     icon = icon("redo")
#   ),
#
#   tags$br(),
#
#   downloadButton(
#     "generar_reporte",
#     tags$small("Generar reporte"),
#     class = "btn-sm btn-secondary",
#     icon = icon("download")
#   ),
#
#   # actionButton(
#   #   "aplicar_filtros",
#   #   uiOutput("aplicar_filtros"),
#   #   class = "btn-primary",
#   #   # icon = icon("filter")
#   # )
# )