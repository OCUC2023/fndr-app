# packages ----------------------------------------------------------------
library(shiny)
library(bslib)
library(bsicons)
library(tidyverse)
library(scales)
library(DT)
library(leaflet)
library(highcharter)
library(sf)

# reporte
library(showtext)
library(ggfittext)

# data --------------------------------------------------------------------
data <- st_read(dsn = "sagir.gdb", layer = "IniciativasFNDR", as_tibble = TRUE)
data <- data |>
  janitor::clean_names() |>
  mutate(across(is.character, ~as.character(forcats::fct_na_value_to_level(.x, "vacío/no indicado")))) |>
  mutate(across(is.character, str_to_title)) |>
  mutate(across(is.character, ~stringi::stri_trans_general(.x,  id = "Latin-ASCII")))

intercomunales <- st_read(dsn = "sagir.gdb", layer = "Intercomunales", as_tibble = TRUE)
intercomunales <- janitor::clean_names(intercomunales)
intercomunales_aux <- intercomunales |>
  count(codigo, sort = TRUE) |>
  mutate(intercomunal = TRUE) |>
  select(-n)

data <- left_join(data, intercomunales_aux, by = join_by(codigo))

# parámetros y opciones ---------------------------------------------------
# https://framework.digital.gob.cl/typography.html
# https://framework.digital.gob.cl/colors.html
fndr_pars <- list(
  primary   = "#006FB3",
  secondary = "#FE6565",
  info      = "#F0F0F0",
  font      = "Roboto",
  font_head = "Roboto Slab",
  font_sys  = '-apple-system, BlinkMacSystemFont, "Segoe UI", Roboto, Helvetica, Arial, sans-serif, "Apple Color Emoji", "Segoe UI Emoji", "Segoe UI Symbol"'
)

fndr_theme <- bs_theme(
  base_font = font_google(fndr_pars$font),
  heading_font = font_google(fndr_pars$font_head),
  primary   = fndr_pars$primary,
  secondary = fndr_pars$secondary,
  info      = fndr_pars$info ,
  "navbar-bg" = fndr_pars$primary
) |>
  bs_add_rules(sass::sass_file("www/custom.scss"))

hcopts <- getOption("highcharter.chart")
hcopts$exporting <- list(
  enabled = TRUE,
  buttons = list(
    contextButton = list(
      symbolStrokeWidth = 1,
      symbolFill =  '#C0C0C0',
      symbolStroke = '#C0C0C0'
      )
    )
  )

newlang_opts <- getOption("highcharter.lang")
newlang_opts$weekdays <- c("domingo", "lunes", "martes", "miércoles", "jueves", "viernes", "sábado")
newlang_opts$months <- c("enero", "febrero", "marzo", "abril", "mayo", "junio", "julio",
                         "agosto", "septiembre", "octubre", "noviembre", "diciembre")
newlang_opts$shortMonths <- c("ene", "feb", "mar", "abr", "may", "jun", "jul", "ago", "sep",
                              "oct", "nov", "dic")
newlang_opts$drillUpText  <- "◁ Volver a {series.name}"
newlang_opts$loading      <- "Cargando información"
newlang_opts$downloadCSV  <- "Descargar CSV"
newlang_opts$downloadJPEG <- "Descargar JPEG"
newlang_opts$downloadPDF  <- "Descargar PDF"
newlang_opts$downloadPNG  <- "Descargar PNG"
newlang_opts$downloadSVG  <- "Descargar SVG"
newlang_opts$downloadXLS  <- "Descargar XLS"
newlang_opts$printChart   <- "Imprimir gráfico"
newlang_opts$viewFullscreen <- "Ver pantalla completa"
newlang_opts$resetZoom    <- "Resetear zoom"
newlang_opts$thousandsSep <- "."
newlang_opts$decimalPoint <- ","

options(
  highcharter.lang = newlang_opts,
  highcharter.theme = hc_theme(
    colors = c(fndr_pars$primary, fndr_pars$secondary, hc_theme_smpl()$colors[c(3:6)]),
    # colors = "#0C0C0C",
    chart = list(style = list(fontFamily = fndr_pars$font_sys)),
    title = list(style = list(fontFamily = fndr_pars$font_sys)),
    subtitle = list(style = list(fontFamily = fndr_pars$font_sys)),
    yAxis = list(endOnTick = FALSE)
    ),
  highcharter.chart = hcopts
  )

# highcharts_demo()

# funciones custom --------------------------------------------------------
# value_box_main <- function(value, description){
#   value_box(
#     title = NULL,
#     value = value,
#     description
#   )
# }

value_box <- partial(bslib::value_box, theme_color = "light")

valor_tipologia_mag_uni <- function(data, eje){
  data |>
    as_data_frame() |>
    filter(tipologia_dentro_del_eje == eje) |>
    select(magnitud, unidad) |>
    summarise(
      magnitud = round(sum(magnitud, na.rm = TRUE), 2),
      unidad   = unique(unidad)
    ) |>
    # remover texto grande si es número
    mutate(
      magnitud = fmt_coma(magnitud),
      unidad = ifelse(str_detect(unidad, "N°"), "", unidad)
      ) |>
    str_glue_data("{magnitud} {unidad}") |>
    str_trim() |>
    tags$h4()
}

value_box_tipologia <- function(data, eje){
  value_box(
    title = NULL,
    value = tags$h3(valor_tipologia_mag_uni(data, eje)),
    eje
  )
}

# custom nav_panel para agregar clase `ttl` al titulo
nav_panel <- function (title, ..., value = title, icon = NULL) {
  bslib:::tabPanel_(
    tags$span(title, class = "ttl"),
    ...,
    value = value,
    icon = icon
    )
}

card <- partial(bslib::card, full_screen = TRUE)

fmt_coma <- purrr::partial(scales::comma, big.mark = ".", decimal.mark = ",")

get_ddd <- function(data, var1 = "provincia_s", var2 = "comuna_s", var2sum = "uno"){
  ddd <- data |>
    group_by(v1 = .data[[var1]], v2 = .data[[var2]]) |>
    summarise(
      value = sum(.data[[var2sum]]),
      .groups = "drop"
    )
  ddd
}

hc_ddd <- function(ddd, name = "", ...){

  ddd1 <- ddd |>
    group_by(v1) |>
    summarise(value = sum(value)) |>
    arrange(desc(value)) |>
    mutate(v1 = fct_inorder(v1)) |>
    ungroup()

  ddd2 <- ddd |>
    ungroup() |>
    arrange(desc(value)) |>
    group_nest(v1) |>
    mutate(
      id = v1,
      type = "column",
      name = v1,
      color = fndr_pars$secondary,
      data = map(data, mutate, name = v2, y = value),
      data = map(data, list_parse)
    )

  hchart(
    ddd1,
    type = "column",
    name = name,
    color = fndr_pars$primary,
    hcaes(x = v1, y = value, drilldown = v1)
  ) |>
    hc_drilldown(
      allowPointDrilldown = TRUE,
      series = list_parse(ddd2),
      activeAxisLabelStyle = list(
        textDecoration = 'none',
        fontStyle = 'normal',
        color = 'gray'
      )
    ) |>
    hc_xAxis(title = list(text = "")) |>
    hc_yAxis(title = list(text = ""))

}

# hc_ddd(ddd, name = "Provincia")

# sidebar -----------------------------------------------------------------
data |> count(eje_programa_de_gobierno)

sidebar_content <- tagList(
  # bip
  selectizeInput(
    "codigo",
    tags$small(icon("barcode"), "Cógido BIP"),
    choices = sort(unique(data$codigo)),
    multiple = TRUE,
    selected = NULL,
    options = list(placeholder = "Todos")
  ),

  selectizeInput(
    "anios",
    tags$small( icon("clock"), "Año de ingreso"),
    choices = rev(sort(unique(data$ano_de_ingreso))),
    multiple = TRUE,
    selected = NULL,
    options = list(placeholder = "Todos")
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