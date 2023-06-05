# setup -------------------------------------------------------------------
library(shiny)
library(bslib)
library(bsicons)
library(tidyverse)
library(scales)
library(DT)
library(leaflet)
library(histoslider)
library(highcharter)

# data --------------------------------------------------------------------
# este proceso debiera esta aparte, la data
# debería venir ya procesada.
suppressWarnings({
  # data <- readxl::read_excel("data/base_completa_abril_2023_COMPILADO.xlsx") |>
  #   janitor::clean_names() |>
  #   mutate(across(is.character, ~as.character(forcats::fct_na_value_to_level(.x, "vacío/no indicado")))) |>
  #   mutate(across(is.character, str_to_title)) |>
  #   mutate(across(is.character, ~stringi::stri_trans_general(.x,  id = "Latin-ASCII"))) |>
  #
  #   # corregir
  #   filter(ano_de_ingreso > 0) |>
  #   filter(!is.na(codigo)) |>
  #   distinct(codigo, .keep_all = TRUE) |>
  #
  #   # para contar en lugar de sumar
  #   mutate(uno = 1) |>
  #
  #   filter(TRUE)
  #
  # saveRDS(data, "data/data.rds")
  data <- readRDS("data/data.rds")
})

# parámetros y opciones ---------------------------------------------------
# https://framework.digital.gob.cl/typography.html
# https://framework.digital.gob.cl/colors.html
fndr_pars <- list(
  primary   = "#006FB3",
  secondary = "#FE6565",
  info      = "#F0F0F0",
  font      = "Roboto",
  font_head = "Roboto Slab"
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

options(
  highcharter.theme = hc_theme_smpl(
    colors = c(fndr_pars$primary, fndr_pars$secondary, hc_theme_smpl()$colors[c(3:6)]),
    yAxis = list(endOnTick = FALSE)
    )
  )

# funciones custom --------------------------------------------------------
# custom nav_panel para agregar clase `ttl` al titulo
nav_panel <- function (title, ..., value = title, icon = NULL) {
  bslib:::tabPanel_(
    tags$span(title, class = "ttl"),
    ...,
    value = value,
    icon = icon
    )
}

value_box <- partial(bslib::value_box, theme_color = "light")

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

hc_ddd <- function(ddd){

  ddd1 <- ddd |>
    group_by(v1) |>
    summarise(value = sum(value)) |>
    arrange(desc(value)) |>
    mutate(v1 = fct_inorder(v1)) |>
    ungroup()

  ddd2 <- ddd |>
    ungroup() |>
    group_nest(v1) |>
    mutate(
      id = v1,
      type = "column",
      name = v1,
      data = map(data, mutate, name = v2, y = value),
      data = map(data, list_parse)
    )

  hchart(
    ddd1,
    type = "column",
    hcaes(x = v1, y = value, drilldown = v1)
  ) |>
    hc_drilldown(
      allowPointDrilldown = TRUE,
      series = list_parse(ddd2)
    ) |>
    hc_xAxis(title = list(text = NULL)) |>
    hc_yAxis(title = list(text = NULL))

}


# inputs (dependen de data) -----------------------------------------------
sidebar_content <- tagList(
  accordion(
    multiple = FALSE,
    accordion_panel(
      "Sector & Subsector",
      icon  = icon("list-check"),
      selectizeInput(
        "sector",
        "Sector",
        choices = names(sort(table(data$sector), decreasing = TRUE)),
        multiple = TRUE,
        selected = NULL,
        options = list(placeholder = "Todos")
      ),
      conditionalPanel(
        condition = "(typeof input.sector !== 'undefined' && input.sector.length > 0)",
        selectInput(
          "subsector",
          "Subsector",
          choices = NULL,
          multiple = TRUE
        )
      )
    ),
    accordion_panel(
      "Eje & Área Gobierno",
      icon = icon("sitemap"),
      selectizeInput(
        "eje_programa_de_gobierno",
        "Eje de Programa de Gobierno",
        choices = names(sort(table(data$eje_programa_de_gobierno), decreasing = TRUE)),
        multiple = TRUE,
        selected = NULL,
        options = list(placeholder = "Todos")
      ),
      conditionalPanel(
        condition = "(typeof input.eje_programa_de_gobierno !== 'undefined' && input.eje_programa_de_gobierno.length > 0)",
        selectInput(
          "area_dentro_eje",
          "Área dentro del Eje",
          choices = NULL,
          multiple = TRUE
        )
      ),
    ),
    accordion_panel(
      "Provinca & Comuna",
      icon = icon("location-dot"),
      selectizeInput(
        "provincia_s",
        "Provincia",
        choices = names(sort(table(data$provincia_s), decreasing = TRUE)),
        multiple = TRUE,
        selected = NULL,
        options = list(placeholder = "Todos")
      ),
      conditionalPanel(
        condition = "(typeof input.provincia_s !== 'undefined' && input.provincia_s.length > 0)",
        selectInput(
          "comuna_s",
          "Comuna",
          choices = NULL,
          multiple = TRUE
        )
      )
    ),
    accordion_panel(
      "Año Ingreso & Estado",
      icon = icon("clock"),
      sliderInput(
        "anios",
        "Año de ingreso",
        min = min(data$ano_de_ingreso),
        max = max(data$ano_de_ingreso),
        value = c(min(data$ano_de_ingreso), max(data$ano_de_ingreso)),
        sep = "",
        ticks = FALSE
      )
    ),
    accordion_panel(
      "Código BIP",
      icon = icon("barcode"),
      selectizeInput(
        "codigo",
        "Cógido BIP",
        choices = sort(unique(data$codigo)),
        multiple = TRUE,
        selected = NULL,
        options = list(placeholder = "Todos")
      )
    )
  ),

  tags$small(uiOutput("aplicar_filtros")),

  tags$br(),

  actionButton(
    "reset_filtros",
    tags$small("Resetar filtros"),
    class = "btn-sm btn-primary",
    icon = icon("redo")
  ),

  tags$br(),

  # actionButton(
  #   "aplicar_filtros",
  #   uiOutput("aplicar_filtros"),
  #   class = "btn-primary",
  #   # icon = icon("filter")
  # )



)