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
# st_layers(dsn = "sagir.gdb")
# st_layers(dsn = "sagir_old.gdb/")
# data <- st_read(dsn = "sagir.gdb", layer = "Iniciativas", as_tibble = TRUE, quiet = TRUE)
data <- st_read(
  dsn = "https://geo.gobiernosantiago.cl/server/rest/services/Iniciativas/Iniciativas/FeatureServer/0/query?where=1%3D1&outFields=*&f=pjson",
  as_tibble = TRUE
  )
data <- data |>
  janitor::clean_names() |>
  st_drop_geometry() |>
  # mutate(ano_de_iniciativa = ifelse(ano_de_iniciativa == 0, "-", ano_de_iniciativa)) |>
  mutate(across(where(is.character), ~as.character(forcats::fct_na_value_to_level(.x, "-")))) |>
  mutate(across(where(is.character), ~ifelse(.x == "", "-", .x))) |>
  # mutate(across(where(is.character), str_to_title)) |>
  # mutate(across(where(is.character), ~stringi::stri_trans_general(.x,  id = "Latin-ASCII"))) |>
  filter(TRUE)

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
attr(data, "agr")       <- NULL
# attr(data, "sf_column") <- NULL


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

# dpuntos <- st_read(dsn = "sagir.gdb", layer = "Iniciativas", as_tibble = TRUE, quiet = TRUE)

dpuntos <- st_read(
  dsn = "https://geo.gobiernosantiago.cl/server/rest/services/Iniciativas/Iniciativas/FeatureServer/0/query?where=1%3D1&f=pjson",
  as_tibble = TRUE
  )
dpuntos <- dpuntos |>
  st_zm() |>
  st_transform(4326) |>
  st_cast("POINT") |>
  janitor::clean_names() |>
  select(codigo = codigo_1) |>
  mutate(
    x = st_coordinates(geometry)[,1],
    y = st_coordinates(geometry)[,2]
  ) |>
  st_drop_geometry()

# sidebar -----------------------------------------------------------------
data |> count(eje_programa_de_gobierno)

info_circle <- icon("info-circle", style= "opacity: 0.25;")

sidebar_content <- tagList(

  accordion(
    open = FALSE,
    multiple = FALSE,

    accordion_panel(
      "Eje & Área Gobierno",
      icon = icon("sitemap"),
      selectizeInput(
        "eje_programa_de_gobierno",
        tags$small(
          "Eje de Programa de Gobierno",
          tooltip(
            info_circle,
            "Eje de Programa de Gobierno."
          )
        ),
        choices = names(sort(table(data$eje_programa_de_gobierno), decreasing = TRUE)),
        multiple = TRUE,
        selected = NULL,
        options = list(placeholder = "Todos")
      ),
      selectInput(
        "area_dentro_del_eje",
        tags$small(
          "Área dentro del Eje",
          tooltip(
            info_circle,
            "Área dentro del Eje."
          )
        ),
        choices = names(sort(table(data$area_dentro_del_eje), decreasing = TRUE)),
        multiple = TRUE
        )
      ),
    accordion_panel(
      "Periodo",
      icon = tooltip(icon("calendar-days"), "Filtro de iniciativas según su año de aprobación o ingreso según corresponda."),
      selectizeInput(
        "ano_resolucion",
        tags$small(
          "Año Resolución",
          tooltip(
            info_circle,
            "Año de la resolución que aprueba la iniciativa."
            )
          ),
        choices = rev(sort(unique(data$ano_resolucion))),
        multiple = TRUE,
        selected = NULL,
        options = list(placeholder = "")
      ),
      selectizeInput(
        "ano_sesion",
        tags$small(
          "Año Sesión",
          tooltip(
            info_circle,
            "Año de la sesión CORE que aprobó la iniciativa."
            )
          ),
        choices = rev(sort(unique(data$ano_sesion))),
        multiple = TRUE,
        selected = NULL,
        options = list(placeholder = "")
      ),
      selectizeInput(
        "ano_ingreso",
        tags$small(
          "Año Ingreso",
          tooltip(
            info_circle,
            "Año en que la iniciativa fue ingresada, aplica a iniciativas que no tienen año de resolución o sesión en el sistema."
            )
          ),
        choices = rev(sort(unique(data$ano_ingreso))),
        multiple = TRUE,
        selected = NULL,
        options = list(placeholder = "")
      ),
    ),
    accordion_panel(
      "Etapa y Fase",
      icon = tooltip(icon("forward"), "filtro iniciativas según la etapa de postulación o la fase en que se encuentra."),
      selectizeInput(
        "etapa",
        tags$small(
          "Etapa",
          tooltip(info_circle, "Etapa a la que postulo la iniciativa (diseño o ejecución).")
          ),
        choices = rev(sort(unique(data$etapa))),
        multiple = TRUE,
        selected = NULL,
        options = list(placeholder = "")
      ),
      selectizeInput(
        "fase",
        tags$small(
          "Fase",
          tooltip(info_circle, "Si la iniciativa está aprobada o terminada.")
          ),
        choices = rev(sort(unique(data$fase))),
        multiple = TRUE,
        selected = NULL,
        options = list(placeholder = "")
      )
    ),
    accordion_panel(
      "Área",
      icon = tooltip(icon("location-dot"), "Filtrar iniciativas según su área geográfica."),
      selectizeInput(
        "alcance",
        tags$small(
          "Alcance",
          tooltip(info_circle, "Comunal, intercomunal o regional según donde se ejecutará o afectará la iniciativa.")
          ),
        choices = names(sort(table(data$alcance), decreasing = TRUE)),
        multiple = TRUE,
        selected = NULL,
        options = list(placeholder = "")
      ),
      selectizeInput(
        "area",
        tags$small(
          "Área",
          tooltip(
            info_circle,
            "Rural o Urbana (el valor “-“ representa iniciativas intercomunales o regionales que no pueden ser definidas como urbanas o rurales)"
            )
          ),
        choices = names(sort(table(data$area), decreasing = TRUE)),
        multiple = TRUE,
        selected = NULL,
        options = list(placeholder = "")
      ),
      selectizeInput(
        "provincia",
        tags$small(
          "Provincia",
          tooltip(info_circle, "Provincias de la región (el valor “-“ representa iniciativas intercomunales o regionales que no pueden ser definidas en una sola provincia).")
          ),
        choices = names(sort(table(data$provincia), decreasing = TRUE)),
        multiple = TRUE,
        selected = NULL,
        options = list(placeholder = "")
      ),
      selectInput(
        "comuna",
        tags$small(
          "Comuna",
          tooltip(
            info_circle,
            "Comunas de la región (el valor “-“ representa iniciativas intercomunales o regionales que no pueden ser definidas en una sola comuna)"
            )
          ),
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
        tags$small(
          "Cógido BIP",
          tooltip(
            info_circle,
            "Filtrar por el código BIP de la iniciativa."
            )
          ),
        choices = sort(unique(data$codigo)),
        multiple = TRUE,
        selected = NULL,
        options = list(placeholder = "Todos")
      ),
      textInput(
        "nombre",
        tags$small(
          "Palabra clave",
          tooltip(
            info_circle,
            "Filtrar utilizando una palabra clave que se encuentre en el nombre de la iniciativa."
            )
          ),
        placeholder = "Buscar por nombre"
      )
    )
  ),
  tags$small(uiOutput("iniciativas_seleccionadas"), class = "text-muted"),
  # tags$br(),
  actionButton(
    "reset_filtros",
    tags$small("Resetar filtros"),
    class = "btn-sm btn-primary",
    icon = icon("redo")
  ),
  # tags$br(),
  downloadButton(
    "generar_reporte",
    tags$small("Generar reporte"),
    class = "btn-sm btn-primary",
    icon = icon("file-pdf")
  ),
  # tags$br(),
  downloadButton(
    "descargar_datos",
    tags$small("Descargar iniciativas seleccionadas"),
    class = "btn-sm btn-primary",
    icon = icon("file-excel")
    )
  # tags$head(tags$script(HTML("$(function () { $('[data-toggle=tooltip]').tooltip() })")))
)
