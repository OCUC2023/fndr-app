page_navbar(
  theme = fndr_theme,
  lang = "es",
  fillable = TRUE,
  fillable_mobile = TRUE,
  bg = fndr_pars$primary,
  inverse = TRUE,
  title = tags$span(
    class = "ttl",
    tags$img(src = "b.png", width = "150px", height = "auto", class = "me-3"),
  ),
  sidebar = sidebar(width = 300, sidebar_content),
  nav_panel(
    tags$head(
      tags$link(href = "favicon-32x32.png", rel = "icon"),
    ),
    title = "Inicio",
    icon  = icon("dashboard"),
    uiOutput("hero_boxes")
    ),
  nav_panel(
    title = "Gráficos",
    icon = icon("chart-pie"),
    navset_card_tab(
      title = NULL,
      nav_panel(
        title = "Cantidad Iniciativas",
        layout_column_wrap(
         1/2,
         fillable = TRUE,
         fill = TRUE,
         card(highchartOutput("chart_proy_prov_comuna")),
         card(highchartOutput("chart_proy_eje_area")),
         card(highchartOutput("chart_tipologia")),
         card(highchartOutput("chart_tipologia_dentro_eje"))
         )
        ),
      nav_panel(
        title = "Monto (millones $)",
        layout_column_wrap(
          1/2,
          fillable = TRUE,
          fill = TRUE,
          card(highchartOutput("chart_proy_prov_comuna_m")),
          card(highchartOutput("chart_proy_eje_area_m")),
          card(highchartOutput("chart_tipologia_m")),
          card(highchartOutput("chart_tipologia_dentro_eje_m"))
          )
        )
      )
    ),
  nav_panel(
    title = "Listado Iniciativas",
    icon  = icon("table"),
    DT::dataTableOutput("tabla_main")
    ),
  nav_panel(
    title = "Mapa Inciativas",
    icon  = icon("map"),
    leafletOutput("mapa_main")
    )
  )
