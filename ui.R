page_navbar(
  theme = fndr_theme,
  lang = "es",
  fillable = TRUE,
  fillable_mobile = TRUE,
  title = tags$span(
    class = "ttl",
    tags$img(src = "logo.png", width = "46px", height = "auto", class = "me-3"),
    tags$span("FNDR", tags$sup("APP"))
  ),
  sidebar = sidebar(width = 300, sidebar_content),
  nav_panel(
    title = "Inicio",
    icon  = icon("dashboard"),
    layout_column_wrap(
      width = 1/4,
      fillable = TRUE,
      fill = TRUE,
      # data |> count(tipologia_dentro_del_eje, sort = TRUE) |> View()
      value_box(NULL, uiOutput("hero_aceras"), "Aceras"),
      value_box(NULL, uiOutput("hero_pavcal"), "Pavimentacion De Calzadas"),
      value_box(NULL, uiOutput("hero_refptn"), "Refugios Peatonales"),
      value_box(NULL, uiOutput("hero_ciclov"), "Ciclovias"),
      value_box(NULL, uiOutput("hero_lumina"), "Luminarias"),
      # value_box(NULL, uiOutput("hero_bacheo"), "Bacheo De Calzadas"),
      value_box(NULL, uiOutput("hero_alarms"), "Alarmas")
      )
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
    title = "Mapa inciativas",
    icon  = icon("map"),
    leafletOutput("mapa_main")
    )
  )
