page_navbar(
  theme = fndr_theme,
  lang = "es",
  fillable = TRUE,
  fillable_mobile = TRUE,
  title = tags$span(
    class = "ttl",
    tags$img(src = "logo.png", width = "46px", height = "auto", class = "me-3"),
    "FNDR-App"
  ),
  sidebar = sidebar(width = 300, sidebar_content),
  nav_panel(
    title = "Inicio",
    icon  = icon("dashboard"),
    # layout_column_wrap(
      # width = 1/2,
    layout_columns(
      col_widths = c(5, 7),
      # fillable = TRUE,
      leafletOutput("mapa_main"),
      layout_column_wrap(
        1/3,
        fillable = TRUE,
        fill = TRUE,
        # data |> count(tipologia_dentro_del_eje, sort = TRUE) |> View()
        value_box(NULL, uiOutput("hero_aceras"), "Aceras"),
        value_box(NULL, uiOutput("hero_pavcal"), "Pavimentacion De Calzadas"),
        value_box(NULL, uiOutput("hero_refptn"), "Refugios Peatonales"),
        value_box(NULL, uiOutput("hero_ciclov"), "Ciclovias"),
        value_box(NULL, uiOutput("hero_lumina"), "Luminarias"),
        value_box(NULL, uiOutput("hero_alarms"), "Alarmas")
        # value_box(NULL, uiOutput("hero_bacheo"), "Bacheo De Calzadas"),
        )
      )
    ),
  nav_panel(
   title = "Gráficos",
   icon = icon("chart-pie"),
   layout_column_wrap(
     1/2,
     fillable = TRUE,
     fill = TRUE,
     card(highchartOutput("home_chart_proy_sector")),
     card(highchartOutput("home_chart_proy_eje")),
     card(highchartOutput("home_chart_proy_prov")),
     card(highchartOutput("home_chart_etapa_anio"))
     )
  ),
  nav_panel(
    title = "Iniciativas",
    icon  = icon("table"),
    DT::dataTableOutput("tabla_main")
  )
  # nav_panel(
  #   title = "Acerca de",
  #   icon  = icon("question-circle"),
  #   "No tenemos mucha información..."
  #   # DT::dataTableOutput("tabla_main")
  # )
)
