page_navbar(
  theme = fndr_theme,
  lang = "es",
  title = tags$span(
    class = "ttl",
    tags$img(src = "logo.png", width = "46px", height = "auto", class = "me-3"),
    "FNDR-App"
  ),
  # fillable = TRUE,
  sidebar = sidebar(width = 300, sidebar_content),
  nav_panel(
    title = "Resumen",
    icon  = icon("chart-line"),
    layout_column_wrap(
      width = 1/4,
      fillable = TRUE,
      # height = 100,
      value_box(
        title = "Proyectos",
        value = tags$h1(textOutput("home_proyectos_value")),
        showcase = bs_icon("building")
      ),
      value_box(
        title = "Etapa",
        value = tags$h1(textOutput("home_etapa")),
        showcase = highchartOutput("home_etapa_chart"),
        showcase_layout = showcase_left_center(max_height = "200px"),
        full_screen = TRUE
      ),
    ),

    layout_column_wrap(
      1/2,
      height = "45%",
      card(highchartOutput("home_chart_proy_sector")),
      card(highchartOutput("home_chart_proy_eje")),
      ),
    layout_column_wrap(
      1/2,
      height = "45%",
      card(highchartOutput("home_chart_proy_prov")),
      card(highchartOutput("home_chart_etapa_anio")),
      )
  ),
  nav_panel(
    title = "Geografía",
    icon  = icon("map-location-dot"),
    leafletOutput("mapa_main")
  ),
  nav_panel(
    title = "Listado proyectos",
    icon  = icon("table"),
    DT::dataTableOutput("tabla_main")
  )
)