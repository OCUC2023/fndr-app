# source("global.R")
# input <- list(sector = "Multisectorial", anios = c(2018, 2023))

function(input, output, session) {

  # data --------------------------------------------------------------------
  # data prefiltrada para conteo
  data_pre_filtrada <- reactive({
    cli::cli_h2("reactive data_pre_filtrada")

    data_pre_filtrada <- data |>
      filter(sector     %in% if(is.null(input$sector    )) unique(data$sector)     else input$sector     ) |>
      filter(sub_sector %in% if(is.null(input$sub_sector)) unique(data$sub_sector) else input$sub_sector ) |>

      filter(eje_programa_de_gobierno %in% if(is.null(input$eje_programa_de_gobierno)) unique(data$eje_programa_de_gobierno) else input$eje_programa_de_gobierno) |>
      filter(area_dentro_del_eje %in% if(is.null(input$area_dentro_del_eje)) unique(data$area_dentro_del_eje) else input$area_dentro_del_eje) |>

      filter(provincia_s %in% if(is.null(input$provincia_s)) unique(data$provincia_s)     else input$provincia_s) |>
      filter(comuna_s %in% if(is.null(input$comuna_s)) unique(data$comuna_s) else input$comuna_s) |>

      filter(between(ano_de_ingreso, input$anios[1], input$anios[2])) |>
      filter(TRUE)

    cli::cli_inform("{fmt_coma(nrow(data_pre_filtrada))} filas")

    data_pre_filtrada

  })

  # reseta/update filtros
  observe({
    cli::cli_h2("observe reset_filtros")

    updateSelectInput(session, "sector", selected = NA)
    updateSelectInput(session, "sub_sector", selected = NA)

    updateSelectInput(session, "eje_programa_de_gobierno", selected = NA)
    updateSelectInput(session, "area_dentro_eje", selected = NA)

    updateSelectInput(session, "provincia_s", selected = NA)
    updateSelectInput(session, "comuna_s", selected = NA)

    updateSliderInput(session, "anios", value = c(min(data$ano_de_ingreso), max(data$ano_de_ingreso)))

  }) |>
    bindEvent(input$reset_filtros)

  data_filtrada <- reactive({
    cli::cli_h2("reactive data_filtrada")
    data_pre_filtrada <- data_pre_filtrada()
    data_filtrada <- data_pre_filtrada
    data_filtrada
  })

  # textos ------------------------------------------------------------------
  output$aplicar_filtros <- renderUI({
    d <- data_pre_filtrada()
    nr <- nrow(d)
    tagList(
      icon("filter"),
      str_glue("{fmt_coma(nr)} proyectos seleccionados.")
      )
  })

  output$home_proyectos_value <- renderText(fmt_coma(nrow(data_filtrada())))

  output$home_etapa_chart <- renderHighchart({
    # data_filtrada()
    # data |>
    data_filtrada() |>
      count(etapa, sort = TRUE) |>
      mutate(etapa = fct_inorder(etapa)) |>
      hchart("pie", name = "Etapa", hcaes(name = etapa, y = n)) |>
      hc_tooltip(shared = TRUE) |>
      hc_plotOptions(
        pie = list(
          borderWidth = 0,
          allowPointSelect = FALSE,
          cursor = 'pointer',
          dataLabels = list(enabled = FALSE)
        )
      )|>
      hc_add_theme(hc_theme_sparkline_vb())

  })

  # home charts -------------------------------------------------------------
  output$home_chart_proy_sector <- renderHighchart({
    data_filtrada <- data_filtrada()
    data_filtrada |> get_ddd("sector", "sub_sector", "uno") |> hc_ddd()

  })

  output$home_chart_proy_prov <- renderHighchart({
    data_filtrada <- data_filtrada()
    data_filtrada |> get_ddd("provincia_s", "comuna_s", "uno") |> hc_ddd()
  })

  output$home_chart_proy_eje <- renderHighchart({
    data_filtrada <- data_filtrada()
    data_filtrada |> get_ddd("eje_programa_de_gobierno", "area_dentro_del_eje", "uno") |> hc_ddd()
  })

  output$home_chart_etapa_anio <- renderHighchart({
    data_filtrada <- data_filtrada()
    data_filtrada$etapa
    data_filtrada |>
      get_ddd("ano_de_ingreso", "etapa", "uno") |>
      mutate(
        # v1 = as.character(v1),
        v2 = fct_reorder(v2, value, sum, .desc = TRUE),
        ) |>
      hchart(
        type = "column",
        hcaes(x = v1, y = value, group = v2),
        stacking = 'normal'
      ) |>
      hc_tooltip(table = TRUE, sort = TRUE) |>
      hc_xAxis(title = list(text = "")) |>
      hc_yAxis(title = list(text = ""))

  })

  # mapa main ---------------------------------------------------------------
  output$mapa_main <- renderLeaflet({

    data_filtrada <- data_filtrada()

    data_mapa <- data_filtrada |>
      filter(!is.na(x), !is.na(y)) |>
      select(codigo, nombre, ano_de_ingreso, monto_aprobado, x, y, provincia_s)

    m <- leaflet(options = leafletOptions(attributionControl = FALSE, zoomControl = TRUE))

    if(nrow(data_mapa) == 0) {

      m <- m |>
        addControl(
          "No hay proyectos seleccionados con información geográfica.",
          position = "topright", className = "info legend"
          )

      return(m)

    }

    bnds <- data_mapa |>
      summarise(
        min(x, na.rm = TRUE), max(x, na.rm = TRUE),
        min(y, na.rm = TRUE), max(y, na.rm = TRUE)
        ) |>
      as.vector()

    m <- m |>
      addCircleMarkers(
        data = data_mapa,
        ~y,
        ~x,
        layerId = ~codigo,
        radius = 5,
        weight = 1,

        color = fndr_pars$secondary,
        opacity = 0.9,
        fillColor = fndr_pars$primary,
        fillOpacity = 0.7,

        label = ~paste(codigo, str_trunc(nombre, 25), sep = ": "),
        labelOptions = labelOptions(
          # offset = c(-20, -20),
          style = list(
            "font-family" = fndr_pars$font,
            "box-shadow" = "2px 2px rgba(0,0,0,0.15)",
            "font-size" = "12px",
            "padding" = "10px",
            "border-color" = "rgba(0,0,0,0.15)"
          )
        )

      ) |>
      addProviderTiles(providers$CartoDB.Positron)

    na_count <- nrow(data_filtrada) - nrow(data_mapa)

    if(na_count > 0){

      m <- m |>
        addControl(
          str_glue(
            "Se muestran {fmt_coma(na_count)} de los {fmt_coma(nrow(data_filtrada))} proyectos seleccionados."
          ),
          position = "bottomright",
          className = "info legend"
          )

    }

    m

  })

  # tabla main --------------------------------------------------------------
  output$tabla_main <- renderDataTable({

    # data$monto_aprobado
    data_filtrada <- data_filtrada()

    d <- data_filtrada |>
      select(
        codigo,
        nombre,
        ano_de_ingreso,
        sector, sub_sector,
        eje_programa_de_gobierno, area_dentro_del_eje,
        monto_aprobado,
        ) |>
      rename_all(~ str_to_title(str_replace_all(.x, "_", " ")))

    d |>
      DT::datatable(
        selection = "single",
        rownames = FALSE,
        options = list(
          language = list(url = '//cdn.datatables.net/plug-ins/1.10.11/i18n/Spanish.json')
        )
      ) |>
      DT::formatStyle(columns = names(d), fontSize = "80%")
  })

  # modal -------------------------------------------------------------------
  bip_selecionado <- reactiveVal(value = NULL, label = NULL)

  # se asigna si se hace click en el mapa
  observe({
    bip_selecionado(input$mapa_main_marker_click$id)
  }) |>
    bindEvent(input$mapa_main_marker_click)

  # se asigna si se hace click en la tabla
  observe({

    data_filtrada <- data_filtrada()

    data_filtrada |>
      pull(codigo) |>
      nth(input$tabla_main_rows_selected) |>
      bip_selecionado()

  }) |>
    bindEvent(input$tabla_main_rows_selected)

  observe({

    bip_selecionado <- bip_selecionado()
    # bip_selecionado <- 30094994

    if(is.null(bip_selecionado)) return(TRUE)

    # bip data
    bp <- data |>
      filter(codigo == bip_selecionado)

    m <- bp |>
      leaflet(
        options = leafletOptions(
          attributionControl = FALSE,
          zoomControl = TRUE
          )
        ) |>
      addCircleMarkers(
        # data = data_mapa,
        ~y,
        ~x,
        # layerId = ~codigo,
        radius = 5,
        weight = 1,
        color = fndr_pars$secondary,
        opacity = 0.9,
        fillColor = fndr_pars$primary,
        fillOpacity = 0.7,
        # popup = ~as.character(nombre),
        label = ~as.character(paste(codigo, provincia_s))
      ) |>
      addProviderTiles(providers$CartoDB.Positron)

    t <- bp |>
      select(
        codigo, nombre, sector, sub_sector,
        eje_programa_de_gobierno, area_dentro_del_eje
        ) |>
      mutate(across(everything(), as.character)) |>
      pivot_longer(cols = everything()) |>
      mutate(
        name = str_replace_all(name, "_", " "),
        name = str_to_title(name)
        ) |>
      pmap(function(name = "codigo", value = "30094994"){

        tagList(
          tags$dt(name),
          tags$dd(value)
        )

      }) |>
      tags$dl()

    showModal(
      modalDialog(
        fluidRow(
          column(12, t),
          # column(3, leaflet() |> addTiles()),
          # column(3, highcharts_demo()),
          # column(3, datatable(head(iris)))
        ),
        title = str_c(bp$codigo, bp$nombre, sep = " - "),
        size = "l",
        footer= NULL,
        easyClose = TRUE,
        fade = TRUE
      )
    )

  }) |>
    bindEvent(bip_selecionado())

  # fin server --------------------------------------------------------------
  # updateActionButton(session = session, "aplicar_filtros", label = "test")



}
