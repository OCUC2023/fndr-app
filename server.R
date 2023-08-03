# source("global.R")
# input <- list(sector = "Multisectorial", anios = c(2018, 2023))

function(input, output, session) {

  # data --------------------------------------------------------------------
  data_filtrada <- reactive({
    cli::cli_h2("reactive data_filtrada")

    data_filtrada <- data |>
      filter(eje_programa_de_gobierno %in% if(is.null(input$eje_programa_de_gobierno)) unique(data$eje_programa_de_gobierno) else input$eje_programa_de_gobierno) |>
      filter(area_dentro_del_eje %in% if(is.null(input$area_dentro_del_eje)) unique(data$area_dentro_del_eje) else input$area_dentro_del_eje) |>

      filter(provincia_s %in% if(is.null(input$provincia_s)) unique(data$provincia_s)     else input$provincia_s) |>
      filter(comuna_s %in% if(is.null(input$comuna_s)) unique(data$comuna_s) else input$comuna_s) |>

      filter(fase_oficial      %in% if(is.null(input$fase))   unique(data$fase_oficial)      else input$fase)   |>
      filter(codigo            %in% if(is.null(input$codigo)) unique(data$codigo)            else input$codigo) |>
      filter(ano_de_iniciativa %in% if(is.null(input$anios))  unique(data$ano_de_iniciativa) else input$anios)  |>
      filter(TRUE)

    cli::cli_inform("{fmt_coma(nrow(data_filtrada))} filas")

    data_filtrada

  })

  # reseta/update filtros
  observe({
    cli::cli_h2("observe reset_filtros")

    updateSelectInput(session, "eje_programa_de_gobierno", selected = NA)
    updateSelectInput(session, "area_dentro_eje", selected = NA)

    updateSelectInput(session, "provincia_s", selected = NA)
    updateSelectInput(session, "comuna_s", selected = NA)

    updateSelectInput(session, "fase"  , selected = NA)
    updateSelectInput(session, "codigo", selected = NA)
    updateSelectInput(session, "anios" , selected = NA)

  }) |>
    bindEvent(input$reset_filtros)

  # textos ------------------------------------------------------------------
  output$iniciativas_seleccionadas <- renderUI({
    d <- data_filtrada()
    nr <- nrow(d)
    tagList(
      icon("filter"),
      str_glue("{fmt_coma(nr)} iniciativas seleccionadas.")
      )
  })

  # mapa main ---------------------------------------------------------------
  output$mapa_main <- renderLeaflet({

    leaflet(options = leafletOptions(attributionControl = FALSE, zoomControl = TRUE)) |>
      addProviderTiles(providers$CartoDB.Positron)

  })

  # value boxes -------------------------------------------------------------
  output$hero_aceras <- renderUI(valor_tipologia_mag_uni(data_filtrada(), "Aceras"))
  output$hero_pavcal <- renderUI(valor_tipologia_mag_uni(data_filtrada(), "Pavimentacion De Calzadas"))
  output$hero_refptn <- renderUI(valor_tipologia_mag_uni(data_filtrada(), "Refugios Peatonales"))
  output$hero_ciclov <- renderUI(valor_tipologia_mag_uni(data_filtrada(), "Ciclovias"))
  output$hero_estuds <- renderUI(valor_tipologia_mag_uni(data_filtrada(), "Estudios"))
  output$hero_lumina <- renderUI(valor_tipologia_mag_uni(data_filtrada(), "Luminarias"))
  output$hero_alarms <- renderUI(valor_tipologia_mag_uni(data_filtrada(), "Alarmas"))
  output$hero_bacheo <- renderUI(valor_tipologia_mag_uni(data_filtrada(), "Bacheo De Calzadas"))

  # tabla main --------------------------------------------------------------
  output$tabla_main <- renderDataTable({

    # data$monto_aprobado
    data_filtrada <- data_filtrada()

    d <- data_filtrada |>
      select(
        codigo,
        nombre,
        ano_de_iniciativa,
        eje_programa_de_gobierno,
        area_dentro_del_eje,
        costo_total,
        ) |>
      rename_all(~ str_to_title(str_replace_all(.x, "_", " ")))

    d |>
      DT::datatable(
        selection = "single",
        rownames = FALSE,
        fillContainer = TRUE,
        options = list(
          bPaginate = FALSE,
          searching = FALSE,
          info = FALSE,
          language = list(url = '//cdn.datatables.net/plug-ins/1.10.11/i18n/Spanish.json')
        )
      ) |>
      DT::formatStyle(columns = names(d), fontSize = "80%")
  })

  # modal -------------------------------------------------------------------
  # bindevent en tabla o en mapa
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

  # reporte -----------------------------------------------------------------
  # observer para mostrar notificaión
  # A queue of notification IDs
  ids <- character(0)
  # A counter
  n <- 0

  nombre_reporte <- reactive({
    iniciativas <- data_filtrada() |> nrow()
    str_glue("reporte_fndr_{iniciativas}_iniciativas")
  })

  # https://shiny.rstudio.com/articles/generating-reports.html
  output$generar_reporte <- downloadHandler(
    # filename = "report.pdf",
    filename = function(){
      fs::path(nombre_reporte(), ext = "pdf")
    },
    content = function(file) {

      ni <- data_filtrada() |>
            nrow() |>
            scales::comma()

      id <- showNotification(
        tags$small(
          shiny::icon("spinner", class = "fa-spin"),
          str_glue(" Generando reporte de {ni} iniciativas seleccionadas...")
        ),
        duration = NULL,
        closeButton = FALSE,
        type = "default"
      )
      ids <<- c(ids, id)
      n <<- n + 1

      tempReport <- file.path(tempdir(), "reporte.Rmd")

      file.copy("reporte/reporte.Rmd", tempReport, overwrite = TRUE)

      codbips <- data_filtrada() |>
        pull(codigo)

      params <- list(
        bips = codbips,
        time = Sys.time(),
        path = here::here("data/data.rds")
        )

      rmarkdown::render(
        tempReport,
        output_file = file,
        params = params,
        envir = new.env(parent = globalenv())
      )

      if (length(ids) > 0)
        removeNotification(ids[1])
      ids <<- ids[-1]

    }
  )

  # fin server --------------------------------------------------------------

}
