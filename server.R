# source("global.R")
input <- list(

)
# data_filtrada <- data
function(input, output, session) {

  # data --------------------------------------------------------------------
  # data pre debounce
  data_filtrada_pre <- reactive({

    cli::cli_h2("reactive data_filtrada")

    data_filtrada <- data
    # data_filtrada_export <- data
    cli::cli_inform("filtrando por años")

    # si todo está nulo, sin seleccionar, entonces mostramos todo! eoc filtramos
    # if(is.null(input$ano_resolucion) & is.null(input$ano_sesion) & is.null(input$ano_ingreso)){
    #   xp <- rep(TRUE, nrow(data_filtrada))
    # } else {
    #   # fechas con operador OR.
    #   x1 <- if(is.null(input$ano_resolucion)) rep(FALSE, nrow(data_filtrada)) else data_filtrada$ano_resolucion %in% input$ano_resolucion
    #   x2 <- if(is.null(input$ano_sesion))     rep(FALSE, nrow(data_filtrada)) else data_filtrada$ano_sesion     %in% input$ano_sesion
    #   x3 <- if(is.null(input$ano_ingreso))    rep(FALSE, nrow(data_filtrada)) else data_filtrada$ano_ingreso    %in% input$ano_ingreso
    #   xp <- x1 | x2 | x2
    # }
    #
    # data_filtrada <- filter(data_filtrada, xp)

    data_filtrada <- data_filtrada |>

      # eje & área gobierno
      # filter(eje_programa_de_gobierno %in% if(is.null(input$eje_programa_de_gobierno)) unique(data$eje_programa_de_gobierno) else input$eje_programa_de_gobierno) |>
      # filter(area_dentro_del_eje      %in% if(is.null(input$area_dentro_del_eje))      unique(data$area_dentro_del_eje)      else input$area_dentro_del_eje)      |>

      filter(categoria %in% if(is.null(input$tipologia)) unique(data$categoria) else input$tipologia) |>
      filter(nivel_1   %in% if(is.null(input$categoria)) unique(data$nivel_1)   else input$categoria) |>

      filter(periodo   %in% if(is.null(input$ano_periodo)) unique(data$periodo) else input$ano_periodo) |>

      # etapa y fase
      # filter(etapa     %in% if(is.null(input$etapa))     unique(data$etapa)     else input$etapa)     |>
      # filter(fase      %in% if(is.null(input$fase))      unique(data$fase)      else input$fase)      |>

      # área
      filter(alcance   %in% if(is.null(input$alcance))   unique(data$alcance)   else input$alcance)   |>
      filter(area      %in% if(is.null(input$area))      unique(data$area)      else input$area)      |>
      filter(provincia %in% if(is.null(input$provincia)) unique(data$provincia) else input$provincia) |>
      filter(comuna    %in% if(is.null(input$comuna))    unique(data$comuna)    else input$comuna)    |>

      # otros
      filter(codigo    %in% if(is.null(input$codigo))    unique(data$codigo)    else input$codigo)    |>

      filter(TRUE)

    if(!is.null(input$etiqueta)){
      cli::cli_inform("filtrando por etiqueta: {str_c(input$etiqueta, sep = ', ')}")

      patt <- str_c(input$etiqueta, collapse = "|")

      cli::cli_inform("filtrando por etiqueta: patrón {patt}")

      data_filtrada <- data_filtrada |>
        filter(str_detect(nivel_1, patt) | str_detect(nivel_2, patt) | str_detect(nivel_3, patt))

    }

    if(input$nombre != ""){
      cli::cli_inform("filtrando por nombre: {input$nombre}")

      data_filtrada <- data_filtrada |>
        filter(str_detect(str_to_lower(nombre), str_to_lower(input$nombre)))
    }

    cli::cli_inform("{fmt_coma(nrow(data_filtrada))} filas")

    data_filtrada

  })

  data_filtrada <- debounce(data_filtrada_pre, 2000)

  data_filtrada_export <- reactive({
    data_filtrada <- data_filtrada()
    # data_filtrada_export <- data_filtrada

    data_filtrada_export <- data_filtrada |>
      select(
        codigo,
        `Año Ingreso` = ano_ingreso,
        `Tipología` = tipologia,
        `Subtítulo` = subtitulo,
        convenios_glosas,
        alcance,
        `Área` = area,
        provincia,
        comuna,
        `Unidad Técnica` = unidad_tecnica,
        etapa,
        nombre,
        costo_total,
        fecha_admisibilidad,
        fase,
        rate,
        # `Fecha Última Revisión` = fecha_ultima_revision,
        `Fecha Sesión` = fecha_sesion,
        `Fecha Resolución` = fecha_resolucion,
        marco_presupuestario,
        `No Resolución` = no_resolucion,
        eje_programa_de_gobierno,
        `Área` = area,
        `Tipología dentro del eje` = tipologia_dentro_del_eje,
        unidad,magnitud,
        unidad_2,
        magnitud_2,
        unidad_3,
        magnitud_3,
        recintos
      )

    data_filtrada_export

  })

  # reseta/update filtros
  observe({
    cli::cli_h2("observe reset_filtros")

    # updateSelectInput(session, "eje_programa_de_gobierno", selected = NA)
    # updateSelectInput(session, "area_dentro_del_eje", selected = NA)

    updateSelectInput(session, "tipologia", selected = NA)
    updateSelectInput(session, "categoria", selected = NA)

    updateSelectInput(session, "ano_periodo", selected = NA)
    # updateSelectInput(session, "ano_resolucion", selected = NA)
    # updateSelectInput(session, "ano_sesion", selected = NA)
    # updateSelectInput(session, "ano_ingreso", selected = NA)

    # updateSelectInput(session, "etapa", selected = NA)
    # updateSelectInput(session, "fase", selected = NA)

    updateSelectInput(session, "alcance", selected = NA)
    updateSelectInput(session, "area", selected = NA)
    updateSelectInput(session, "provincia", selected = NA)
    updateSelectInput(session, "comuna", selected = NA)

    updateSelectInput(session, "codigo", selected = NA)
    updateSelectInput(session, "etiqueta", selected = NA)
    updateTextInput(session, "nombre" , value = "")

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

    data_filtrada <- data_filtrada()

    # dpuntos |> filter(codigo %in% pull(data_filtrada, codigo))

    data_mapa <- inner_join(dpuntos, data_filtrada, by = join_by(codigo), relationship = "many-to-many")

    data_mapa <- data_mapa |>
      group_by(codigo) |>
      mutate(codigo2 = str_c(codigo, row_number(), sep = "|"), .before = 1) |>
      ungroup()

    m <- leaflet(options = leafletOptions(attributionControl = FALSE, zoomControl = TRUE))

    if(nrow(data_mapa) == 0) {

      m <- m |>
        addControl(
          "No hay iniciativas seleccionadas con información geográfica.",
          position = "bottomright", className = "info legend"
        )

      return(m)

    }

    bnds <- data_mapa |>
      summarise(
        min(x, na.rm = TRUE), max(x, na.rm = TRUE),
        min(y, na.rm = TRUE), max(y, na.rm = TRUE)
      ) |>
      as.vector()

    m <- leaflet(options = leafletOptions(attributionControl = FALSE, zoomControl = TRUE)) |>
      addProviderTiles(providers$CartoDB.Positron) |>
      addCircleMarkers(
        data = data_mapa,
        ~x,
        ~y,
        layerId = ~codigo2,
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
      )
    # |>
    #   addControl(
    #     str_glue(
    #       "Se muestran {fmt_coma(nrow(data_mapa))} ubicaciones de las {fmt_coma(nrow(data_filtrada))} iniciativas seleccionadas."
    #     ),
    #     position = "bottomright",
    #     className = "info legend"
    #   )

    m

  })

  # value boxes -------------------------------------------------------------
  output$hero_boxes <- renderUI({
    data_filtrada <- data_filtrada()
    # data_filtrada <- data
    # data_filtrada |>
    #   filter(str_detect(unidad, "tricos")) |>
    #   View()
    dunimag1 <- data_filtrada |>
      group_by(eje_programa_de_gobierno, tipologia_dentro_del_eje, unidad) |>
      summarise(
        magnitud = round(sum(magnitud, na.rm = TRUE), 0),
        n = n(),
        .groups = "drop"
        ) |>
      arrange(desc(n)) |>
      mutate(magnitud = fmt_coma(magnitud))

    dunimag2 <- data_filtrada |>
      group_by(eje_programa_de_gobierno, tipologia_dentro_del_eje, unidad = unidad_2) |>
      summarise(
        magnitud = round(sum(magnitud_2, na.rm = TRUE), 0),
        n = n(),
        .groups = "drop"
      ) |>
      arrange(desc(n)) |>
      mutate(magnitud = fmt_coma(magnitud))

    dunimag <- bind_rows(dunimag1, dunimag2)
    # dunimag <- dunimag1

    vbs <- pmap(dunimag, function(eje_programa_de_gobierno, tipologia_dentro_del_eje, unidad, magnitud, n){

      # eje_programa_de_gobierno <- "Santiago sustentable"
      # tipologia_dentro_del_eje <- "Construcción, recuperación o mejoramiento de plazas"
      # unidad   <- "m2"
      # magnitud <- "289.145"
      # n        <- 120

      value_box(
        tags$span(
          tags$h6(tags$strong(eje_programa_de_gobierno)),
          tags$h5(tipologia_dentro_del_eje),
          tags$h3(magnitud, " ", unidad)
          ),
        value = NULL,
        theme = value_box_theme(bg = "white", fg = fndr_pars$fg)
        )

      })

    layout_column_wrap(
      width = 1/4,
      fillable = TRUE,
      fill = TRUE,
      !!!vbs
    )

  })

  # graficos ----------------------------------------------------------------
  output$chart_proy_prov_comuna <- renderHighchart({
    data_filtrada <- data_filtrada()
    data_filtrada |>
      get_ddd("provincia", "comuna", "uno") |>
      hc_ddd(name = "Provincia") |>
      hc_subtitle(text = "Provincia/Comuna") |>
      hc_yAxis(title = list(text = "Iniciativas"))
  })

  output$chart_proy_eje_area <- renderHighchart({
    data_filtrada <- data_filtrada()
    data_filtrada |>
      get_ddd("eje_programa_de_gobierno", "area_dentro_del_eje", "uno") |>
      hc_ddd(name = "Eje") |>
      hc_subtitle(text = "Eje/Área") |>
      hc_yAxis(title = list(text = "Iniciativas"))
  })

  output$chart_tipologia <- renderHighchart({
    data_filtrada <- data_filtrada()
    data_filtrada |>
      count(tipologia = fct_infreq(tipologia)) |>
      hchart("pie", hcaes(tipologia, n), color = fndr_pars$secondary, name = "Iniciativas") |>
      hc_subtitle(text = "Tipología")
  })

  output$chart_tipologia_dentro_eje <- renderHighchart({
    data_filtrada <- data_filtrada()
    daux <- data_filtrada |>
      count(
        tipologia_dentro_del_eje =
          fct_infreq(fct_lump_n(tipologia_dentro_del_eje, Inf, other_level = "Otras Tipologías"))
        )
    # daux <- head(daux, 6)
    hc <- hchart(daux, "bar", hcaes(tipologia_dentro_del_eje, n),
           color = fndr_pars$secondary, name = "Tipología dentro del eje") |>
      hc_subtitle(text = "Tipología dentro del eje") |>
      hc_xAxis(title = list(text = "Tipología dentro del eje")) |>
      hc_yAxis(title = list(text = "Iniciativas"))
    if(nrow(daux) > 10){
      hc <- hc |>
        hc_xAxis(min = 0, max = 9, scrollbar = list(enabled = TRUE))
    }
    hc
  })

  # monto -------------------------------------------------------------------
  output$chart_proy_prov_comuna_m <- renderHighchart({
    data_filtrada <- data_filtrada()
    data_filtrada |>
      get_ddd("provincia", "comuna", "costo_total") |>
      hc_ddd(name = "Provincia") |>
      hc_subtitle(text = "Provincia/Comuna") |>
      hc_yAxis(title = list(text = "Millones")) |>
      hc_yAxis(labels = list(formatter = JS("function(){return (Highcharts.numberFormat(this.value / 1000000, 0)) + ' M';}")))
    # https://jsfiddle.net/JVNjs/210/

  })

  output$chart_proy_eje_area_m <- renderHighchart({
    data_filtrada <- data_filtrada()
    data_filtrada |>
      get_ddd("eje_programa_de_gobierno", "area_dentro_del_eje", "costo_total") |>
      hc_ddd(name = "Eje") |>
      hc_subtitle(text = "Eje/Área") |>
      hc_yAxis(title = list(text = "Millones")) |>
      hc_yAxis(labels = list(formatter = JS("function(){return (Highcharts.numberFormat(this.value / 1000000, 0)) + ' M';}")))
  })

  output$chart_tipologia_m <- renderHighchart({
    data_filtrada <- data_filtrada()
    data_filtrada |>
      count(tipologia = fct_infreq(tipologia, costo_total), wt = costo_total) |>
      hchart("pie", hcaes(tipologia, n), color = fndr_pars$secondary, name = "Millones") |>
      hc_subtitle(text = "Tipología")
  })

  output$chart_tipologia_dentro_eje_m <- renderHighchart({
    data_filtrada <- data_filtrada()
    daux <- data_filtrada |>
      count(tipologia_dentro_del_eje = fct_infreq(tipologia_dentro_del_eje, costo_total), wt = costo_total)

    hc <- hchart(daux, "bar", hcaes(tipologia_dentro_del_eje, n),
                 color = fndr_pars$secondary, name = "Tipología dentro del eje") |>
      hc_subtitle(text = "Tipología dentro del eje") |>
      hc_xAxis(title = list(text = "Tipología dentro del eje")) |>
      hc_yAxis(title = list(text = "Millones")) |>
      hc_yAxis(labels = list(formatter = JS("function(){return (Highcharts.numberFormat(this.value / 1000000, 0)) + ' M';}")))
    if(nrow(daux) > 10){
      hc <- hc |>
        hc_xAxis(min = 0, max = 9, scrollbar = list(enabled = TRUE))
    }
    hc
  })

  # tabla main --------------------------------------------------------------
  output$tabla_main <- renderDataTable({

    data_filtrada_export <- data_filtrada_export()

    data_filtrada_export <- data_filtrada_export |>
      rename_all(~ str_to_title(str_replace_all(.x, "_", " ")))

    data_filtrada_export |>
      # select(`Costo Total`, everything()) |> head(10) |>
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
      DT::formatStyle(columns = names(data_filtrada_export), fontSize = "80%") |>
      DT::formatCurrency(columns = "Costo Total", currency = "", interval = 3, mark = ".", dec.mark = ",", digits = 0)
  })

  # modal -------------------------------------------------------------------
  # bindevent en tabla o en mapa
  bip_selecionado <- reactiveVal(value = NULL, label = NULL)

  # se asigna si se hace click en el mapa
  observe({
    cp <- input$mapa_main_marker_click$id
    print(cp)
    # cp <- "23423|3453"
    cp <- str_remove(cp, "\\|.*")
    print(cp)

    cli::cli_alert_info("Mapa clickeado {cp}")
    el <- list(cp, runif(1))
    bip_selecionado(el)
  }) |>
    bindEvent(input$mapa_main_marker_click)

  # se asigna si se hace click en la tabla
  observe({
    cli::cli_alert_info("Tabla clickeada {input$tabla_main_rows_selected}")
    data_filtrada <- data_filtrada()

    cb <- data_filtrada |>
      pull(codigo) |>
      nth(input$tabla_main_rows_selected)

    el <- list(cb, runif(1))
    bip_selecionado(el)

  }) |>
    bindEvent(input$tabla_main_rows_selected)

  observe({

    cli::cli_alert_info("Tabla/Mapa clickeado")
    bip_selecionado <- bip_selecionado()[1]
    # bip_selecionado <- 40046745

    if(is.null(bip_selecionado)) return(TRUE)

    # bip data
    bp <- data |>
      filter(codigo == bip_selecionado)

    # bp_mapa <- inner_join(dpuntos, bp, by = join_by(codigo))
    #
    # m <- bp_mapa |>
    #   leaflet(
    #     options = leafletOptions(
    #       attributionControl = FALSE,
    #       zoomControl = TRUE
    #       )
    #     ) |>
    #   addCircleMarkers(
    #     # data = data_mapa,
    #     ~x,
    #     ~y,
    #     # layerId = ~codigo,
    #     radius = 5,
    #     weight = 1,
    #     color = fndr_pars$secondary,
    #     opacity = 0.9,
    #     fillColor = fndr_pars$primary,
    #     fillOpacity = 0.7,
    #     # popup = ~as.character(nombre),
    #     label = ~as.character(paste(codigo, provincia_s))
    #   ) |>
    #   addProviderTiles(providers$CartoDB.Positron)

    t <- bp |>
      select(
        codigo, nombre, sector, sub_sector,
        eje_programa_de_gobierno, area_dentro_del_eje,
        costo_total
        ) |>
      mutate(costo_total = fmt_coma(costo_total)) |>
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
        fluidRow(column(12, t)),
        title = str_c(bp$codigo, bp$nombre, sep = " - "),
        size = "xl",
        footer= NULL,
        easyClose = TRUE,
        fade = TRUE
      )
    )

  }) |>
    bindEvent(bip_selecionado())



  # download data -----------------------------------------------------------
  output$descargar_datos<- downloadHandler(
    filename = function() {
      fs::path(str_glue("iniciativas_{nrow(data_filtrada_export())}"), ext = "xlsx")
    },
    content = function(file) {
      tempdata    <- file.path(tempdir(), "datos.xlsx")
      data_filtrada_export <- data_filtrada_export()
      writexl::write_xlsx(data_filtrada_export(), file)
    }
  )

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
