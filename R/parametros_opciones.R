# parámetros y opciones ---------------------------------------------------
cli::cli_alert_info("parámetros y opciones")

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
  "navbar-bg" = fndr_pars$primary,
  "tooltip-color" = "black",
  "tooltip-bg" = "#f7f7f7",
) |>
  bs_add_rules(sass::sass_file("www/custom.scss"))

hcopts <- getOption("highcharter.chart")

hcopts$exporting <- list(
  enabled = TRUE,
  buttons = list(
    contextButton = list(
      menuItems = list(
        "printChart",
        # "separator",
        # "downloadPNG",
        "downloadJPEG",
        "downloadPDF",
        # "downloadSVG",
        # "separator",
        # "downloadCSV",
        "downloadXLS"
      ),
      symbolStrokeWidth = 1,
      symbolFill =  '#C0C0C0',
      symbolStroke = '#C0C0C0'
    )
  )
)

newlang_opts <- getOption("highcharter.lang")
newlang_opts$weekdays     <- c("domingo", "lunes", "martes", "miércoles", "jueves", "viernes", "sábado")
newlang_opts$months       <- c("enero", "febrero", "marzo", "abril", "mayo", "junio", "julio",
                               "agosto", "septiembre", "octubre", "noviembre", "diciembre")
newlang_opts$shortMonths  <- c("ene", "feb", "mar", "abr", "may", "jun", "jul", "ago", "sep",
                               "oct", "nov", "dic")
newlang_opts$drillUpText  <- "◁ Volver a {series.name}"
newlang_opts$loading      <- "Cargando información"
newlang_opts$downloadCSV  <- "Descargar CSV"
newlang_opts$downloadJPEG <- "Descargar JPEG"
newlang_opts$downloadPDF  <- "Descargar PDF"
newlang_opts$downloadPNG  <- "Descargar PNG"
newlang_opts$downloadSVG  <- "Descargar SVG"
newlang_opts$downloadXLS  <- "Descargar Excel"
newlang_opts$printChart   <- "Imprimir gráfico"
newlang_opts$viewFullscreen <- "Ver pantalla completa"
newlang_opts$resetZoom    <- "Resetear zoom"
newlang_opts$thousandsSep <- "."
newlang_opts$decimalPoint <- ","

newlang_opts$contextButtonTitle <- "Menú contextual del gráfico"
newlang_opts$numericSymbols <- JS("null")

options(
  highcharter.lang = newlang_opts,
  highcharter.theme = hc_theme(
    colors = c(fndr_pars$primary, fndr_pars$secondary, hc_theme_smpl()$colors[c(3:6)]),
    # colors = "#0C0C0C",
    chart = list(style = list(fontFamily = fndr_pars$font_sys)),
    title = list(style = list(fontFamily = fndr_pars$font_sys)),
    subtitle = list(style = list(fontFamily = fndr_pars$font_sys)),
    yAxis = list(endOnTick = FALSE),
    tooltip = list(valueDecimals = 0),
    plotOptions = list(
      series = list(
        dataLabels = list(style = list(fontWeight = "normal"))
      )
    )
  ),
  highcharter.chart = hcopts
)

rm(newlang_opts, hcopts)
