# funciones helpers -------------------------------------------------------
cli::cli_alert_info("funciones helpers")
# value_box_main <- function(value, description){
#   value_box(
#     title = NULL,
#     value = value,
#     description
#   )
# }

value_box <- partial(bslib::value_box, theme_color = "light")

valor_tipologia_mag_uni <- function(data, eje){
  data |>
    # as_data_frame() |>
    filter(tipologia_dentro_del_eje == eje) |>
    select(magnitud, unidad) |>
    reframe(
      magnitud = round(sum(magnitud, na.rm = TRUE), 2),
      unidad   = unique(unidad)
    ) |>
    # remover texto grande si es número
    mutate(
      magnitud = fmt_coma(magnitud),
      unidad = ifelse(str_detect(unidad, "N°"), "", unidad)
    ) |>
    str_glue_data("{magnitud} {unidad}") |>
    str_trim() |>
    tags$h4()
}

value_box_tipologia <- function(data, eje){
  value_box(
    title = NULL,
    value = tags$h3(valor_tipologia_mag_uni(data, eje)),
    eje
  )
}

# custom nav_panel para agregar clase `ttl` al titulo
nav_panel <- function (title, ..., value = title, icon = NULL) {
  bslib:::tabPanel_(
    tags$span(title, class = "ttl"),
    ...,
    value = value,
    icon = icon
  )
}

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

hc_ddd <- function(ddd, name = "", ...){

  ddd1 <- ddd |>
    group_by(v1) |>
    summarise(value = sum(value)) |>
    arrange(desc(value)) |>
    mutate(v1 = fct_inorder(v1)) |>
    ungroup()

  ddd2 <- ddd |>
    ungroup() |>
    arrange(desc(value)) |>
    group_nest(v1) |>
    mutate(
      id = v1,
      type = "column",
      name = v1,
      color = fndr_pars$secondary,
      data = map(data, mutate, name = v2, y = value),
      data = map(data, list_parse)
    )

  hchart(
    ddd1,
    type = "column",
    name = name,
    color = fndr_pars$primary,
    hcaes(x = v1, y = value, drilldown = v1)
  ) |>
    hc_drilldown(
      allowPointDrilldown = TRUE,
      series = list_parse(ddd2),
      activeAxisLabelStyle = list(
        textDecoration = 'none',
        fontStyle = 'normal',
        color = 'gray'
      )
    ) |>
    hc_xAxis(title = list(text = "")) |>
    hc_yAxis(title = list(text = ""))

}

# hc_ddd(ddd, name = "Provincia")
