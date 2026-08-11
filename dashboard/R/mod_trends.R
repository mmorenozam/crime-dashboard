## mod_trends.R -----------------------------------------------------------
## Reusable "Trends" tab: 2x2 grid of visualizations. Shared by both pages;
## behavior (counts vs incidence, split-by-parish/area) is driven entirely
## by reactive inputs passed in, so this file has no dataset-specific logic.

mod_trends_ui <- function(id) {
  ns <- NS(id)
  layout_column_wrap(
    width = 1/2, heights_equal = "row",
    card(card_header("Evolución temporal"), plotly::plotlyOutput(ns("temporal"), height = "320px")),
    card(card_header("Distribución por sexo"), plotly::plotlyOutput(ns("sex"), height = "320px")),
    card(card_header("Distribución por grupo de edad"), plotly::plotlyOutput(ns("age"), height = "320px")),
    card(card_header("Hora \u00d7 día de la semana - Mapa de calor"), plotly::plotlyOutput(ns("hourweek"), height = "320px"))
  )
}

#' @param filtered_data reactive() -> filtered df, must include:
#'   year, sexo, grupo_edad, weekday, hour_of_day, parroquia_key,
#'   nombre_parroquia, area_hecho (or NA)
#' @param map_type reactive() -> current map type string (drives counts vs incidence)
#' @param pob_parroquias static population df
#' @param year_range reactive() -> list(from=, to=)
#' @param area_col name of the urban/rural column in the dataset (may differ by page)
#' @param split_active reactive() -> TRUE if parish or area filter is active
#' @param split_col reactive() -> "nombre_parroquia" or "area" or NULL, whichever
#'   dimension is currently active for splitting (parish takes priority)
mod_trends_server <- function(id, filtered_data, map_type, pob_parroquias,
                               year_range, area_col, split_col) {
  moduleServer(id, function(input, output, session) {

    output$temporal <- plotly::renderPlotly({
      df <- filtered_data()
      validate(need(nrow(df) > 0, "No hay datos para los filtros aplicados."))
      grp <- split_col()

      if (identical(map_type(), "Incidencia")) {
        if (!is.null(grp) && grp %in% names(df)) {
          agg <- compute_incidence(df, pob_parroquias)
          agg <- merge(agg, unique(df[, c("parroquia_key", grp)]), by = "parroquia_key")
          p <- ggplot2::ggplot(agg, ggplot2::aes(x = year, y = incidencia, fill = .data[[grp]])) +
            ggplot2::geom_col(position = position_dodge2(width = 0.9, preserve = "single"))
        } else {
          agg <- compute_incidence(df, pob_parroquias)
          agg <- stats::aggregate(incidencia ~ year, data = agg, FUN = mean, na.rm = TRUE)
          p <- ggplot2::ggplot(agg, ggplot2::aes(x = year, y = incidencia)) +
            ggplot2::geom_col(fill = "#c0392b")
        }
        # if (!is.null(grp) && grp %in% names(df)) {
        #   agg <- compute_incidence(df, pob_parroquias)
        #   agg <- merge(agg, unique(df[, c("parroquia_key", grp)]), by = "parroquia_key")
        #   p <- ggplot2::ggplot(agg, ggplot2::aes(x = year, y = incidencia, color = .data[[grp]])) +
        #     ggplot2::geom_line(linewidth = 1) + ggplot2::geom_point()
        # } else {
        #   agg <- compute_incidence(df, pob_parroquias)
        #   p <- ggplot2::ggplot(agg, ggplot2::aes(x = year, y = incidencia)) +
        #     ggplot2::geom_line(linewidth = 1, color = "#c0392b") + ggplot2::geom_point()
        # }
        p <- p + ggplot2::labs(y = "Incidencia por 100k", x = "Year")
      } else {
        if (!is.null(grp) && grp %in% names(df)) {
          agg <- as.data.frame(table(year = df$year, grp = df[[grp]]))
          names(agg) <- c("year", grp, "n")
          agg$year <- as.numeric(as.character(agg$year))
          agg <- agg |>
            tidyr::complete(year = seq(min(year), max(year), by = 1),!!rlang::sym(grp), fill = list(n = 0))
          p <- ggplot2::ggplot(agg, ggplot2::aes(x = year, y = n, color = .data[[grp]])) +
            ggplot2::geom_line(linewidth = 1) + ggplot2::geom_point()
        } else {
          agg <- as.data.frame(table(year = df$year))
          names(agg) <- c("year", "n")
          agg$year <- as.numeric(as.character(agg$year))
          agg <- agg |>
            tidyr::complete(year = seq(min(year), max(year), by = 1), fill = list(n = 0))
          p <- ggplot2::ggplot(agg, ggplot2::aes(x = year, y = n)) +
            ggplot2::geom_line(linewidth = 1, color = "#c0392b") + ggplot2::geom_point()
        }
        p <- p + ggplot2::labs(y = "Conteo de casos", x = "Año")
      }
      plotly::ggplotly(p + ggplot2::theme_minimal())
    })

    output$sex <- plotly::renderPlotly({
      df <- filtered_data()
      validate(need(nrow(df) > 0, "No hay datos para los filtros aplicados."))
      grp <- split_col()

      if (!is.null(grp) && grp %in% names(df)) {
        agg <- as.data.frame(table(grp = df[[grp]], sexo = df$sexo))
        names(agg) <- c(grp, "sexo", "n")
        p <- ggplot2::ggplot(agg, ggplot2::aes(x = .data[[grp]], y = n, fill = sexo)) +
          ggplot2::geom_col(position = "fill") +
          ggplot2::labs(y = "Proporción", x = NULL) +
          ggplot2::coord_flip()
        plotly::ggplotly(p + ggplot2::theme_minimal())
      } else {
        agg <- as.data.frame(table(sexo = df$sexo))
        names(agg) <- c("sexo", "n")
        plotly::plot_ly(
          agg, labels = ~sexo, values = ~n, type = "pie",
          textinfo = "label+percent"
        )
      }
    })

    output$age <- plotly::renderPlotly({
      df <- filtered_data()
      validate(need(nrow(df) > 0, "No hay datos para los filtros aplicados."))
      agg <- as.data.frame(table(grupo_edad = df$grupo_edad))
      names(agg) <- c("grupo_edad", "n")
      p <- ggplot2::ggplot(agg, ggplot2::aes(x = stats::reorder(grupo_edad, n), y = n)) +
        ggplot2::geom_col(fill = "#2980b9") +
        ggplot2::coord_flip() +
        ggplot2::labs(x = NULL, y = "Conteo de casos")
      plotly::ggplotly(p + ggplot2::theme_minimal())
    })

    output$hourweek <- plotly::renderPlotly({
      df <- filtered_data()
      validate(need(nrow(df) > 0, "No hay datos para los filtros aplicados."))
      df <- df[!is.na(df$weekday) & !is.na(df$hour_of_day), , drop = FALSE]
      validate(need(nrow(df) > 0, "Insuficientes datos de fecha/tiempo."))

      agg <- as.data.frame(table(weekday = df$weekday, hour = df$hour_of_day))
      names(agg) <- c("weekday", "hour", "n")
      agg$hour <- as.integer(as.character(agg$hour))

      p <- ggplot2::ggplot(agg, ggplot2::aes(x = hour, y = weekday, fill = n)) +
        ggplot2::geom_tile() +
        ggplot2::scale_fill_gradient(low = "#fff5f0", high = "#c0392b") +
        ggplot2::labs(x = "Hora del día", y = NULL, fill = "Casos")
      plotly::ggplotly(p + ggplot2::theme_minimal())
    })
  })
}
