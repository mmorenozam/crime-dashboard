## mod_map.R ------------------------------------------------------------
## Reusable Leaflet map module shared by Homicides and Arrests pages.
## Both pages call the same module with different reactive data frames,
## which keeps the map rendering logic in exactly one place.

mod_map_ui <- function(id) {
  ns <- NS(id)
  leaflet::leafletOutput(ns("map"), height = "650px")
}

## Above this many filtered points, "Casos" (raw markers) mode switches to
## clustering automatically -- unclustered SVG/canvas markers at this scale
## is what actually freezes the browser (e.g. the ~77k-row detenciones
## dataset). "Casos agrupados" already clusters unconditionally.
AUTO_CLUSTER_THRESHOLD <- 3000

#' @param id module id
#' @param filtered_data reactive() returning the filtered event data.frame
#'   (must contain latitud, longitud, parroquia_key, year)
#' @param map_type reactive() returning one of "Case locations", "Heatmap",
#'   "Clustered cases", "Incidence"
#' @param quito_outline sf polygon (static)
#' @param quito_parroquias sf polygon (static), must contain parroquia_key
#' @param pob_parroquias population data.frame (static)
#' @param year_range reactive() returning list(from=, to=)
#' @param popup_fields character vector of column names to show in point popups

mod_map_server <- function(id, filtered_data, map_type, quito_outline,
                            quito_parroquias, pob_parroquias, year_range, 
                            popup_fields = character(0)) {
  moduleServer(id, function(input, output, session) {
    
    output$map <- leaflet::renderLeaflet({
      leaflet::leaflet(options = leaflet::leafletOptions(preferCanvas = TRUE)) |>
        leaflet::addProviderTiles(leaflet::providers$CartoDB.Positron) |>
        leaflet::addPolygons(
          data = quito_outline, fill = FALSE,
          options = pathOptions(interactive = FALSE),
          color = "#333333", weight = 2, group = "outline"
        ) |>
        leaflet::addPolygons(
          data = quito_parroquias, fill = "transparent",
          color = "transparent",
          fillOpacity = 0.1,
          weight = 0.3,
          label = ~paste0("PARROQUIA:", parroquia_key),
          highlightOptions = highlightOptions(weight = 1, color = "black", bringToFront = F), group = "parroquia"
        ) |>
        leaflet::setView(lng = -78.4678, lat = -0.1807, zoom = 10)

    })
    
    outputOptions(output, "map", suspendWhenHidden = FALSE)

    observe({
      
      df   <- filtered_data()
      type <- map_type()

      proxy <- leaflet::leafletProxy("map", session) |>
        leaflet::clearGroup("points") |>
        leaflet::clearGroup("heat") |>
        leaflet::clearGroup("choropleth") |>
        leaflet::clearControls()

      df <- df[!is.na(df$latitud) & !is.na(df$longitud), , drop = FALSE]

      if (type == "Casos") {
        if (nrow(df) > 0) {
          popup_txt <- build_popup(df, popup_fields)
          auto_cluster <- nrow(df) > AUTO_CLUSTER_THRESHOLD
          proxy <- proxy |> leaflet::addCircleMarkers(
            data = df, lng = ~as.numeric(longitud), lat = ~as.numeric(latitud),
            radius = 4, stroke = FALSE, fillOpacity = 0.7, color = "#c0392b",
            popup = popup_txt, group = "points",
            clusterOptions = if (auto_cluster) leaflet::markerClusterOptions() else NULL
          )
          if (auto_cluster) {
            proxy <- proxy |> leaflet::addControl(
              html = sprintf(
                "<div style=\"background:white;padding:4px 8px;border-radius:4px;font-size:12px;box-shadow:0 1px 4px rgba(0,0,0,0.3);\">%s puntos agrupados automáticamente para mejorar el rendimiento</div>",
                format(nrow(df), big.mark = ",")
              ),
              position = "bottomleft"
            )
          }
        }
      } else if (type == "Mapa de calor") {
        if (nrow(df) > 0) {
          proxy <- proxy |> leaflet.extras::addHeatmap(
            data = df, lng = ~as.numeric(longitud), lat = ~as.numeric(latitud),
            radius = 14, blur = 20, max = 0.01, group = "heat" #max original = 0.6
          )
        }
      } else if (type == "Casos agrupados") {
        if (nrow(df) > 0) {
          popup_txt <- build_popup(df, popup_fields)
          proxy <- proxy |> leaflet::addCircleMarkers(
            data = df, lng = ~as.numeric(longitud), lat = ~as.numeric(latitud),
            radius = 5, stroke = FALSE, fillOpacity = 0.7, color = "#2980b9",
            popup = popup_txt, group = "points",
            clusterOptions = leaflet::markerClusterOptions()
          )
        }
      } else if (type == "Incidencia") {
        yr <- year_range()
        agg <- compute_incidence_by_parish(
          df, pob_parroquias, yr$from, yr$to
        )
        poly <- quito_parroquias
        poly <- merge(poly, agg, by = "parroquia_key", all.x = TRUE)

        pal <- leaflet::colorNumeric(
          palette = "YlOrRd", domain = poly$incidencia, na.color = "#f0f0f0"
        )

        proxy <- proxy |> leaflet::addPolygons(
          data = poly, fillColor = ~pal(incidencia), fillOpacity = 0.75,
          color = "#555555", weight = 1, group = "choropleth",
          label = ~sprintf(
            "%s: %s per 100k (n=%s)",
            nombre_parroquia,
            ifelse(is.na(incidencia), "N/A", fmt_number(incidencia, 1)),
            ifelse(is.na(n), 0, n)
          )
        ) |> leaflet::addLegend(
          position = "bottomright", pal = pal, values = poly$incidencia,
          title = "Incidencia /100k", na.label = "N/A"
        )
      }

      proxy
    })
  })
}

#' Build a simple HTML popup string vector from selected columns of df.
#' Vectorized per-column (rather than looping row-by-row with apply()) so
#' it stays fast on datasets with tens of thousands of rows.
build_popup <- function(df, fields) {
  fields <- fields[fields %in% names(df)]
  if (length(fields) == 0) return(NULL)
  parts <- lapply(fields, function(f) sprintf("<b>%s:</b> %s", f, as.character(df[[f]])))
  do.call(paste, c(parts, sep = "<br>"))
}
