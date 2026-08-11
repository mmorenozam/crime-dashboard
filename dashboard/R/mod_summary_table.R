## mod_summary_table.R -----------------------------------------------------
## Generic frequency-table module. Each page decides which column to
## summarize (weapon for Homicides, decode_iccs for Arrests) and passes it
## in, so the rendering logic itself is written once.

mod_summary_table_ui <- function(id) {
  ns <- NS(id)
  DT::DTOutput(ns("tbl"))
}

#' @param filtered_data reactive() -> filtered df
#' @param group_col character, column name to summarize
#' @param label character, human-friendly label for the column header
mod_summary_table_server <- function(id, filtered_data, group_col, label = group_col) {
  moduleServer(id, function(input, output, session) {
    output$tbl <- DT::renderDT({
      df <- filtered_data()
      validate(need(nrow(df) > 0, "No hay suficientes datos para los filtros aplicados."))
      validate(need(group_col %in% names(df), paste("Columna", group_col, "no encontrada.")))

      agg <- as.data.frame(table(df[[group_col]], useNA = "ifany"))
      names(agg) <- c(label, "Frequency")
      agg <- agg[order(-agg$Frequency), ]
      agg$Percent <- round(100 * agg$Frequency / sum(agg$Frequency), 1)

      DT::datatable(
        agg, rownames = FALSE, options = list(pageLength = 10, order = list(list(1, "desc")))
      )
    })
  })
}
