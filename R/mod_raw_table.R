## mod_raw_table.R ---------------------------------------------------------
## Generic searchable / sortable / downloadable raw-data table module,
## shared by both pages.

mod_raw_table_ui <- function(id) {
  ns <- NS(id)
  tagList(
    div(class = "d-flex justify-content-end mb-2",
        downloadButton(ns("download"), "Descargar datos filtrados (CSV)", class = "btn-sm btn-outline-secondary")
    ),
    DT::DTOutput(ns("tbl"))
  )
}

#' @param filtered_data reactive() -> filtered df
#' @param display_cols optional character vector limiting/ordering displayed columns
mod_raw_table_server <- function(id, filtered_data, display_cols = NULL, file_prefix = "data") {
  moduleServer(id, function(input, output, session) {

    display_df <- reactive({
      df <- filtered_data()
      if (!is.null(display_cols)) {
        cols <- intersect(display_cols, names(df))
        df <- df[, cols, drop = FALSE]
      }
      if (inherits(df, "sf")) df <- sf::st_drop_geometry(df)
      df
    })

    output$tbl <- DT::renderDT({
      df <- display_df()
      validate(need(nrow(df) > 0, "No hay datos para los filtros aplicados."))
      DT::datatable(
        df, filter = "top", rownames = FALSE,
        options = list(pageLength = 15, scrollX = TRUE)
      )
    })

    output$download <- downloadHandler(
      filename = function() paste0(file_prefix, "_filtered_", Sys.Date(), ".csv"),
      content = function(file) {
        utils::write.csv(display_df(), file, row.names = FALSE)
      }
    )
  })
}
