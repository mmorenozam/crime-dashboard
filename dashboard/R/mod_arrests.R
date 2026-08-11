## mod_arrests.R --------------------------------------------------------
## Full "Arrests" page: its own sidebar filters, its own reactive
## pipeline, and its own wiring of the shared map / trends / table modules.
## Deliberately independent from mod_homicides.R -- the "detenciones"
## dataset has different variables (tipo, jerarquia_iccs, decode_iccs,
## clase_tipo_lugar, etc.) so no common filtering framework is forced.

mod_arrests_ui <- function(id) {
  ns <- NS(id)

  page_sidebar(
    title = NULL,
    sidebar = sidebar(
      width = 300, open = "open",
      selectInput(ns("map_type"), "Tipo de mapa",
                  choices = c("Casos", "Mapa de calor", "Casos agrupados", "Incidencia"), selected = "Casos agrupados"),
      layout_column_wrap(
        width = 1/2,
        selectInput(ns("year_from"), "Desde", choices = NULL),
        selectInput(ns("year_to"), "Hasta", choices = NULL)
      ),
      selectizeInput(ns("tipo"), "Tipo de evento", choices = NULL, multiple = TRUE, selected = "Todos"),
      selectizeInput(ns("presunta_infraccion"), "Presunta infracción", choices = NULL, multiple = TRUE, selected = "Todos"),
      selectizeInput(ns("jerarquia_iccs"), "Familia ICCS", choices = NULL, multiple = TRUE),
      selectizeInput(ns("clase_tipo_lugar"), "Tipo de lugar", choices = NULL, multiple = TRUE),
      selectizeInput(ns("grupo_edad"), "Grupo de Edad", choices = NULL, multiple = TRUE),
      selectizeInput(ns("sexo"), "Sexo", choices = NULL, multiple = TRUE),
      selectizeInput(ns("parroquia"), "Parroquia", choices = NULL, multiple = TRUE),
      radioButtons(ns("area"), "Area", choices = c("Todos", "Urbana", "Rural"), inline = TRUE, selected = "Todos")
    ),

    navset_tab(
      id = ns("subtabs"),
      nav_panel("Mapa", mod_map_ui(ns("map"))),
      nav_panel("Tendencias", mod_trends_ui(ns("trends"))),
      nav_panel("Tablas de resumen",
                 card(card_header("Frecuencia Clasificación ICCS"), mod_summary_table_ui(ns("summary")))),
      nav_panel("Datos crudos", mod_raw_table_ui(ns("raw")))
    )
  )
}

#' @param data named list produced by load_app_data()
mod_arrests_server <- function(id, data) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    detenciones <- data$detenciones
    detenciones <- derive_time_fields(detenciones, "fecha_detencion_aprehension", "hora_detencion_aprehension")
    detenciones$year <- as.integer(format(detenciones$fecha_detencion_aprehension, "%Y"))
    detenciones$area <- detenciones$area_hecho

    # "detenciones" has no explicit urban/rural column in the spec besides
    # clase_tipo_lugar; if an "area" style column exists we use it, otherwise
    # the Area filter degrades gracefully to "Todos" only.
    # if (!"area" %in% names(detenciones)) {
    #   detenciones$area_hecho <- if ("area_hecho" %in% names(detenciones)) detenciones$area_hecho else NA_character_
    # }

    years <- sort(unique(stats::na.omit(detenciones$year)))

    observe({
      
      updateSelectInput(session, "year_from", choices = years, selected = min(years))
      updateSelectInput(session, "year_to", choices = years, selected = max(years))
      updateSelectizeInput(session, "tipo", choices = choices_from(detenciones$tipo), selected = "Todos", server = TRUE)
      updateSelectizeInput(session, "presunta_infraccion", choices = choices_from(detenciones$presunta_infraccion), 
                           selected = "Todos", server = TRUE)
      updateSelectizeInput(session, "jerarquia_iccs", choices = choices_from(detenciones$jerarquia_iccs), server = TRUE)
      updateSelectizeInput(session, "clase_tipo_lugar", choices = choices_from(detenciones$clase_tipo_lugar), server = TRUE)
      updateSelectizeInput(session, "grupo_edad", choices = choices_from(detenciones$grupo_edad), server = TRUE)
      updateSelectizeInput(session, "sexo", choices = choices_from(detenciones$sexo), server = TRUE)
      updateSelectizeInput(session, "parroquia", choices = choices_from(detenciones$nombre_parroquia), server = TRUE)
      updateRadioButtons(session, "area", choices = choices_from(detenciones$area), inline = TRUE)
    })

    year_range <- reactive({
      req(input$year_from, input$year_to)
      sanitize_year_range(input$year_from, input$year_to)
    })

    observeEvent(year_range(), {
      yr <- year_range()
      if (!is.na(yr$from) && as.character(yr$from) != input$year_from) {
        updateSelectInput(session, "year_from", selected = yr$from)
      }
      if (!is.na(yr$to) && as.character(yr$to) != input$year_to) {
        updateSelectInput(session, "year_to", selected = yr$to)
      }
    })

    filtered_data <- reactive({
      yr <- year_range()
      req(!is.na(yr$from), !is.na(yr$to))

      df <- filter_year_range(detenciones, "year", yr$from, yr$to)

      filters <- list(
        tipo                 = expand_all_selection(input$tipo, detenciones$tipo),
        presunta_infraccion  = expand_all_selection(input$presunta_infraccion, detenciones$presunta_infraccion),
        jerarquia_iccs       = expand_all_selection(input$jerarquia_iccs, detenciones$jerarquia_iccs),
        clase_tipo_lugar     = expand_all_selection(input$clase_tipo_lugar, detenciones$clase_tipo_lugar),
        grupo_edad           = expand_all_selection(input$grupo_edad, detenciones$grupo_edad),
        sexo                 = expand_all_selection(input$sexo, detenciones$sexo),
        nombre_parroquia     = expand_all_selection(input$parroquia, detenciones$nombre_parroquia),
        area_hecho           = expand_all_selection(input$area, detenciones$area_hecho)
      )
      df <- apply_filters(df, filters)

      if (!identical(input$area, "Todos")) {
        df <- df[!is.na(df$area_hecho) & df$area_hecho == input$area, , drop = FALSE]
      }

      df
    })

    split_col <- reactive({
      if (!is.null(input$parroquia) && length(input$parroquia) > 0 && !("Todos" %in% input$parroquia)) {
        return("nombre_parroquia")
      }
      if (!identical(input$area, "Todos")) {
        return("area")
      }
      NULL
    })

    # map_type_r <- reactive(input$map_type)
    map_type_r <- reactive({
      req(input$map_type)
      message("DETENCIONES MAP TYPE: ", input$map_type)
      input$map_type
    })

    mod_map_server(
      "map", filtered_data, map_type_r,
      quito_outline = data$quito_outline,
      quito_parroquias = data$quito_parroquias,
      pob_parroquias = data$pob_parroquias,
      year_range = year_range,
      popup_fields = c("fecha_detencion_aprehension", "tipo", "presunta_infraccion", "decode_iccs","nombre_parroquia")
    )

    mod_trends_server(
      "trends", filtered_data, map_type_r,
      pob_parroquias = data$pob_parroquias,
      year_range = year_range, area_col = "area",
      split_col = split_col
    )

    mod_summary_table_server("summary", filtered_data, group_col = "decode_iccs", label = "Clasificación ICCS")

    mod_raw_table_server(
      "raw", filtered_data,
      display_cols = c("fecha_detencion_aprehension", "hora_detencion_aprehension", "tipo",
                        "jerarquia_iccs", "grupo_edad", "sexo", "condicion", "movilizacion",
                        "tipo_arma", "nombre_parroquia", "presunta_infraccion",
                        "clase_tipo_lugar", "imputada", "decode_iccs", "year"),
      file_prefix = "detenciones"
    )
  })
}
