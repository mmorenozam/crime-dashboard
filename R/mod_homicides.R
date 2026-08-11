## mod_homicides.R ----------------------------------------------------------
## Full "Homicides" page: its own sidebar filters, its own reactive
## pipeline, and its own wiring of the shared map / trends / table modules.
## Deliberately independent from mod_arrests.R (different variables,
## different dataset) even though the visual layout is parallel.

mod_homicides_ui <- function(id) {
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
      selectizeInput(ns("tipo_muerte"), "Tipo de muerte", choices = NULL, multiple = TRUE,
                      selected = "Todos"),
      selectizeInput(ns("arma"), "Arma", choices = NULL, multiple = TRUE),
      selectizeInput(ns("motivacion"), "Presunta motivación", choices = NULL, multiple = TRUE,
                      selected = "Todos"),
      selectizeInput(ns("tipo_lugar"), "Tipo de lugar", choices = NULL, multiple = TRUE),
      selectizeInput(ns("grupo_edad"), "Grupo de edad", choices = NULL, multiple = TRUE),
      selectizeInput(ns("sexo"), "Sexo", choices = NULL, multiple = TRUE),
      selectizeInput(ns("parroquia"), "Parroquia", choices = NULL, multiple = TRUE),
      radioButtons(ns("area"), "Area", choices = c("Todos", "Urbana", "Rural"), inline = TRUE)
    ),

    navset_tab(
      id = ns("subtabs"),
      nav_panel("Mapa", mod_map_ui(ns("map"))),
      nav_panel("Tendencias", mod_trends_ui(ns("trends"))),
      nav_panel("Tablas de resumen",
                 card(card_header("Frecuencia armas"), mod_summary_table_ui(ns("summary")))),
      nav_panel("Datos crudos", mod_raw_table_ui(ns("raw")))
    )
  )
}

#' @param data named list produced by load_app_data()
mod_homicides_server <- function(id, data) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    homicidios <- data$homicidios
    homicidios <- derive_time_fields(homicidios, "fecha_infraccion", "hora_infraccion")
    homicidios$area <- homicidios$area_hecho

    years <- sort(unique(stats::na.omit(homicidios$year)))

    # --- Populate filter choices once, from the static dataset ------------
    observe({
      updateSelectInput(session, "year_from", choices = years, selected = min(years))
      updateSelectInput(session, "year_to", choices = years, selected = max(years))
      updateSelectizeInput(session, "tipo_muerte", choices = choices_from(homicidios$tipo_muerte), selected = "Todos", server = TRUE)
      updateSelectizeInput(session, "arma", choices = choices_from(homicidios$arma), server = TRUE)
      updateSelectizeInput(session, "motivacion", choices = choices_from(homicidios$presunta_motivacion), selected = "Todos", server = TRUE)
      updateSelectizeInput(session, "tipo_lugar", choices = choices_from(homicidios$tipo_lugar), server = TRUE)
      updateSelectizeInput(session, "grupo_edad", choices = choices_from(homicidios$grupo_edad), server = TRUE)
      updateSelectizeInput(session, "sexo", choices = choices_from(homicidios$sexo), server = TRUE)
      updateSelectizeInput(session, "parroquia", choices = choices_from(homicidios$nombre_parroquia), server = TRUE)
      updateRadioButtons(session, "area", choices = choices_from(homicidios$area), inline = TRUE)
    })

    # --- Prevent invalid "From > To" year combinations ---------------------
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

    # --- Central reactive pipeline for this page ---------------------------
    filtered_data <- reactive({
      yr <- year_range()
      req(!is.na(yr$from), !is.na(yr$to))

      df <- filter_year_range(homicidios, "year", yr$from, yr$to)

      filters <- list(
        tipo_muerte           = expand_all_selection(input$tipo_muerte, homicidios$tipo_muerte),
        arma                  = expand_all_selection(input$arma, homicidios$arma),
        presunta_motivacion   = expand_all_selection(input$motivacion, homicidios$presunta_motivacion),
        tipo_lugar            = expand_all_selection(input$tipo_lugar, homicidios$tipo_lugar),
        grupo_edad            = expand_all_selection(input$grupo_edad, homicidios$grupo_edad),
        sexo                  = expand_all_selection(input$sexo, homicidios$sexo),
        nombre_parroquia      = expand_all_selection(input$parroquia, homicidios$nombre_parroquia)
      )
      df <- apply_filters(df, filters)

      if (!identical(input$area, "Todos")) {
        df <- df[!is.na(df$area) & df$area == input$area, , drop = FALSE]
      }

      df
    })

    # Dynamic grouping: parish filter takes priority over area, per spec.
    split_col <- reactive({
      if (!is.null(input$parroquia) && length(input$parroquia) > 0 && !("Todos" %in% input$parroquia)) {
        return("nombre_parroquia")
      }
      if (!identical(input$area, "Todos")) {
        return("area")
      }
      NULL
    })

    map_type_r <- reactive(input$map_type)

    mod_map_server(
      "map", filtered_data, map_type_r,
      quito_outline = data$quito_outline,
      quito_parroquias = data$quito_parroquias,
      pob_parroquias = data$pob_parroquias,
      year_range = year_range,
      popup_fields = c("fecha_infraccion", "tipo_muerte", "arma", "nombre_parroquia")
    )

    mod_trends_server(
      "trends", filtered_data, map_type_r,
      pob_parroquias = data$pob_parroquias,
      year_range = year_range, area_col = "area",
      split_col = split_col
    )

    mod_summary_table_server("summary", filtered_data, group_col = "arma", label = "Weapon")

    mod_raw_table_server(
      "raw", filtered_data,
      display_cols = c("fecha_infraccion", "hora_infraccion", "tipo_muerte", "area_hecho",
                        "tipo_lugar", "presunta_motivacion", "grupo_edad", "sexo",
                        "nombre_parroquia", "year", "arma"),
      file_prefix = "homicidios"
    )
  })
}
