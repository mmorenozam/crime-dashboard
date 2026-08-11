## app.R ----------------------------------------------------------------
## Entry point: loads packages/modules, loads the data once, and wires the
## three pages together into a single page_navbar() application. All actual
## page logic lives in R/mod_*.R so this file stays a thin composition root.

library(shiny)
library(bslib)
library(leaflet)
library(leaflet.extras)
library(sf)
library(ggplot2)
library(plotly)
library(DT)

# --- Load helpers & modules -------------------------------------------------
path <- here::here()

r_files <- list.files(here::here(path,"R"), pattern = "\\.R$", full.names = TRUE)
invisible(lapply(r_files, source))

# --- Load data once at app start (shared, read-only, across sessions) -----
app_data <- load_app_data(data_dir = here::here(path,"data"))

# --- UI ---------------------------------------------------------------------
ui <- page_navbar(
  title = "Homicidios y Detenciones en Quito",
  id = "main_nav",
  theme = bs_theme(version = 5, bootswatch = "flatly"),
  fillable = TRUE,

  nav_panel("Homicidios", mod_homicides_ui("homicides")),
  nav_panel("Detenciones", mod_arrests_ui("arrests")),
  nav_panel("Acerca", mod_about_ui("about"))
)

# --- Server ------------------------------------------------------------------
server <- function(input, output, session) {
  mod_homicides_server("homicides", app_data)
  mod_arrests_server("arrests", app_data)
  mod_about_server("about")
}

shinyApp(ui, server)
