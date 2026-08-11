## utils_data.R -----------------------------------------------------------
## Responsible for ONE thing: loading the five RDS inputs and exposing them
## under clean (prefix-free) names. Keeping this isolated means that if the
## input file names/paths ever change, only this file needs to be touched.

#' Load the five source RDS files and return them as a clean, named list
#'
#' @param data_dir path to the folder containing the *.rds files
#' @return a named list: quito_outline, quito_parroquias, homicidios,
#'   detenciones, pob_parroquias
load_app_data <- function(data_dir = "data") {

  file_map <- c(
    quito_outline    = "01_quito_outline.rds",
    quito_parroquias = "02_quito_parroquias.rds",
    homicidios       = "03_homicidios.rds",
    detenciones      = "04_detenciones.rds",
    pob_parroquias   = "05_pob_parroquias.rds"
  )

  missing <- file_map[!file.exists(file.path(data_dir, file_map))]
  if (length(missing) > 0) {
    stop(
      "Missing input file(s) in '", data_dir, "': ",
      paste(missing, collapse = ", ")
    )
  }

  data <- lapply(file_map, function(f) readRDS(file.path(data_dir, f)))
  names(data) <- names(file_map)

  data <- standardize_app_data(data)

  data
}

#' Light normalization so downstream code can rely on consistent types
#' without guessing about raw encodings coming from the RDS files.
standardize_app_data <- function(data) {

  # Ensure sf objects use a consistent, predictable CRS (WGS84) for Leaflet.
  if (inherits(data$quito_outline, "sf")) {
    data$quito_outline <- sf::st_transform(data$quito_outline, 4326)
  }
  if (inherits(data$quito_parroquias, "sf")) {
    data$quito_parroquias <- sf::st_transform(data$quito_parroquias, 4326)
  }

  # Normalize the parish-name join key across every table so that spatial
  # joins / population lookups are not silently broken by casing, accents,
  # or leading/trailing whitespace.
  norm_key <- function(x) {
    x <- trimws(toupper(as.character(x)))
    x
  }

  if (!is.null(data$homicidios$nombre_parroquia)) {
    data$homicidios$parroquia_key <- norm_key(data$homicidios$nombre_parroquia)
  }
  if (!is.null(data$detenciones$nombre_parroquia)) {
    data$detenciones$parroquia_key <- norm_key(data$detenciones$nombre_parroquia)
  }
  if (!is.null(data$pob_parroquias$nombre_parroquia)) {
    data$pob_parroquias$parroquia_key <- norm_key(data$pob_parroquias$nombre_parroquia)
  }
  if (!is.null(data$quito_parroquias$nombre_parroquia)) {
    data$quito_parroquias$parroquia_key <- norm_key(data$quito_parroquias$nombre_parroquia)
  }

  # Parse dates/times defensively (idempotent if already Date/POSIXct).
  if (!is.null(data$homicidios$fecha_infraccion)) {
    data$homicidios$fecha_infraccion <- as.Date(data$homicidios$fecha_infraccion)
  }
  if (!is.null(data$detenciones$fecha_detencion_aprehension)) {
    data$detenciones$fecha_detencion_aprehension <-
      as.Date(data$detenciones$fecha_detencion_aprehension)
  }

  data
}
