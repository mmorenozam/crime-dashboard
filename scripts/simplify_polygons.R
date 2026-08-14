## simplify_polygons.R ----------------------------------------------------
## The Quito outline and parish polygons in data/ come straight from the
## raw geoportal shapefiles and carry far more vertices than a Leaflet
## dashboard needs (157k and 575k respectively). Since they are drawn on
## EVERY page load regardless of any filter, that cost is paid by every
## user on every session. This script simplifies them offline (topology-
## preserving, so shared parish borders stay seamless with no slivers/gaps)
## and overwrites the deployed data/*.rds files in place.
##
## The original, unsimplified geometry can always be regenerated from
## data_geoportal_raw/ via R_prep/01_Data_Preparation.R, so this is safe
## to re-run or revert.
##
## Run once from the project root:
##   Rscript scripts/simplify_polygons.R

suppressPackageStartupMessages({
  library(sf)
  library(rmapshaper)
})

# keep = fraction of vertices to retain. 0.06 keeps a ~90-95% reduction,
# which is imperceptible at the city-dashboard zoom levels this app uses
# (zoom ~10, never zoomed to individual-building scale).
keep_fraction <- 0.06

simplify_and_report <- function(path, keep) {
  before <- readRDS(path)
  before_size <- file.size(path)
  before_vertices <- nrow(sf::st_coordinates(before))

  after <- rmapshaper::ms_simplify(
    before, keep = keep, keep_shapes = TRUE, sys = FALSE
  )
  # ms_simplify can occasionally emit invalid geometry at aggressive
  # simplification ratios; repair defensively before saving.
  after <- sf::st_make_valid(after)

  saveRDS(after, path)

  after_vertices <- nrow(sf::st_coordinates(after))
  after_size <- file.size(path)

  cat(sprintf(
    "%s: %s -> %s vertices (%.1f%% kept), %.2f MB -> %.2f MB\n",
    basename(path), format(before_vertices, big.mark = ","),
    format(after_vertices, big.mark = ","),
    100 * after_vertices / before_vertices,
    before_size / 1e6, after_size / 1e6
  ))
}

simplify_and_report(here::here("data", "01_quito_outline.rds"), keep_fraction)
simplify_and_report(here::here("data", "02_quito_parroquias.rds"), keep_fraction)
