## generate_sample_data.R -----------------------------------------------
## Creates small SYNTHETIC datasets shaped like the real inputs described
## in the specification, purely so the dashboard can be smoke-tested
## end-to-end without the real (confidential) data. Run this once from the
## project root:
##
##   Rscript scripts/generate_sample_data.R
##
## It writes the five expected RDS files into data/.

set.seed(42)
if (!dir.exists("data")) dir.create("data")

suppressPackageStartupMessages({
  library(sf)
})

parroquias <- c("Carcelen", "Chillogallo", "Cotocollao", "Iñaquito",
                 "Kennedy", "La Ferroviaria", "La Mena", "Puengasi",
                 "Quitumbe", "San Isidro del Inca", "Solanda", "Turubamba")

n_parr <- length(parroquias)

# --- Simple grid-based polygons around Quito's approximate centroid -------
make_grid_polygons <- function(n, center_lon = -78.4678, center_lat = -0.1807, step = 0.05) {
  ncol <- ceiling(sqrt(n))
  polys <- vector("list", n)
  k <- 1
  for (i in seq_len(ncol)) {
    for (j in seq_len(ncol)) {
      if (k > n) break
      x0 <- center_lon + (i - ncol / 2) * step
      y0 <- center_lat + (j - ncol / 2) * step
      poly <- sf::st_polygon(list(rbind(
        c(x0, y0), c(x0 + step * 0.9, y0),
        c(x0 + step * 0.9, y0 + step * 0.9), c(x0, y0 + step * 0.9), c(x0, y0)
      )))
      polys[[k]] <- poly
      k <- k + 1
    }
  }
  sf::st_sfc(polys, crs = 4326)
}

geoms <- make_grid_polygons(n_parr)
quito_parroquias <- sf::st_sf(
  nombre_parroquia = parroquias,
  geometry = geoms
)

quito_outline <- sf::st_sf(
  nombre = "Distrito Metropolitano de Quito",
  geometry = sf::st_sfc(sf::st_union(sf::st_geometry(quito_parroquias)), crs = 4326)
)

# --- Population table (2001-2035) -----------------------------------------
years_pob <- 2001:2035
pob_parroquias <- do.call(rbind, lapply(parroquias, function(p) {
  base <- sample(15000:80000, 1)
  data.frame(
    nombre_parroquia = p,
    year = years_pob,
    poblacion = round(base * (1 + 0.01 * (years_pob - 2001)) * runif(length(years_pob), 0.97, 1.03))
  )
}))

# --- Homicidios --------------------------------------------------------
n_hom <- 1200
homicidios <- data.frame(
  tipo_muerte = sample(c("Homicidio intencional", "Femicidio", "Sicariato"), n_hom, replace = TRUE, prob = c(0.7, 0.15, 0.15)),
  area_hecho = sample(c("Urbana", "Rural"), n_hom, replace = TRUE, prob = c(0.85, 0.15)),
  tipo_lugar = sample(c("Via publica", "Domicilio", "Establecimiento comercial", "Otro"), n_hom, replace = TRUE),
  fecha_infraccion = as.Date("2018-01-01") + sample(0:2500, n_hom, replace = TRUE),
  hora_infraccion = sprintf("%02d:%02d:00", sample(0:23, n_hom, replace = TRUE), sample(0:59, n_hom, replace = TRUE)),
  presunta_motivacion = sample(c("Ajuste de cuentas", "Riña", "Robo", "Violencia intrafamiliar", "Desconocida"), n_hom, replace = TRUE),
  grupo_edad = sample(c("0-17", "18-24", "25-34", "35-44", "45-64", "65+"), n_hom, replace = TRUE),
  sexo = sample(c("Hombre", "Mujer"), n_hom, replace = TRUE, prob = c(0.85, 0.15)),
  nombre_parroquia = sample(parroquias, n_hom, replace = TRUE),
  arma = sample(c("Arma de fuego", "Arma blanca", "Objeto contundente", "Sin arma", "Otro"), n_hom, replace = TRUE),
  stringsAsFactors = FALSE
)
homicidios$year <- as.integer(format(homicidios$fecha_infraccion, "%Y"))

# Attach approximate coordinates near each parish's polygon centroid.
parr_centroids <- sf::st_coordinates(sf::st_centroid(quito_parroquias))
rownames(parr_centroids) <- quito_parroquias$nombre_parroquia
homicidios$longitud <- parr_centroids[homicidios$nombre_parroquia, "X"] + rnorm(n_hom, 0, 0.01)
homicidios$latitud  <- parr_centroids[homicidios$nombre_parroquia, "Y"] + rnorm(n_hom, 0, 0.01)

# --- Detenciones ---------------------------------------------------------
n_det <- 2500
iccs_families <- c("01 Homicidio", "02 Lesiones", "05 Robo", "07 Delitos sexuales", "10 Drogas")
decode_vals <- c("Robo a personas", "Robo a domicilios", "Trafico de drogas",
                  "Lesiones dolosas", "Violencia sexual", "Homicidio simple")

detenciones <- data.frame(
  codigo_iccs = sample(sprintf("ICCS-%03d", 1:20), n_det, replace = TRUE),
  tipo = sample(c("Detenido", "Aprehendido"), n_det, replace = TRUE),
  jerarquia_iccs = sample(iccs_families, n_det, replace = TRUE),
  grupo_edad = sample(c("0-17", "18-24", "25-34", "35-44", "45-64", "65+"), n_det, replace = TRUE),
  sexo = sample(c("Hombre", "Mujer"), n_det, replace = TRUE, prob = c(0.9, 0.1)),
  condicion = sample(c("Flagrancia", "Orden judicial"), n_det, replace = TRUE),
  movilizacion = sample(c("A pie", "Vehiculo", "Motocicleta"), n_det, replace = TRUE),
  tipo_arma = sample(c("Arma de fuego", "Arma blanca", "Sin arma"), n_det, replace = TRUE),
  fecha_detencion_aprehension = as.Date("2018-01-01") + sample(0:2500, n_det, replace = TRUE),
  hora_detencion_aprehension = sprintf("%02d:%02d:00", sample(0:23, n_det, replace = TRUE), sample(0:59, n_det, replace = TRUE)),
  nombre_parroquia = sample(parroquias, n_det, replace = TRUE),
  presunta_infraccion = sample(c("Robo", "Trafico de drogas", "Lesiones", "Violencia sexual", "Homicidio"), n_det, replace = TRUE),
  clase_tipo_lugar = sample(c("Via publica", "Domicilio", "Establecimiento comercial", "Otro"), n_det, replace = TRUE),
  imputada = sample(c("Si", "No"), n_det, replace = TRUE, prob = c(0.6, 0.4)),
  decode_iccs = sample(decode_vals, n_det, replace = TRUE),
  stringsAsFactors = FALSE
)
detenciones$longitud <- parr_centroids[detenciones$nombre_parroquia, "X"] + rnorm(n_det, 0, 0.01)
detenciones$latitud  <- parr_centroids[detenciones$nombre_parroquia, "Y"] + rnorm(n_det, 0, 0.01)

# --- Persist ---------------------------------------------------------------
saveRDS(quito_outline,    "data/01_quito_outline.rds")
saveRDS(quito_parroquias, "data/02_quito_parroquias.rds")
saveRDS(homicidios,       "data/03_homicidios.rds")
saveRDS(detenciones,      "data/04_detenciones.rds")
saveRDS(pob_parroquias,   "data/05_pob_parroquias.rds")

cat("Synthetic sample data written to data/\n")
