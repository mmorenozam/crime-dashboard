source(here::here("R/00_Auxiliary_Functions.R"))

# Script descritption ####################################################
# Code explanation:
# This Data Preparation script is as messy as the data is, here
# I will start explaining step by step the logic of sequence of the
# the code below:
# 1. Create the outline of Quito from an .shp file that originally
#    contains the administrative contours of the Census Areas of
#    Quito.
# 2. Urban and rural parishes oulines: from .shp files containing
#    the administrative countours of rural and urban parishes
#    I create a single st object with both.
# 3. Preparing the homicide and detainees data sets: this part
#    is the most complicated one. the homicide data set does not
#    contain information regarding the parish in which the event took
#    place. Instead, has a variable denoting the circuit (administrative
#    unit used internally by the National Police to report crimes). 
#    Parish information is only available in the detainee data set
#    along with circuit information too. With that in mind, the sub-
#    steps here are:
#       3.a) Loading both data sets. On detainees data:  matching rows 
#            with ICCS decodes, correcting typos in ICCS codes origi-
#            nally reported and classififying `tipo_lugar` into a new
#            simpler variable with fewer classes: `clase_tipo_lugar`.
#            Finally, both data set are filtered for Quito data only.
#       3.b) Recoding `nombre_parroquia` in detainees data. This reco-
#            de was manually conducted (steps not included here). A new
#            df is created: `parroquia_id`, which contains the circuit
#            code and the parish name. This is going to be used later
#            for joining with the homicide data.
#       3.c) Geographical coordinates in the Quito's detainee data are con-
#            verted to numeric. This generates by purpose NAs for missing 
#            data.
#       3.d) Df `parroquia_tipo` is created (containing whether the parish
#            is urban/rural) and joined to the Quito's detainee data 
#       3.e) Both, national detainees df and Quito's detainee data are 
#            exported for ICCS code imputation (not in this repo, check:
#            https://mauricio-moreno.net/projects/iccs-imputation/)
#       3.f) Df `homicide` is joined with `parroquia_id`. This provides
#            parish name to this df. Then, geographical coordinates are
#            transformed to numbers and geodata validation takes place.
#            `qc_parroquia_function` outputs' are two sets of data, the
#            one called `validas` contains all geo validated data.
#            `invalidas` can be disregarded.
# 4. Population data: wrangles population data at the parish level for
#    Quito. Data were computed using methodology by official sources, see:
#    https://github.com/gobierno-abierto2026/Poblacion-Quito-2022-2035
#    Script is found in folder `population` of this repo.
#
# Summary inputs and outputs
# Inputs:  sec_a.shp (outline Quito)
#          parr_urbana_ord002_a.shp
#          organizacion_territorial_parroquial_rural_a.shp
#          mdi_detenidosaprehendidos_pm_2019_2025.xlsx
#          iccs_es.xlsx
#          03_Resultados_proyeccion2023_2035.xlsx
#          proyeccion_parroquial_modelos.xlsx
#
# Outputs: 01_quito_outline.rds
#          02_quito_parroquias.rds
#          apren_uio.rds (for data imputation)
#          apren_total.rds (for data imputation)
#          03_homicidios.rds
#          05_pob_parroquias.rds

# 1. Outline of Quito ####

quito_outline <- st_read(here::here("data_geoportal_raw/area/sec_a.shp")) |>
  st_transform(crs = 4326)

quito_outline <- st_union(st_make_valid(quito_outline)) |>
  st_as_sf()

saveRDS(quito_outline, here::here("dashboard/data/01_quito_outline.rds"))

# 2. Urban and rural parishes of Quito oulines ####

urbano <- st_read(here::here("data_geoportal_raw/urbano/parr_urbana_ord002_a.shp")) |>
  select(AD_ZONAL, dpa_parroq, dpa_despar) |>
  mutate(tipo = "URBANA")
rural <- st_read(here::here("data_geoportal_raw/rural/organizacion_territorial_parroquial_rural_a.shp")) |>
  select(AD_ZONAL = adm_zonal, dpa_parroq, dpa_despar) |>
  mutate(tipo = "RURAL", 
         AD_ZONAL = toupper(stri_trans_general(AD_ZONAL, id = "Latin-ASCII")), 
         dpa_parroq = toupper(stri_trans_general(dpa_parroq, id = "Latin-ASCII")), 
         dpa_despar = toupper(stri_trans_general(dpa_despar, id = "Latin-ASCII"))) |>
  filter(AD_ZONAL != "VARIAS")

quito_parroquias <- bind_rows(urbano, rural) %>%
  st_transform(crs = 4326)

saveRDS(quito_parroquias, here::here("dashboard/data/02_quito_parroquias.rds"))

# 3. Wrangling homicide data and detainees ####

## 3a ####

aprehendidos <- readxl::read_excel(here::here("data_crimen_raw/mdi_detenidosaprehendidos_pm_2019_2025.xlsx"),
                                   sheet = "1")

catalogo <- readxl::read_excel(here::here("data_crimen_raw/iccs_es.xlsx"),
                               sheet = "todos") |>
  drop_na()

aprehendidos <- iccs_correccion(aprehendidos, catalogo)
aprehendidos <- lugar_reclasificacion(aprehendidos)

aprehendidos_uio <- aprehendidos |>
  filter(nombre_subzona == "D.M. QUITO")

homicidios <- readxl::read_excel(here::here("data_crimen_raw/mdi_homicidiosintencionales_pm_2014_2025-2.xlsx"),
                                 sheet = "1. Homicidios Intencionales")
homicidios <- homicidios |>
  filter(subzona == "D.M. QUITO")

## 3b ####

aprehendidos_uio <- aprehendidos_uio |>
  mutate(nombre_parroquia = recode(nombre_parroquia,
                                   `ATAHUALPA (HABASPAMBA)` = "ATAHUALPA",
                                   `ATAHUALPA (HABASPAMBA), CHAVEZPAMBA, PERUCHO` = "PERUCHO",
                                   `CALDERON (CARAPUNGO)` = "CALDERON",
                                   `CHECA (CHILPA)` = "CHECA",
                                   `CONCEPCION` = "LA CONCEPCION",
                                   `INAQUITO` = "IÑAQUITO",
                                   `QUITO` = "CONOCOTO"
  ))

parroquia_id <- aprehendidos_uio |>
  select(codigo_subcircuito, nombre_parroquia) |>
  distinct()

## 3c ####

aprehendidos_uio <- aprehendidos_uio |>
  mutate(latitud = gsub(",", ".", latitud),
         longitud = gsub(",", ".", longitud),
         latitud = as.numeric(latitud), 
         longitud = as.numeric(longitud))

## 3d #### 

parroquias_tipo <- quito_parroquias |> 
  select(dpa_despar, area_lugar = tipo) |> 
  tibble() |> 
  select(-geometry)

aprehendidos_uio <- left_join(aprehendidos_uio, parroquias_tipo, by = c("nombre_parroquia" = "dpa_despar")) 

# saving outputs para imputation

## 3e ####

# saveRDS(aprehendidos_uio, "C:/Users/mmore/Documents/GitHub/mmorenozam.github.io/projects/iccs-imputation/raw_data/apren_uio.rds")
# 
# saveRDS(aprehendidos, "C:/Users/mmore/Documents/GitHub/mmorenozam.github.io/projects/iccs-imputation/raw_data/apren_total.rds")

## 3f ####

homicidios <- left_join(homicidios, parroquia_id, by = c("codigo_subcircuito"))

homicidios <- homicidios |>
  mutate(longitud = gsub(",", ".", coordenada_x),
         latitud = gsub(",", ".", coordenada_y),
         latitud = as.numeric(latitud), 
         longitud = as.numeric(longitud))


qc_parroquia_function(df = homicidios,
                      parroquias = quito_parroquias,
                      outpath_validas = "data_processed/validas.rds",
                      outpath_invalidas = "data_processed/invalidas.rds", 
                      out_path = here::here())

homicidio_valida <- readRDS(here::here("data_processed/validas.rds"))
saveRDS(homicidio_valida, here::here("dashboard/data/03_homicidios.rds"))

# 4. Population data ####

proy_futura <- read_excel(here::here("population/03_Resultados_proyeccion2023_2035.xlsx"), sheet = "modelo_exponencial") 

proy_futura <- proy_futura |>
  select(-modelo, -area, codigo_parroquia, nombre_parroquia, 
         year = anio, poblacion_parroquia = pob_proy, -proy_quito, -hombre, -mujer)

pob_2022 <- read_excel(here::here("population/03_Resultados_proyeccion2023_2035.xlsx"), sheet = "poblacion_2022") |> 
  mutate(year = 2022) |>
  select(-area, codigo_parroquia, nombre_parroquia, poblacion_parroquia = pob_total_2022, -prop_hombre, -prop_mujer)

proy23_35 <- bind_rows(pob_2022, proy_futura)

proy_pasada <- read_excel(here::here("population/proyeccion_parroquial_modelos.xlsx"), sheet = "modelo_final")|>
  filter(!anio %in% c(2001, 2010)) |>
  select(codigo_parroquia = grupo_parroquia, poblacion_parroquia = pob_proy, year = anio,
         -prop_hombre, -part_proy, 
         -prop_mujer, -hombre, -mujer, -part_ajustada, -proy_quito, -modelo, -area) 

pob_2010 <- read_excel(here::here("population/proyeccion_parroquial_modelos.xlsx"), sheet = "poblacion_2010") |>
  mutate(year = 2010) |>
  select(-area, codigo_parroquia = grupo_parroquia, year, poblacion_parroquia = pob_total_2010, -prop_hombre, 
         -prop_mujer)

proy10_22 <- bind_rows(pob_2010, proy_pasada)

codigo_parroquias <- proy23_35 |> select(codigo_parroquia, nombre_parroquia) |> distinct() 

proy10_22 <- right_join(codigo_parroquias, proy10_22, by = c("codigo_parroquia"))

pob_parroquias <- bind_rows(proy10_22, proy23_35)

pob_parroquias <- pob_parroquias |>
  mutate(nombre_parroquia = stri_trans_general(nombre_parroquia, "Latin-ASCII"))

saveRDS(pob_parroquias, here::here("dashboard/data/05_pob_parroquias.rds"))
