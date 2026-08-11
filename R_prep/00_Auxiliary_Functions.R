library(dplyr)
library(sf)
library(stringi)
library(tidyr)
library(stringr)
library(ggplot2)
library(lubridate)
library(readxl)
# Function to validate coordinates within the parishes of Quito

qc_parroquia_function <- function(df,
                                  parroquias,
                                  campo_parroquia_poligono = "dpa_despar",
                                  campo_parroquia_registrada = "nombre_parroquia",
                                  tolerancia_borde_m = 50,
                                  outpath_validas = NULL,
                                  outpath_invalidas = NULL,
                                  out_path = NULL) {
  
  ## --- Step 1: separate absent and non-numeric coordinates ------------------
  sin_coords <- df |>
    filter(is.na(longitud) | is.na(latitud) |
             !is.finite(longitud) | !is.finite(latitud)) |>
    mutate(parroquia_corregida = NA_character_, parroquia_coincide = NA)
  
  con_coords <- df |>
    filter(!is.na(longitud), !is.na(latitud),
           is.finite(longitud), is.finite(latitud))
  
  pts <- st_as_sf(con_coords, coords = c("longitud", "latitud"), crs = 4326, remove = FALSE)
  
  ## --- Step 2: Spatial join with no tolerance -----------------------
  # largest = TRUE solves the case where a point is right on the limit of a parish
  # and selects the parish where the major intersects exists, thus avoiding row duplications
  join_exacto <- st_join(pts, parroquias[campo_parroquia_poligono], join = st_intersects, largest = TRUE)
  pts$parroquia_espacial <- join_exacto[[campo_parroquia_poligono]]
  
  ## --- Step 3: For points that did not fall within any polygon,
  ## re-attempts with a little buffer (capture cases with boundary rounding)
  idx_sin_match <- which(is.na(pts$parroquia_espacial))
  
  if (length(idx_sin_match) > 0) {
    parroquias_buffer <- parroquias |>
      st_transform(32717) |>
      st_buffer(tolerancia_borde_m) |>
      st_transform(4326)
    
    join_buffer <- st_join(pts[idx_sin_match, ], parroquias_buffer[campo_parroquia_poligono],
                           join = st_intersects, largest = TRUE)
    pts$parroquia_espacial[idx_sin_match] <- join_buffer[[campo_parroquia_poligono]]
  }
  
  ## --- Step 4: Thos without a match, are left out of Quito ---------------
  pts$parroquia_corregida <- ifelse(is.na(pts$parroquia_espacial), "NO_EN_QUITO", pts$parroquia_espacial)
  
  ## --- Step 5: compare records with parish determined by coordinates ----------------
  registrado_norm <- pts[[campo_parroquia_registrada]]
  espacial_norm   <- pts$parroquia_corregida
  pts$parroquia_coincide <- registrado_norm == espacial_norm
  
  con_coords_final <- st_drop_geometry(pts) |> select(-parroquia_espacial)
  
  ## --- Step 6: bind rows and save ----
  resultado <- bind_rows(con_coords_final, sin_coords)
  
  n_discrepancias <- sum(resultado$parroquia_coincide == FALSE, na.rm = TRUE)
  n_fuera_quito <- sum(resultado$parroquia_corregida == "NO_EN_QUITO", na.rm = TRUE)
  cat("=== QC Counties (spatial validation) ===\n")
  cat("Total records:", nrow(resultado), "\n")
  cat("Recorded parish does not coincide with geospatial info:", n_discrepancias,
      sprintf("(%.2f%%)\n", 100 * n_discrepancias / nrow(resultado))) 
  cat("Points out of any parish (possibly out of Quito):", n_fuera_quito, "\n")
  
  if (!is.null(outpath_validas)) {
    saveRDS(resultado, here::here(out_path, outpath_validas))
  }
  if (!is.null(outpath_invalidas)) {
    discrepancias <- resultado |> filter(parroquia_coincide == FALSE)
    saveRDS(discrepancias, here::here(out_path, outpath_invalidas))
  }
  
  resultado
}

# Funcion para reclasificar el tipo_lugar ####

lugar_reclasificacion <- function(df){
  df <- df |> 
    mutate(
      clase_tipo_lugar = case_when(
        
        tipo_lugar %in% c(
          "CASA DE LA VICTIMA",
          "CASA DEL VICTIMARIO",
          "VIVIENDA PARTICULAR",
          "CONJUNTO HABITACIONAL",
          "QUINTA",
          "VIVIENDA/ALOJAMIENTO",
          "ESPACIO PRIVADO",
          "ÁREA PRIVADA",
          "HACIENDA"
        ) ~ "ESPACIO PRIVADO",
        
        tipo_lugar %in% c(
          "VÍA PÚBLICA",
          "ESPACIO PÚBLICO",
          "PARQUES",
          "PLAZAS",
          "SEMAFOROS",
          "ÁREAS DE ACCESO PÚBLICO",
          "ZONA DE INSPECCIÓN" 
        ) ~ "ESPACIO PUBLICO",
        
        tipo_lugar %in% c(
          "TRANSPORTE PÚBLICO",
          "AUTOBUS",
          "PARADA BUSES",
          "TRASPORTE PRIVADO",
          "INTERIOR DEL VEHICULO PARTICULAR",
          "INTERIOR DE TAXIS",
          "TRANSPORTE",
          "TRANSPORTE SUBTERRÁNEO",
          "COOPERATIVAS DE TRANSPORTES",
          "TERMINALES TERRESTRES",
          "AEROPUERTOS",
          "PUERTOS",
          "LANCHA",
          "EMBARCACIONES MARITIMAS"
        ) ~ "MEDIOS DE TRANSPORTE",
        
        tipo_lugar %in% c(
          "CENTROS COMERCIALES",
          "ALMACENES",
          "TIENDAS",
          "BODEGAS",
          "MERCADOS",
          "FERIAS LIBRES",
          "FERIA DE VEHICULOS",
          "CAFETERIAS",
          "RESTAURANTES",
          "HELADERIAS",
          "LICORERIAS",
          "GASOLINERAS",
          "FARMACIAS",
          "HOTELES",
          "LUGARES QUE PRESTAN SERVICIOS",
          "ÁREAS DEDICADAS AL COMERCIO",
          "SERVICIOS BASICOS",
          "SALAS DE BELLEZA",
          "MECANICAS",
          "VULCANIZADORAS",
          "CABINAS - CENTROS DE INTERNET",
          "TELEFONICAS",
          "CORREOS"
        ) ~ "CENTROS DE COMERCIO",
        
        tipo_lugar %in% c(
          "ESCUELAS",
          "COLEGIOS",
          "UNIVERSIDADES",
          "INSTITUTOS",
          "CENTROS INFANTILES",
          "INSTITUCIONES EDUCATIVAS"
        ) ~ "INSTITUCIONES EDUCATIVAS",
        
        tipo_lugar %in% c(
          "HOSPITALES",
          "CLINICAS",
          "CENTROS DE SALUD",
          "INSTITUCIONES DE SALUD"
        ) ~ "INSTITUCIONES DE SALUD",
        
        tipo_lugar %in% c(
          "BARES",
          "DISCOTECAS",
          "NIGTH CLUBS",
          "KARAOQUES",
          "SALAS DE JUEGO",
          "ESTADIOS",
          "COLISEOS",
          "PLAZA DE TOROS",
          "CENTROS DE DIVERSIÓN",
          "CANCHAS DE USO MULTIPLE"
        ) ~ "LUGARES DE RECREACION",
        
        tipo_lugar %in% c(
          "MINISTERIOS",
          "MUNICIPIOS",
          "REGISTRO CIVIL",
          "ENTIDADES PÚBLICAS",
          "ORGANISMOS INTERNACIONALES"
        ) ~ "INSTITUCIONES PUBLICAS E INTER.",
        
        tipo_lugar %in% c(
          "UNIDAD POLICIAL",
          "COMANDANCIA GENERAL DE POLICIA",
          "UNIDAD FUERZAS ARMADAS",
          "UNIDAD DE BOMBEROS",
          "CASA DE SEGURIDAD",
          "CENTROS DE REHABILITACIÓN SOCIAL",
          "CENTRO DE REHABILITACIÓN SOCIAL (CRS)",
          "CENTRO DE DETENCION PROVISIONAL",
          "CENTRO DE PRIVACIÓN PROVISIONAL DE LIBERTAD (CPPL)",
          "UNIDADES DE ASEGURAMIENTO TRANSITORIO (UAT)",
          "CENTRO DE ADOLESCENTES INFRACTORES (CAI)",
          "FISCALIA",
          "JUZGADOS",
          "CORTE NACIONAL DE JUSTICIA",
          "CONSEJO DE LA JUDICATURA",
          "UNIDADES DE REACCIÓN Y EMERGENCIA"
        ) ~ "INSTITUCIONES JUDICIALES",
        
        tipo_lugar %in% c(
          "RÍO",
          "LAGUNA - LAGO",
          "MAR",
          "BOSQUES",
          "QUEBRADA",
          "TROCHA",
          "CUEVA",
          "TERRENOS BALDIOS"
        ) ~ "AREA NATURAL",
        
        tipo_lugar %in% c(
          "ENTIDADES FINANCIERAS",
          "BANCOS",
          "COOPERATIVAS FINANCIERAS",
          "CAJEROS"
        ) ~ "ENTIDADES FINANCIERAS",
        
        TRUE ~ "Otros"
      )
    )
  return(df)
}

# Funcion para corregir codigos_iccs ####


iccs_correccion <- function(df, catalog){
  df <- df  |>
    mutate(codigo_iccs = case_when(!grepl("\\.", codigo_iccs) & codigo_iccs!="SIN_DATO" ~ paste0("0", codigo_iccs, ".??"),
                                   TRUE ~ codigo_iccs)) |>
    mutate(codigo_iccs = str_extract(codigo_iccs, "^[^.]+")) |>
    mutate(codigo_iccs = str_remove(codigo_iccs, "0*$")) |>
    mutate(codigo_iccs = case_when(presunta_infraccion == "BOLETAS" ~ "0000",
                                   TRUE ~ codigo_iccs)) |>
    mutate(codigo_iccs = case_when(codigo_iccs == "010101" ~ "0101",
                                   codigo_iccs == "010102" ~ "0102",
                                   codigo_iccs == "010109" ~ "0109",
                                   TRUE ~ codigo_iccs))
  df_iccs <- df |>
    select(iccs = codigo_iccs) |>
    distinct() |>
    filter(!iccs %in% c("SIN_DATO", "0000"))
  
  df_iccs_match <- df_iccs |>
    rowwise() %>%
    mutate({
      
      posibles <- catalog$code_iccs[
        str_starts(iccs, catalog$code_iccs)
      ]
      
      if(length(posibles) == 0){
        
        tibble(
          iccs_match = NA_character_,
          n_match = NA_integer_,
          sobrantes = NA_integer_,
          estado = "Sin match"
        )
        
      } else{
        
        mejor <- posibles[which.max(nchar(posibles))]
        
        tibble(
          iccs_match = mejor,
          n_match = nchar(mejor),
          sobrantes = nchar(iccs) - nchar(mejor),
          estado = case_when(
            sobrantes == 0 ~ "Exacto",
            TRUE ~ paste0("Sobran ", sobrantes, " caracteres")
          )
        )
        
      }
      
    }) %>%
    ungroup()
  
  diccionario_iccs <- setNames(
    df_iccs_match$iccs_match,
    df_iccs_match$iccs
  )
  
  df <- df |>
    ungroup()|>
    mutate(codigo_iccs = recode(codigo_iccs, !!!diccionario_iccs)) |>
    left_join(catalog, by = c("codigo_iccs" = "code_iccs")) |>
    mutate(codigo_iccs = na_if(codigo_iccs, "SIN_DATO"))
  return(df)
}