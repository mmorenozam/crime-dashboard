library(openxlsx)
library(dplyr)
library(tidyr)
library(stringr)
library(readxl)
library(writexl)
library(ggplot2)



pob_01_10_22 <- read_excel(here::here("population/pob_parroquia_final_2001_2010_2022.xlsx"))

pob_01_10_22 <- pob_01_10_22 %>%
  select(area, grupo_parroquia, pob_total_2001, pob_total_2010, pob_total_2022, prop_hombre, prop_mujer)

pob_01_10_22 <- pob_01_10_22 %>%
  filter(!is.na(grupo_parroquia))

pob_censal_larga <- pob_01_10_22 %>%
  select(area, grupo_parroquia, pob_total_2001, pob_total_2010, pob_total_2022) %>%
  pivot_longer(
    cols = starts_with("pob_total_"),
    names_to = "anio",
    values_to = "poblacion_parroquia"
  ) %>%
  mutate(
    anio = as.numeric(str_remove(anio, "pob_total_"))
  )

total_quito_censal <- pob_censal_larga %>%
  group_by(anio) %>%
  summarise(
    poblacion_quito = sum(poblacion_parroquia, na.rm = TRUE),
    .groups = "drop"
  )

pob_censal_part <- pob_censal_larga %>%
  left_join(total_quito_censal, by = "anio") %>%
  mutate(
    participacion = poblacion_parroquia / poblacion_quito
  )

proy_quito <- tibble(
  anio = c(
    2001, 2010, 2011, 2012, 2013, 2014, 2015, 2016, 2017, 2018, 2019,
    2020, 2021, 2022, 2023, 2024, 2025, 2026, 2027, 2028, 2029,
    2030, 2031, 2032, 2033, 2034, 2035
  ),
  proy_quito = c(
    1823694, 2292804, 2345150, 2396342, 2446079, 2496183, 2546687,
    2595123, 2643569, 2698654, 2752623, 2782399, 2792784,
    2804269, 2820059, 2838174, 2856667, 2875075, 2893151,
    2910700, 2927602, 2943834, 2959453, 2974544, 2988948,
    3002540, 3015497
  )
)

proy_quito_pasada <- proy_quito %>%
  filter(anio < 2022)

anios_proyeccion <- proy_quito_pasada$anio


proy_lineal <- pob_censal_part %>%
  group_by(area, grupo_parroquia) %>%
  summarise(
    intercepto = coef(lm(participacion ~ anio))[1],
    pendiente  = coef(lm(participacion ~ anio))[2],
    .groups = "drop"
  ) %>%
  crossing(anio = anios_proyeccion) %>%
  mutate(
    part_proy = intercepto + pendiente * anio,
    modelo = "Lineal"
  )

proy_exponencial <- pob_censal_part %>%
  filter(participacion > 0) %>%
  group_by(area, grupo_parroquia) %>%
  summarise(
    intercepto = coef(lm(log(participacion) ~ anio))[1],
    pendiente  = coef(lm(log(participacion) ~ anio))[2],
    .groups = "drop"
  ) %>%
  crossing(anio = anios_proyeccion) %>%
  mutate(
    part_proy = exp(intercepto + pendiente * anio),
    modelo = "Exponencial"
  )

proy_logaritmico <- pob_censal_part %>%
  group_by(area, grupo_parroquia) %>%
  summarise(
    intercepto = coef(lm(participacion ~ log(anio)))[1],
    pendiente  = coef(lm(participacion ~ log(anio)))[2],
    .groups = "drop"
  ) %>%
  crossing(anio = anios_proyeccion) %>%
  mutate(
    part_proy = intercepto + pendiente * log(anio),
    modelo = "Logaritmico"
  )

proy_polinomico <- pob_censal_part %>%
  group_by(area, grupo_parroquia) %>%
  group_modify(~{
    
    modelo_poly <- lm(participacion ~ poly(anio, 2, raw = TRUE), data = .x)
    
    tibble(
      anio = anios_proyeccion,
      part_proy = predict(
        modelo_poly,
        newdata = tibble(anio = anios_proyeccion)
      )
    )
    
  }) %>%
  ungroup() %>%
  mutate(
    modelo = "Polinomico grado 2"
  )

proyecciones_modelos <- bind_rows(
  proy_lineal,
  proy_exponencial,
  proy_logaritmico,
  proy_polinomico
)

participaciones_negativas <- proyecciones_modelos %>%
  filter(part_proy < 0) %>%
  arrange(modelo, anio, grupo_parroquia)

proyecciones_modelos_ajustadas <- proyecciones_modelos %>%
  group_by(modelo, anio) %>%
  mutate(
    suma_part_anual = sum(part_proy, na.rm = TRUE),
    part_ajustada = part_proy / suma_part_anual
  ) %>%
  ungroup()


pob_modelos <- proyecciones_modelos_ajustadas %>%
  left_join(proy_quito_pasada, by = "anio") %>%
  mutate(
    pob_proy = round(part_ajustada * proy_quito, 0)
  ) %>%
  select(
    modelo,
    area,
    grupo_parroquia,
    anio,
    part_proy,
    part_ajustada,
    proy_quito,
    pob_proy
  )

pob_modelos_ajustados <- pob_modelos %>%
  group_by(modelo, anio) %>%
  arrange(desc(pob_proy), .by_group = TRUE) %>%
  mutate(
    diferencia_redondeo = first(proy_quito) - sum(pob_proy, na.rm = TRUE),
    pob_proy_ajustada = if_else(
      row_number() == 1,
      pob_proy + diferencia_redondeo,
      pob_proy
    )
  ) %>%
  ungroup() %>%
  select(
    modelo,
    area,
    grupo_parroquia,
    anio,
    part_proy,
    part_ajustada,
    proy_quito,
    pob_proy = pob_proy_ajustada
  )

pob_modelos_ancha <- pob_modelos_ajustados %>%
  select(
    area,
    grupo_parroquia,
    anio,
    modelo,
    pob_proy
  ) %>%
  pivot_wider(
    names_from = modelo,
    values_from = pob_proy
  ) %>%
  rename(
    Polinomico_grado_2 = `Polinomico grado 2`
  ) %>%
  arrange(area, grupo_parroquia, anio)

validacion_modelos <- pob_modelos_ajustados %>%
  group_by(modelo, anio) %>%
  summarise(
    suma_parroquias = sum(pob_proy, na.rm = TRUE),
    proy_quito = first(proy_quito),
    diferencia = suma_parroquias - proy_quito,
    .groups = "drop"
  )

base_train <- pob_censal_part %>%
  filter(anio %in% c(2001, 2022))

base_real_2010 <- pob_censal_part %>%
  filter(anio == 2010) %>%
  select(
    area,
    grupo_parroquia,
    part_real_2010 = participacion,
    pob_real_2010 = poblacion_parroquia
  )

val_lineal <- base_train %>%
  group_by(area, grupo_parroquia) %>%
  summarise(
    intercepto = coef(lm(participacion ~ anio))[1],
    pendiente  = coef(lm(participacion ~ anio))[2],
    .groups = "drop"
  ) %>%
  mutate(
    anio = 2010,
    part_proy_2010 = intercepto + pendiente * anio,
    modelo = "Lineal"
  )

val_exponencial <- base_train %>%
  filter(participacion > 0) %>%
  group_by(area, grupo_parroquia) %>%
  summarise(
    intercepto = coef(lm(log(participacion) ~ anio))[1],
    pendiente  = coef(lm(log(participacion) ~ anio))[2],
    .groups = "drop"
  ) %>%
  mutate(
    anio = 2010,
    part_proy_2010 = exp(intercepto + pendiente * anio),
    modelo = "Exponencial"
  )

val_logaritmico <- base_train %>%
  group_by(area, grupo_parroquia) %>%
  summarise(
    intercepto = coef(lm(participacion ~ log(anio)))[1],
    pendiente  = coef(lm(participacion ~ log(anio)))[2],
    .groups = "drop"
  ) %>%
  mutate(
    anio = 2010,
    part_proy_2010 = intercepto + pendiente * log(anio),
    modelo = "Logaritmico"
  )

validacion_retrospectiva <- bind_rows(
  val_lineal,
  val_exponencial,
  val_logaritmico
) %>%
  group_by(modelo, anio) %>%
  mutate(
    part_ajustada_2010 = part_proy_2010 / sum(part_proy_2010, na.rm = TRUE)
  ) %>%
  ungroup() %>%
  left_join(
    base_real_2010,
    by = c("area", "grupo_parroquia")
  ) %>%
  mutate(
    error_part = part_ajustada_2010 - part_real_2010,
    error_abs_part = abs(error_part),
    error_pct_part = if_else(
      part_real_2010 > 0,
      abs(error_part / part_real_2010) * 100,
      NA_real_
    )
  )

metricas_modelos <- validacion_retrospectiva %>%
  group_by(modelo) %>%
  summarise(
    MAE_part = mean(error_abs_part, na.rm = TRUE),
    RMSE_part = sqrt(mean(error_part^2, na.rm = TRUE)),
    .groups = "drop"
  ) %>%
  arrange(MAE_part)

metricas_modelos

mejor_modelo_retrospectivo <- metricas_modelos %>%
  slice_min(MAE_part, n = 1, with_ties = FALSE)

mejor_modelo_retrospectivo

mejor_modelo_por_parroquia <- validacion_retrospectiva %>%
  group_by(area, grupo_parroquia) %>%
  slice_min(error_abs_part, n = 1, with_ties = FALSE) %>%
  ungroup() %>%
  select(
    area,
    grupo_parroquia,
    mejor_modelo_parroquia = modelo,
    part_real_2010,
    part_ajustada_2010,
    error_abs_part,
    error_pct_part
  )

conteo_mejor_modelo_parroquia <- mejor_modelo_por_parroquia %>%
  count(mejor_modelo_parroquia, name = "n_parroquias") %>%
  arrange(desc(n_parroquias))

conteo_mejor_modelo_parroquia

errores_mayores_retrospectiva <- validacion_retrospectiva %>%
  arrange(desc(error_abs_part)) %>%
  select(
    modelo,
    area,
    grupo_parroquia,
    part_real_2010,
    part_ajustada_2010,
    error_part,
    error_abs_part,
    error_pct_part
  )

metricas_modelos_larga <- metricas_modelos %>%
  pivot_longer(
    cols = c(MAE_part, RMSE_part),
    names_to = "metrica",
    values_to = "valor"
  ) %>%
  group_by(metrica) %>%
  mutate(
    limite_inferior = min(valor, na.rm = TRUE) * 0.995,
    limite_superior = max(valor, na.rm = TRUE) * 1.005
  ) %>%
  ungroup() %>%
  mutate(
    metrica = recode(
      metrica,
      MAE_part = "MAE\nError absoluto promedio",
      RMSE_part = "RMSE\nPenaliza errores grandes"
    ),
    etiqueta = format(round(valor, 7), scientific = FALSE)
  )

grafico_metricas_modelos <- ggplot(
  metricas_modelos_larga,
  aes(
    x = reorder(modelo, valor),
    y = valor
  )
) +
  geom_col(width = 0.42) +
  geom_text(
    aes(label = etiqueta),
    hjust = -0.10,
    size = 3.3
  ) +
  coord_flip() +
  facet_wrap(
    ~ metrica,
    scales = "free_x"
  ) +
  scale_y_continuous(
    expand = expansion(mult = c(0.02, 0.25))
  ) +
  labs(
    title = "Desempeño retrospectivo de los modelos",
    subtitle = "Menor valor indica mejor desempeño retrospectivo",
    x = NULL,
    y = "Valor del error"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 13),
    plot.subtitle = element_text(size = 10),
    strip.text = element_text(face = "bold", size = 9),
    axis.text.y = element_text(size = 9),
    panel.grid.major.y = element_blank(),
    panel.grid.minor = element_blank()
  )

grafico_metricas_modelos

conteo_mejor_modelo_parroquia_graf <- conteo_mejor_modelo_parroquia %>%
  mutate(
    porcentaje = n_parroquias / sum(n_parroquias) * 100,
    etiqueta = paste0(n_parroquias, " (", round(porcentaje, 1), "%)")
  ) %>%
  arrange(desc(n_parroquias))

grafico_mejor_modelo_parroquia <- ggplot(
  conteo_mejor_modelo_parroquia_graf,
  aes(
    x = reorder(mejor_modelo_parroquia, n_parroquias),
    y = n_parroquias
  )
) +
  geom_col(width = 0.42) +
  geom_text(
    aes(label = etiqueta),
    hjust = -0.12,
    size = 3.4
  ) +
  coord_flip() +
  scale_y_continuous(
    expand = expansion(mult = c(0, 0.28))
  ) +
  labs(
    title = "Modelo con menor error por parroquia",
    subtitle = "Número y porcentaje de parroquias donde cada modelo presentó el menor error retrospectivo",
    x = NULL,
    y = "Número de parroquias"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 13),
    plot.subtitle = element_text(size = 10),
    axis.text.y = element_text(size = 10),
    axis.text.x = element_text(size = 9),
    panel.grid.major.y = element_blank(),
    panel.grid.minor = element_blank()
  )

grafico_mejor_modelo_parroquia

r2_lineal <- pob_censal_part %>%
  group_by(area, grupo_parroquia) %>%
  summarise(
    R2 = summary(lm(participacion ~ anio))$r.squared,
    modelo = "Lineal",
    .groups = "drop"
  )

r2_exponencial <- pob_censal_part %>%
  filter(participacion > 0) %>%
  group_by(area, grupo_parroquia) %>%
  summarise(
    R2 = summary(lm(log(participacion) ~ anio))$r.squared,
    modelo = "Exponencial",
    .groups = "drop"
  )

r2_logaritmico <- pob_censal_part %>%
  group_by(area, grupo_parroquia) %>%
  summarise(
    R2 = summary(lm(participacion ~ log(anio)))$r.squared,
    modelo = "Logaritmico",
    .groups = "drop"
  )

r2_polinomico <- pob_censal_part %>%
  group_by(area, grupo_parroquia) %>%
  summarise(
    R2 = summary(lm(participacion ~ poly(anio, 2, raw = TRUE)))$r.squared,
    modelo = "Polinomico grado 2",
    .groups = "drop"
  )

r2_modelos <- bind_rows(
  r2_lineal,
  r2_exponencial,
  r2_logaritmico,
  r2_polinomico
)

resumen_r2_modelos <- r2_modelos %>%
  group_by(modelo) %>%
  summarise(
    R2_promedio = mean(R2, na.rm = TRUE),
    R2_mediana = median(R2, na.rm = TRUE),
    R2_min = min(R2, na.rm = TRUE),
    R2_max = max(R2, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(desc(R2_promedio))

resumen_r2_modelos


pob_observada_2010 <- pob_censal_part %>%
  filter(anio == 2010) %>%
  select(
    area,
    grupo_parroquia,
    anio,
    part_serie = participacion,
    pob_serie = poblacion_parroquia
  )

pob_proyectada_2001_2022 <- pob_modelos_ajustados %>%
  select(
    modelo,
    area,
    grupo_parroquia,
    anio,
    part_serie = part_ajustada,
    pob_serie = pob_proy
  )

pob_observada_2010_por_modelo <- pob_modelos_ajustados %>%
  distinct(modelo) %>%
  crossing(pob_observada_2010)

serie_2001_2022_modelos <- bind_rows(
  pob_observada_2010_por_modelo,
  pob_proyectada_2001_2022
) %>%
  arrange(modelo, area, grupo_parroquia, anio)


estabilidad_anual <- serie_2001_2022_modelos %>%
  group_by(modelo, area, grupo_parroquia) %>%
  arrange(anio, .by_group = TRUE) %>%
  mutate(
    cambio_abs_pob = pob_serie - lag(pob_serie),
    cambio_pct_pob = if_else(
      lag(pob_serie) > 0,
      (pob_serie / lag(pob_serie) - 1) * 100,
      NA_real_
    ),
    cambio_abs_part = part_serie - lag(part_serie)
  ) %>%
  ungroup()

eval_estabilidad_modelos <- estabilidad_anual %>%
  group_by(modelo) %>%
  summarise(
    promedio_cambio_pct_anual = mean(abs(cambio_pct_pob), na.rm = TRUE),
    mediana_cambio_pct_anual = median(abs(cambio_pct_pob), na.rm = TRUE),
    max_cambio_pct_anual = max(abs(cambio_pct_pob), na.rm = TRUE),
    promedio_cambio_part_anual = mean(abs(cambio_abs_part), na.rm = TRUE),
    max_cambio_part_anual = max(abs(cambio_abs_part), na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(promedio_cambio_pct_anual)

eval_estabilidad_modelos

saltos_extremos_anuales <- estabilidad_anual %>%
  filter(!is.na(cambio_pct_pob)) %>%
  arrange(desc(abs(cambio_pct_pob))) %>%
  select(
    modelo,
    area,
    grupo_parroquia,
    anio,
    pob_serie,
    cambio_abs_pob,
    cambio_pct_pob,
    cambio_abs_part
  )

saltos_exponencial <- estabilidad_anual %>%
  filter(modelo == "Exponencial", !is.na(cambio_pct_pob)) %>%
  arrange(desc(abs(cambio_pct_pob))) %>%
  select(
    modelo,
    area,
    grupo_parroquia,
    anio,
    pob_serie,
    cambio_abs_pob,
    cambio_pct_pob,
    cambio_abs_part
  )

grafico_estabilidad_modelos <- ggplot(
  eval_estabilidad_modelos,
  aes(
    x = reorder(modelo, promedio_cambio_pct_anual),
    y = promedio_cambio_pct_anual
  )
) +
  geom_col(width = 0.42) +
  geom_text(
    aes(label = paste0(round(promedio_cambio_pct_anual, 2), "%")),
    hjust = -0.12,
    size = 3.4
  ) +
  coord_flip() +
  scale_y_continuous(
    expand = expansion(mult = c(0, 0.28))
  ) +
  labs(
    title = "Estabilidad anual de las proyecciones por modelo",
    subtitle = "Promedio del cambio porcentual anual absoluto por parroquia, 2022–2035",
    x = NULL,
    y = "Cambio porcentual anual promedio (%)"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 13),
    plot.subtitle = element_text(size = 10),
    axis.text.y = element_text(size = 10),
    axis.text.x = element_text(size = 9),
    panel.grid.major.y = element_blank(),
    panel.grid.minor = element_blank()
  )

grafico_estabilidad_modelos

grafico_saltos_maximos <- ggplot(
  eval_estabilidad_modelos,
  aes(
    x = reorder(modelo, max_cambio_pct_anual),
    y = max_cambio_pct_anual
  )
) +
  geom_col(width = 0.42) +
  geom_text(
    aes(label = paste0(round(max_cambio_pct_anual, 2), "%")),
    hjust = -0.12,
    size = 3.4
  ) +
  coord_flip() +
  scale_y_continuous(
    expand = expansion(mult = c(0, 0.28))
  ) +
  labs(
    title = "Mayor salto anual proyectado por modelo",
    subtitle = "Máximo cambio porcentual anual observado en una parroquia, 2022–2035",
    x = NULL,
    y = "Máximo cambio porcentual anual (%)"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 13),
    plot.subtitle = element_text(size = 10),
    axis.text.y = element_text(size = 10),
    axis.text.x = element_text(size = 9),
    panel.grid.major.y = element_blank(),
    panel.grid.minor = element_blank()
  )

grafico_saltos_maximos

modelo_final <- mejor_modelo_retrospectivo %>%
  select(modelo, MAE_part, RMSE_part)

modelo_final

nombre_modelo_final <- modelo_final$modelo[1]

pob_modelo_final <- pob_modelos_ajustados %>%
  filter(modelo == nombre_modelo_final) %>%
  arrange(area, grupo_parroquia, anio)

pob_2010_sexo_prop <- pob_01_10_22 %>%
  select(
    area,
    grupo_parroquia,
    pob_total_2010,
    prop_hombre,
    prop_mujer
  )

pob_modelo_final <- pob_modelo_final %>%
  left_join(
    pob_2010_sexo_prop %>%
      select(area, grupo_parroquia, prop_hombre, prop_mujer),
    by = c("area", "grupo_parroquia")
  ) %>%
  mutate(
    hombre = round(pob_proy * prop_hombre, 0),
    mujer = round(pob_proy * prop_mujer, 0)
  )

pob_modelo_final

write.xlsx(
  list(
    "poblacion_modelos" = pob_modelos_ajustados,
    "tabla_ancha_modelos" = pob_modelos_ancha,
    "validacion_total" = validacion_modelos,
    "metricas_retrospectivas" = metricas_modelos,
    "mejor_modelo_parroquia" = mejor_modelo_por_parroquia,
    "conteo_modelo_parroquia" = conteo_mejor_modelo_parroquia,
    "r2_modelos" = resumen_r2_modelos,
    "estabilidad_modelos" = eval_estabilidad_modelos,
    "poblacion_2010" = pob_2010_sexo_prop,
    "modelo_final" = pob_modelo_final
  ),
  here::here("population/proyeccion_parroquial_modelos.xlsx"),
  overwrite = TRUE
)

