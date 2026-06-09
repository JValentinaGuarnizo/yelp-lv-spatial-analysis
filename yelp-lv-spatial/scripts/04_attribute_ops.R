# ============================================================================
# Proyecto: Análisis espacial de restaurantes en Las Vegas - Yelp
# Script: 04_attribute_ops.R
# Objetivo: Operaciones de atributos explícitas para análisis y rúbrica
# Input:  outputs/restaurants_lv.rds
#         outputs/restaurants_sf.rds
#         outputs/sentiment_neighborhood.rds
#         outputs/sentiment_cuisine.rds
# Output: outputs/restaurants_enriquecido.rds
#         outputs/resumen_neighborhood.rds
#         outputs/centroides_neighborhood.rds
# Autora: Análisis de datos
# Fecha: 13 de abril de 2026
# ============================================================================

# ============================================================================
# SECCIÓN 1: CARGAR LIBRERÍAS
# ============================================================================
library(tidyverse)
library(sf)

# ============================================================================
# SECCIÓN 2: CARGAR DATOS
# ============================================================================
message("📂 Cargando datos de entrada...")

restaurants_lv <- readRDS("outputs/restaurants_lv.rds")
restaurants_sf <- readRDS("outputs/restaurants_sf.rds")
sentiment_neighborhood <- readRDS("outputs/sentiment_neighborhood.rds")
sentiment_cuisine <- readRDS("outputs/sentiment_cuisine.rds")

message("   ✅ restaurants_lv: ", nrow(restaurants_lv), " restaurantes")
message("   ✅ restaurants_sf: ", nrow(restaurants_sf), " restaurantes")
message("   ✅ sentiment_neighborhood: ", nrow(sentiment_neighborhood), " neighborhoods")
message("   ✅ sentiment_cuisine: ", nrow(sentiment_cuisine), " cuisines")

# ============================================================================
# SECCIÓN 3: select() Y filter()
# ============================================================================
message("\n🔍 Aplicando select() y filter()...")

# ────────────────────────────────────────────────────────────────────────────
# OPERACIÓN 1: select()
# Por qué: Reducir dimensionalidad a solo columnas relevantes para análisis
# ────────────────────────────────────────────────────────────────────────────
restaurants_base <- restaurants_lv %>%
  select(
    business_id, name, neighborhood, cuisine,
    latitude, longitude, review_count
  )

message("   ✅ OPERACIÓN 1 (select): Reducido a 7 columnas relevantes")

# ────────────────────────────────────────────────────────────────────────────
# OPERACIÓN 2a: filter() - volumen de reseñas
# Por qué: Excluir negocios con pocas reseñas; son menos confiables
# ────────────────────────────────────────────────────────────────────────────
n_antes_review <- nrow(restaurants_base)

restaurants_base <- restaurants_base %>%
  filter(review_count >= 10)

n_después_review <- nrow(restaurants_base)
message("   ✅ OPERACIÓN 2a (filter - review_count): ",
        n_antes_review, " → ", n_después_review,
        " restaurantes (eliminados: ", n_antes_review - n_después_review, ")")

# ────────────────────────────────────────────────────────────────────────────
# OPERACIÓN 2b: filter() - excluir categoría "Others"
# Por qué: Facilitar análisis comparativo entre cocinas identificadas
# ────────────────────────────────────────────────────────────────────────────
n_antes_others <- nrow(restaurants_base)

restaurants_base <- restaurants_base %>%
  filter(cuisine != "Others")

n_después_others <- nrow(restaurants_base)
message("   ✅ OPERACIÓN 2b (filter - cuisine != Others): ",
        n_antes_others, " → ", n_después_others,
        " restaurantes (eliminados: ", n_antes_others - n_después_others, ")")

# ============================================================================
# SECCIÓN 4: mutate()
# ============================================================================
message("\n🔨 Aplicando mutate()...")

# ────────────────────────────────────────────────────────────────────────────
# OPERACIÓN 3: mutate()
# Por qué: Crear variables categóricas para segmentación y análisis
# ────────────────────────────────────────────────────────────────────────────

# Identificar top 3 cuisines por promedio de estrellas
top_3_cuisines <- sentiment_cuisine %>%
  slice(1:3) %>%
  pull(cuisine)

restaurants_base <- restaurants_base %>%
  mutate(
    # Popularidad basada en volumen de reseñas
    popularidad = case_when(
      review_count >= 100 ~ "Alta",
      review_count >= 30 ~ "Media",
      TRUE ~ "Baja"
    ),
    # Flag si la cuisine está en el top 3 mejor valorado
    es_top_cuisine = cuisine %in% top_3_cuisines
  )

message("   ✅ OPERACIÓN 3 (mutate): Creadas columnas 'popularidad' y 'es_top_cuisine'")
message("      Top 3 cuisines: ", paste(top_3_cuisines, collapse = ", "))

# Distribución de popularidad
dist_pop <- restaurants_base %>%
  count(popularidad, name = "n_restaurantes") %>%
  mutate(pct = round(100 * n_restaurantes / sum(n_restaurantes), 1))
message("\n      Distribución de popularidad:")
message("      ", paste(apply(dist_pop, 1, function(x) paste0(x[1], ": ", x[2], " (", x[3], "%)")), collapse = " | "))

# ============================================================================
# SECCIÓN 5: left_join()
# ============================================================================
message("\n🔗 Aplicando left_join()...")

# ────────────────────────────────────────────────────────────────────────────
# OPERACIÓN 4: left_join()
# Por qué: Agregar contexto de sentimiento del neighborhood a cada restaurante
#         Permite comparar desempeño individual vs promedio zonal
# ────────────────────────────────────────────────────────────────────────────

restaurants_enriquecido <- restaurants_base %>%
  left_join(
    sentiment_neighborhood %>% select(neighborhood, avg_star, pct_positive),
    by = "neighborhood",
    suffix = c("", "_neighborhood")
  ) %>%
  rename(
    avg_star_neighborhood = avg_star,
    pct_positive_neighborhood = pct_positive
  )

message("   ✅ OPERACIÓN 4 (left_join): Unido con sentimiento por neighborhood")
message("\n      Primeras filas después del join:")
glimpse(restaurants_enriquecido, width = 80)

# ============================================================================
# SECCIÓN 6: group_by() + summarize()
# ============================================================================
message("\n📊 Aplicando group_by() + summarize()...")

# ────────────────────────────────────────────────────────────────────────────
# OPERACIÓN 5a: group_by() + summarize() - por neighborhood
# Por qué: Entender características agregadas de restaurantes por zona
# ────────────────────────────────────────────────────────────────────────────

resumen_neighborhood <- restaurants_enriquecido %>%
  group_by(neighborhood) %>%
  summarise(
    n_restaurantes = n(),
    review_count_promedio = round(mean(review_count, na.rm = TRUE), 1),
    pct_popularidad_alta = round(100 * sum(popularidad == "Alta") / n(), 1),
    cuisine_mas_frecuente = names(sort(table(cuisine), decreasing = TRUE))[1],
    .groups = "drop"
  ) %>%
  arrange(desc(n_restaurantes))

message("   ✅ OPERACIÓN 5a (group_by + summarize - neighborhood):")
message("\n--- RESUMEN POR NEIGHBORHOOD ---\n")
print(resumen_neighborhood, n = Inf)

# ────────────────────────────────────────────────────────────────────────────
# OPERACIÓN 5b: group_by() + summarize() - por cuisine
# Por qué: Entender cobertura y características de cada tipo de cocina
# ────────────────────────────────────────────────────────────────────────────

resumen_cuisine <- restaurants_enriquecido %>%
  group_by(cuisine) %>%
  summarise(
    n_restaurantes = n(),
    n_neighborhoods_distintos = n_distinct(neighborhood),
    review_count_promedio = round(mean(review_count, na.rm = TRUE), 1),
    .groups = "drop"
  ) %>%
  arrange(desc(n_restaurantes))

message("   ✅ OPERACIÓN 5b (group_by + summarize - cuisine):")
message("\n--- RESUMEN POR CUISINE ---\n")
print(resumen_cuisine, n = Inf)

# ============================================================================
# SECCIÓN 7: st_drop_geometry()
# ============================================================================
message("\n📍 Aplicando st_drop_geometry()...")

# ────────────────────────────────────────────────────────────────────────────
# OPERACIÓN 6: st_drop_geometry()
# Por qué: Convertir objeto sf a tibble plano para operaciones de atributos
#         Elimina overhead de geometría cuando solo usamos latitud/longitud
# ────────────────────────────────────────────────────────────────────────────

restaurants_sf_plano <- restaurants_sf %>%
  st_drop_geometry()

# Calcular centroide aproximado por neighborhood
centroides_neighborhood <- restaurants_sf_plano %>%
  group_by(neighborhood) %>%
  summarise(
    n_restaurantes = n(),
    centroide_lat = round(mean(latitude, na.rm = TRUE), 4),
    centroide_lon = round(mean(longitude, na.rm = TRUE), 4),
    .groups = "drop"
  ) %>%
  arrange(neighborhood)

message("   ✅ OPERACIÓN 6 (st_drop_geometry): Convertido sf a tibble")
message("      Calculados centroides aproximados por neighborhood:")
message("\n--- CENTROIDES POR NEIGHBORHOOD ---\n")
print(centroides_neighborhood, n = Inf)

# ============================================================================
# SECCIÓN 8: EXPORTAR RESULTADOS
# ============================================================================
message("\n💾 Exportando resultados...")

saveRDS(restaurants_enriquecido, "outputs/restaurants_enriquecido.rds")
message("   ✅ restaurants_enriquecido.rds (", nrow(restaurants_enriquecido), " filas)")

saveRDS(resumen_neighborhood, "outputs/resumen_neighborhood.rds")
message("   ✅ resumen_neighborhood.rds (", nrow(resumen_neighborhood), " filas)")

saveRDS(centroides_neighborhood, "outputs/centroides_neighborhood.rds")
message("   ✅ centroides_neighborhood.rds (", nrow(centroides_neighborhood), " filas)")

cat("\n✨ Script 04_attribute_ops.R completado exitosamente\n\n")

