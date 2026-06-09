# ============================================================================
# Proyecto: Análisis espacial de restaurantes en Las Vegas - Yelp
# Script: 03_sentiment_analysis.R
# Objetivo: Análisis de sentimiento por neighborhood y cuisine
# Input:  outputs/reviews_lv.rds
# Output: outputs/sentiment_neighborhood.rds
#         outputs/sentiment_cuisine.rds
#         outputs/sentiment_neighborhood_cuisine.rds
#         outputs/sentiment_summary.rds
# Autora: Análisis de datos
# Fecha: 13 de abril de 2026
# ============================================================================

# ============================================================================
# SECCIÓN 1: CARGAR LIBRERÍAS
# ============================================================================
library(tidyverse)
library(tidytext)

# ============================================================================
# SECCIÓN 2: CARGAR DATOS
# ============================================================================

# Leer datos de reseñas por neighborhood
reviews_lv <- readRDS("outputs/reviews_lv.rds")

# Cargamos restaurants_lv para traer la columna cuisine
restaurants_lv <- readRDS("outputs/restaurants_lv.rds")

# Unimos cuisine al dataset de reviews por business_id
# Necesitamos cuisine para el análisis por tipo de cocina
reviews_lv <- reviews_lv %>%
  left_join(
    restaurants_lv %>% select(business_id, cuisine),
    by = "business_id"
  )

# Verificar dimensiones
message("\n📊 DATOS CARGADOS:")
message("Filas: ", nrow(reviews_lv), " | Columnas: ", ncol(reviews_lv))

# Verificar distribución de sentimiento
sentiment_dist <- reviews_lv %>%
  count(sentiment, name = "n_reseñas") %>%
  mutate(porcentaje = round(100 * n_reseñas / sum(n_reseñas), 2))

message("\n📈 Distribución de sentimientos:")
print(sentiment_dist)

# ============================================================================
# SECCIÓN 3: SENTIMIENTO POR NEIGHBORHOOD
# ============================================================================
message("\n🔍 Calculando sentimiento por neighborhood...")

sentiment_neighborhood <- reviews_lv %>%
  # Agrupar por neighborhood
  group_by(neighborhood) %>%
  
  # Calcular métricas de sentimiento
  summarise(
    n_reviews = n(),
    n_restaurantes = n_distinct(business_id),
    pct_positive = round(100 * sum(sentiment == "positive") / n(), 2),
    pct_negative = round(100 * sum(sentiment == "negative") / n(), 2),
    pct_neutral = round(100 * sum(sentiment == "neutral") / n(), 2),
    avg_star = round(mean(review_star, na.rm = TRUE), 2),
    .groups = "drop"
  ) %>%
  
  # Ordenar por promedio de estrellas descendente
  arrange(desc(avg_star))

message("✅ Sentimiento por neighborhood calculado")
message("\n--- RESULTADOS POR NEIGHBORHOOD ---\n")
print(sentiment_neighborhood, n = Inf)

# ============================================================================
# SECCIÓN 4: SENTIMIENTO POR CUISINE
# ============================================================================
message("\n🔍 Calculando sentimiento por cuisine...")

sentiment_cuisine <- reviews_lv %>%
  # Agrupar por cuisine
  group_by(cuisine) %>%
  
  # Calcular métricas de sentimiento
  summarise(
    n_reviews = n(),
    n_restaurantes = n_distinct(business_id),
    pct_positive = round(100 * sum(sentiment == "positive") / n(), 2),
    pct_negative = round(100 * sum(sentiment == "negative") / n(), 2),
    pct_neutral = round(100 * sum(sentiment == "neutral") / n(), 2),
    avg_star = round(mean(review_star, na.rm = TRUE), 2),
    .groups = "drop"
  ) %>%
  
  # Filtrar cuisines con al menos 500 reviews para evitar sesgos
  filter(n_reviews >= 500) %>%
  
  # Ordenar por promedio de estrellas descendente
  arrange(desc(avg_star))

message("✅ Sentimiento por cuisine calculado (solo cuisines con ≥500 reviews)")
message("\n--- RESULTADOS POR CUISINE ---\n")
print(sentiment_cuisine, n = Inf)

# ============================================================================
# SECCIÓN 5: SENTIMIENTO POR NEIGHBORHOOD Y CUISINE (COMBINADOS)
# ============================================================================
message("\n🔍 Calculando sentimiento por neighborhood + cuisine...")

sentiment_neighborhood_cuisine <- reviews_lv %>%
  # Agrupar por neighborhood y cuisine
  group_by(neighborhood, cuisine) %>%
  
  # Calcular métricas
  summarise(
    n_reviews = n(),
    n_restaurantes = n_distinct(business_id),
    avg_star = round(mean(review_star, na.rm = TRUE), 2),
    .groups = "drop"
  ) %>%
  
  # Filtrar combinaciones con al menos 100 reviews
  filter(n_reviews >= 100) %>%
  
  # Ordenar por promedio de estrellas descendente
  arrange(desc(avg_star))

message("✅ Sentimiento por neighborhood + cuisine calculado (solo combos con ≥100 reviews)")
message("\n--- RESULTADOS POR NEIGHBORHOOD + CUISINE (primeras 30 filas) ---\n")
print(sentiment_neighborhood_cuisine, n = 30)

# ============================================================================
# SECCIÓN 6: RESUMEN EJECUTIVO
# ============================================================================
message("\n🔍 Creando resumen ejecutivo...")

# Neighborhood con mayor avg_star
best_neighborhood <- sentiment_neighborhood %>%
  slice(1) %>%
  select(neighborhood, avg_star)

worst_neighborhood <- sentiment_neighborhood %>%
  slice(n()) %>%
  select(neighborhood, avg_star)

# Cuisine con mayor avg_star
best_cuisine <- sentiment_cuisine %>%
  slice(1) %>%
  select(cuisine, avg_star)

worst_cuisine <- sentiment_cuisine %>%
  slice(n()) %>%
  select(cuisine, avg_star)

# Combinación neighborhood + cuisine con mayor avg_star
best_combo <- sentiment_neighborhood_cuisine %>%
  slice(1) %>%
  select(neighborhood, cuisine, avg_star, n_reviews)

# Total de reseñas
total_reviews <- nrow(reviews_lv)

# Crear objeto de resumen
sentiment_summary <- list(
  best_neighborhood = as.list(best_neighborhood),
  worst_neighborhood = as.list(worst_neighborhood),
  best_cuisine = as.list(best_cuisine),
  worst_cuisine = as.list(worst_cuisine),
  best_combo = as.list(best_combo),
  total_reviews = total_reviews
)

message("✅ Resumen ejecutivo creado")

# Imprimir de forma clara
message("\n╔════════════════════════════════════════════════════════════════╗")
message("║             📊 RESUMEN EJECUTIVO - ANÁLISIS SENTIMIENTO        ║")
message("╚════════════════════════════════════════════════════════════════╝")

message("\n🏆 MEJORES RESULTADOS:")
message("   • Neighborhood con mayor rating: ", 
        best_neighborhood$neighborhood, 
        " (⭐ ", best_neighborhood$avg_star, ")")
message("   • Cuisine con mayor rating: ", 
        best_cuisine$cuisine, 
        " (⭐ ", best_cuisine$avg_star, ")")
message("   • Best combo (neighborhood + cuisine): ",
        best_combo$neighborhood, " + ", best_combo$cuisine,
        " (⭐ ", best_combo$avg_star, " | ", best_combo$n_reviews, " reviews)")

message("\n📉 PEORES RESULTADOS:")
message("   • Neighborhood con menor rating: ", 
        worst_neighborhood$neighborhood, 
        " (⭐ ", worst_neighborhood$avg_star, ")")
message("   • Cuisine con menor rating: ", 
        worst_cuisine$cuisine, 
        " (⭐ ", worst_cuisine$avg_star, ")")

message("\n📈 TOTALES:")
message("   • Total de reseñas analizadas: ", total_reviews)
message("   • Neighborhoods únicos: ", n_distinct(reviews_lv$neighborhood))
message("   • Cuisines analizadas (≥500 reviews): ", nrow(sentiment_cuisine))
message("   • Combinaciones analizadas (≥100 reviews): ", 
        nrow(sentiment_neighborhood_cuisine))

message("\n")

# ============================================================================
# SECCIÓN 7: EXPORTAR RESULTADOS
# ============================================================================
message("💾 Exportando resultados...")

saveRDS(sentiment_neighborhood, "outputs/sentiment_neighborhood.rds")
message("   ✅ sentiment_neighborhood.rds")

saveRDS(sentiment_cuisine, "outputs/sentiment_cuisine.rds")
message("   ✅ sentiment_cuisine.rds")

saveRDS(sentiment_neighborhood_cuisine, "outputs/sentiment_neighborhood_cuisine.rds")
message("   ✅ sentiment_neighborhood_cuisine.rds")

saveRDS(sentiment_summary, "outputs/sentiment_summary.rds")
message("   ✅ sentiment_summary.rds")

cat("\n✨ Script completado exitosamente\n\n")

