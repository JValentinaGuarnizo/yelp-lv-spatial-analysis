# =============================================================================
# Proyecto: Análisis espacial de restaurantes en Las Vegas - Yelp
# Script:   01_data_prep.R
# Objetivo: Carga, limpieza y preparación de los datos base del proyecto.

# Input:    data/yelp.rds
#           data/Restaurants_new.csv
#           data/Restaurants_review.rds

# Output:   outputs/restaurants_lv.rds  → restaurantes de Las Vegas
#           outputs/reviews_lv.rds      → reseñas de Las Vegas (2015–2017)


# =============================================================================
# SECCIÓN 1 — Librerías
# =============================================================================

library(tidyverse)   # Manipulación de datos (dplyr, tidyr, readr, ggplot2...)
library(stringr)     # Operaciones con texto y expresiones regulares


# =============================================================================
# SECCIÓN 2 — Carga de datos
# =============================================================================
message("=== yelp.rds ===")
yelp_raw              <- readRDS("data/yelp.rds")
view(yelp_raw)

message("=== Restaurants_new.csv ===")
restaurants_raw       <- read.csv("data/Restaurants_new.csv",
                                  stringsAsFactors = FALSE)
view(restaurants_raw)

message("=== Restaurants_review.rds ===")
reviews_raw           <- readRDS("data/Restaurants_review.rds")
view(reviews_raw)



# =============================================================================
# SECCIÓN 3 — Limpieza y filtrado de restaurants_lv
# =============================================================================

# Partimos de Restaurants_new.csv, que contiene restaurantes de varias ciudades.
# El objetivo es quedarnos solo con Las Vegas y enriquecer el dataset con
# columnas derivadas que necesitaremos en scripts posteriores.

# --- 3.1 Tomamos solo Las Vegas ---

restaurants_lv <- restaurants_raw %>%
  
  #1. Filtrar solo restaurantes de Las Vegas
  filter(str_to_upper(city) == "LAS VEGAS") %>%
  { message("Tras filtrar Las Vegas: ", nrow(.), " restaurantes"); . } %>%
  
  #2. Convertir coordenadas a numérico
  mutate(latitude  = as.numeric(latitude), longitude = as.numeric(longitude)) %>%
  
  #3. Eliminar coordenadas inválidas
  filter(is.finite(latitude) & is.finite(longitude)) %>%
  { message("Tras limpiar coordenadas: ", nrow(.), " restaurantes"); . } %>%
  
  #4. Eliminar restaurantes sin neighborhood
  filter(!is.na(neighborhood) & str_trim(neighborhood) != "") %>%
  { message("Tras filtrar sin neighborhood: ", nrow(.), " restaurantes"); . }


# --- 3.2 Columna cuisine ---

# Clasificamos cada restaurante en un tipo de cocina según palabras clave

restaurants_lv <- restaurants_lv %>%
  mutate(
    cuisine = case_when(
      str_detect(categories, regex("american|sandwiches|fast food|burgers",
                                   ignore_case = TRUE)) ~ "American",
      str_detect(categories, regex("italian",       ignore_case = TRUE)) ~ "Italian",
      str_detect(categories, regex("mexican",       ignore_case = TRUE)) ~ "Mexican",
      str_detect(categories, regex("chinese",       ignore_case = TRUE)) ~ "Chinese",
      str_detect(categories, regex("indian",        ignore_case = TRUE)) ~ "Indian",
      str_detect(categories, regex("thai",          ignore_case = TRUE)) ~ "Thai",
      str_detect(categories, regex("korean",        ignore_case = TRUE)) ~ "Korean",
      str_detect(categories, regex("japanese",      ignore_case = TRUE)) ~ "Japanese",
      str_detect(categories, regex("french",        ignore_case = TRUE)) ~ "French",
      str_detect(categories, regex("vietnamese",    ignore_case = TRUE)) ~ "Vietnamese",
      str_detect(categories, regex("mediterranean", ignore_case = TRUE)) ~ "Mediterranean",
      str_detect(categories, regex("hawaiian",      ignore_case = TRUE)) ~ "Hawaiian", TRUE ~ "Others"))


# --- 3.3 Selección de columnas finales ---
restaurants_lv <- restaurants_lv %>%
  select(business_id, name, address, neighborhood,
         city, latitude, longitude, stars,
         review_count, categories, cuisine)

# --- 3.5 Verificación ---

message("=== restaurants_lv: resultado ===")
view(restaurants_lv)

# Distribución por neighborhood: nos dice cuántos restaurantes hay en cada zona
message("=== Restaurantes por neighborhood ===")
print(count(restaurants_lv, neighborhood, sort = TRUE))


# =============================================================================
# SECCIÓN 4 — Limpieza de reviews_lv
# =============================================================================
# Partimos de Restaurants_review.rds (reviews de múltiples ciudades o sin filtro).
# El objetivo es quedarnos con las reviews que corresponden a restaurantes de LV

# --- 4.1 Join con restaurants_lv para filtrar a Las Vegas ---
reviews_lv <- reviews_raw %>%
  inner_join(
    restaurants_lv %>% select(business_id, neighborhood),
    by = "business_id")


view(reviews_lv)


# --- 4.4 Columna review_star como numeric ---
reviews_lv <- reviews_lv %>%
  mutate(review_star = as.numeric(review_star))

# --- 4.5 Columna sentiment ---

reviews_lv <- reviews_lv %>%
  mutate(
    sentiment = case_when(
      review_star >= 4 ~ "positive",
      review_star <= 2 ~ "negative",
      review_star == 3 ~ "neutral",
      TRUE             ~ NA_character_   # Por si hay estrellas fuera de rango
    )
  )

# --- 4.5 Selección de columnas finales ---

reviews_lv <- reviews_lv %>%
  select(review_id, business_id, user_id,
         review_star, sentiment,
         neighborhood, cuisine)

# --- 4.6 Distribución de sentimientos
message("=== Reviews por sentimiento ===")
print(count(reviews_lv, sentiment, sort = TRUE))


# =============================================================================
# SECCIÓN 5 — Exportar
# =============================================================================

dir.create("outputs", showWarnings = FALSE)

saveRDS(restaurants_lv, "outputs/restaurants_lv.rds")
saveRDS(reviews_lv,     "outputs/reviews_lv.rds")

cat("✓ restaurants_lv:", nrow(restaurants_lv), "restaurantes\n")
cat("✓ reviews_lv:",     nrow(reviews_lv),     "reseñas\n")

