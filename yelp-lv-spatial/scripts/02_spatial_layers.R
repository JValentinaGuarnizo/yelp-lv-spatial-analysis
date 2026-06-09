# =============================================================================
# Proyecto: Análisis espacial de restaurantes en Las Vegas - Yelp
# Script:   02_spatial_layers.R
# Objetivo: Convertir restaurantes a objetos sf, descargar Census tracts
#           de Clark County (tigris), y hacer el join espacial punto-en-polígono.

# Input:    outputs/restaurants_lv.rds
# Output:   outputs/restaurants_sf.rds  → puntos sf con tract asignado
#           outputs/tracts_lv.rds       → polígonos de Census tracts


# =============================================================================
# SECCIÓN 1 — Librerías
# =============================================================================

library(sf)        # Objetos y operaciones espaciales
library(tidyverse) # Manipulación de datos
library(tigris)    # Descarga de capas geográficas del Census Bureau

options(tigris_use_cache = TRUE)


# =============================================================================
# SECCIÓN 2 — Cargar datos
# =============================================================================

restaurants_lv <- readRDS("outputs/restaurants_lv.rds")

message("Restaurantes cargados: ", nrow(restaurants_lv), " filas | ",
        n_distinct(restaurants_lv$neighborhood), " neighborhoods distintos")


# =============================================================================
# SECCIÓN 3 — Convertir restaurantes a objeto sf
# =============================================================================

# CRS 4326 = WGS84: coordenadas geográficas en grados decimales.
# remove = FALSE conserva las columnas longitude/latitude originales.

restaurants_sf <- st_as_sf(
  restaurants_lv,
  coords = c("longitude", "latitude"),
  crs    = 4326,
  remove = FALSE
)

view(restaurants_sf)


# =============================================================================
# SECCIÓN 4 — Descargar Census tracts de Clark County, NV
# =============================================================================

# year = 2016 alineado con reviews 2015–2017
# cb = TRUE → versión cartográfica simplificada, suficiente para joins y mapas

message("Descargando Census tracts de Clark County, NV (año 2016)...")

tracts_lv <- tryCatch(
  tigris::tracts(state = "NV", county = "Clark", year = 2016, cb = TRUE), #API descargar poligonos
  error = function(e) {
    message("ERROR al descargar Census tracts: ", conditionMessage(e))
    NULL
  }
)

if (is.null(tracts_lv)) {
  stop("No se pudieron descargar los Census tracts. Verifica la conexión a internet.")
}

message("Tracts descargados: ", nrow(tracts_lv), " polígonos")

# Reproyectamos a 4326 para coincidir con los puntos (tigris devuelve en 4269)
tracts_lv <- tracts_lv %>%
  st_transform(crs = 4326) %>%
  select(GEOID, NAME)


# =============================================================================
# SECCIÓN 5 — Join espacial punto-en-polígono
# =============================================================================

# st_within: el punto debe estar completamente dentro del polígono.
# Más estricto que st_intersects — evita asignar un restaurante a dos tracts
# si cae exactamente en la frontera.

message("Ejecutando join espacial st_within...")

restaurants_sf <- st_join(
  restaurants_sf,
  tracts_lv %>% select(tract_geoid = GEOID, tract_name = NAME),
  join = st_within
)

n_con_tract <- sum(!is.na(restaurants_sf$tract_geoid))
n_sin_tract <- sum(is.na(restaurants_sf$tract_geoid))

message("Restaurantes con tract asignado: ", n_con_tract)
message("Restaurantes sin tract asignado: ", n_sin_tract)


# =============================================================================
# SECCIÓN 6 — Verificación
# =============================================================================

# Por cada neighborhood: cuántos restaurantes y cuántos tracts distintos los contienen.
# Si n_tracts_distintos > 1, confirma que los tracts son más granulares que los neighborhoods.

resumen_join <- restaurants_sf %>%
  st_drop_geometry() %>%
  filter(!is.na(tract_geoid)) %>%
  group_by(neighborhood) %>%
  summarise(
    n_restaurantes     = n(),
    n_tracts_distintos = n_distinct(tract_geoid),
    .groups = "drop"
  ) %>%
  arrange(desc(n_restaurantes))

message("=== Neighborhoods vs tracts ===")
print(resumen_join)

message("Tracts distintos por neighborhood (promedio): ",
        round(mean(resumen_join$n_tracts_distintos), 1))


# =============================================================================
# SECCIÓN 7 — Exportar
# =============================================================================

dir.create("outputs", showWarnings = FALSE)

saveRDS(restaurants_sf, "outputs/restaurants_sf.rds")
saveRDS(tracts_lv,      "outputs/tracts_lv.rds")

cat("✓ restaurants_sf:", nrow(restaurants_sf), "restaurantes (sf, puntos)\n")
cat("✓ tracts_lv:     ", nrow(tracts_lv),      "Census tracts (sf, polígonos)\n")
