# Asegurar que el working directory sea la raíz del proyecto
setwd(dirname(dirname(rstudioapi::getActiveDocumentContext()$path)))

# ============================================================================
# Proyecto: Análisis espacial de restaurantes en Las Vegas - Yelp
# Script: 05_app.R
# Objetivo: Shiny app interactiva integrando todos los análisis del proyecto
# Input:  outputs/restaurants_lv.rds
#         outputs/restaurants_sf.rds
#         outputs/tracts_lv.rds
#         outputs/sentiment_neighborhood.rds
#         outputs/sentiment_cuisine.rds
#         outputs/sentiment_neighborhood_cuisine.rds
#         outputs/restaurants_enriquecido.rds
#         outputs/resumen_neighborhood.rds
#         outputs/centroides_neighborhood.rds
# Output: App Shiny interactiva (puerto 3838 por defecto)
# Autora: Análisis de datos
# Fecha: 13 de abril de 2026
# ============================================================================

# ============================================================================
# INSTALACIÓN DE PAQUETES (si es necesario)
# ============================================================================
message("⏳ Verificando paquetes requeridos...")

packages_required <- c(
  "shiny",
  "tidyverse",
  "sf",
  "leaflet",
  "plotly",
  "DT",
  "bslib"
)

packages_to_install <- packages_required[
  !packages_required %in% installed.packages()[, "Package"]
]

if (length(packages_to_install) > 0) {
  message("📦 Instalando paquetes faltantes: ",
          paste(packages_to_install, collapse = ", "))
  install.packages(packages_to_install, dependencies = TRUE)
  message("✅ Paquetes instalados correctamente")
} else {
  message("✅ Todos los paquetes ya están instalados")
}

# ============================================================================
# CARGAR LIBRERÍAS
# ============================================================================
library(shiny)
library(tidyverse)
library(sf)
library(leaflet)
library(plotly)
library(DT)

# ============================================================================
# CARGAR DATOS (fuera del server para eficiencia)
# ============================================================================
message("📂 Iniciando carga de datos...")

restaurants_lv <- readRDS("outputs/restaurants_lv.rds")
restaurants_sf <- readRDS("outputs/restaurants_sf.rds")
tracts_lv <- readRDS("outputs/tracts_lv.rds")
sentiment_neighborhood <- readRDS("outputs/sentiment_neighborhood.rds")
sentiment_cuisine <- readRDS("outputs/sentiment_cuisine.rds")
sentiment_neighborhood_cuisine <- readRDS("outputs/sentiment_neighborhood_cuisine.rds")
restaurants_enriquecido <- readRDS("outputs/restaurants_enriquecido.rds")
resumen_neighborhood <- readRDS("outputs/resumen_neighborhood.rds")
centroides_neighborhood <- readRDS("outputs/centroides_neighborhood.rds")

message("✅ Datos cargados exitosamente")

# ============================================================================
# PREPARACIÓN DE DATOS PARA MAPAS
# ============================================================================

# Asignar neighborhood a cada tract usando el centroide más cercano
message("🔧 Asignando neighborhoods a tracts...")
tracts_con_neighborhood <- tracts_lv %>%
  st_join(
    centroides_neighborhood %>% st_as_sf(coords = c("centroide_lon", "centroide_lat"), crs = 4326),
    join = st_nearest_feature
  ) %>%
  left_join(
    sentiment_neighborhood,
    by = "neighborhood"
  )

message("✅ Preparación completada")

# ============================================================================
# FUNCIÓN AUXILIAR: Paleta de colores para popularidad
# ============================================================================
color_popularidad <- function(pop) {
  case_when(
    pop == "Alta" ~ "#2ecc71",    # Verde
    pop == "Media" ~ "#f39c12",   # Naranja
    pop == "Baja" ~ "#e74c3c",    # Rojo
    TRUE ~ "#95a5a6"              # Gris default
  )
}

# ============================================================================
# UI - INTERFAZ DE USUARIO
# ============================================================================

ui <- navbarPage(
  # Título principal
  title = "🍽️ Restaurantes en Las Vegas · Análisis Espacial Yelp",
  theme = bslib::bs_theme(version = 5, bootswatch = "flatly"),
  
  # ────────────────────────────────────────────────────────────────────────
  # TAB 1: MAPA DE CALIFICACIONES POR ZONA
  # ────────────────────────────────────────────────────────────────────────
  tabPanel(
    "Calificaciones por Zona",
    
    # Descripción
    div(
      class = "well",
      h4("📊 Calificaciones promedio por neighborhood"),
      p("Este mapa muestra el promedio de estrellas de las reseñas en cada zona de Las Vegas. ",
        "El color indica la calificación: rojo (baja) a verde (alta). ",
        "Pasa el cursor sobre cada zona para ver el detalle.")
    ),
    
    # Mapa
    leafletOutput("mapa_calificaciones", height = "600px")
  ),
  
  # ────────────────────────────────────────────────────────────────────────
  # TAB 2: SENTIMIENTO POR TIPO DE COCINA
  # ────────────────────────────────────────────────────────────────────────
  tabPanel(
    "Cocinas de Las Vegas",
    
    # Descripción
    div(
      class = "well",
      h4("🍜 Ranking de cocinas por calificación"),
      p("Comparación de las cocinas principales en Las Vegas ordenadas por promedio de estrellas. ",
        "El color de las barras indica el porcentaje de reseñas positivas.")
    ),
    
    # Gráfico
    plotlyOutput("grafico_cocinas", height = "500px"),
    
    # Tabla
    div(
      class = "well",
      h5("Datos detallados por cuisine:"),
      DTOutput("tabla_cocinas")
    )
  ),
  
  # ────────────────────────────────────────────────────────────────────────
  # TAB 3: ¿QUÉ COCINA FUNCIONA EN CADA ZONA?
  # ────────────────────────────────────────────────────────────────────────
  tabPanel(
    "Cocina por Zona",
    
    # Descripción
    div(
      class = "well",
      h4("🗺️ Heatmap: Cocina × Neighborhood"),
      p("Mapa de calor mostrando qué tipos de cocina funcionan mejor en cada zona de Las Vegas. ",
        "Verde indica mejor calificación, rojo indica peor. ",
        "Solo se muestran combinaciones con suficientes reseñas.")
    ),
    
    # Sidebar con controles
    sidebarLayout(
      sidebarPanel(
        width = 3,
        h5("Filtros:"),
        sliderInput(
          "min_reviews_heatmap",
          label = "Mínimo de reseñas en la combinación:",
          min = 100,
          max = 500,
          value = 100,
          step = 50
        )
      ),
      
      mainPanel(
        width = 9,
        plotlyOutput("heatmap_cocina_zona", height = "600px")
      )
    )
  ),
  
  # ────────────────────────────────────────────────────────────────────────
  # TAB 4: MAPA DE RESTAURANTES
  # ────────────────────────────────────────────────────────────────────────
  tabPanel(
    "Restaurantes en el Mapa",
    
    # Descripción
    div(
      class = "well",
      h4("📍 Ubicación de restaurantes en Las Vegas"),
      p("Mapa interactivo con todos los restaurantes. ",
        "Verde = muy populares, Naranja = popularidad media, Rojo = menos reseñas. ",
        "Filtra por neighborhood o tipo de cocina.")
    ),
    
    # Sidebar con controles
    sidebarLayout(
      sidebarPanel(
        width = 3,
        h5("Filtros:"),
        selectInput(
          "filtro_neighborhood_mapa",
          label = "Neighborhood:",
          choices = c("Todos", unique(sort(restaurants_enriquecido$neighborhood)))
        ),
        selectInput(
          "filtro_cuisine_mapa",
          label = "Cuisine:",
          choices = c("Todos", unique(sort(restaurants_enriquecido$cuisine)))
        )
      ),
      
      mainPanel(
        width = 9,
        leafletOutput("mapa_restaurantes", height = "600px")
      )
    )
  ),
  
  # ────────────────────────────────────────────────────────────────────────
  # TAB 5: EXPLORADOR DE RESTAURANTES
  # ────────────────────────────────────────────────────────────────────────
  tabPanel(
    "Explorador",
    
    # Descripción
    div(
      class = "well",
      h4("🔍 Explorador interactivo de restaurantes"),
      p("Tabla completa de restaurantes con búsqueda avanzada. ",
        "Filtra por neighborhood, tipo de cocina o nivel de popularidad. ",
        "Haz clic en los encabezados para ordenar.")
    ),
    
    # Sidebar con controles
    sidebarLayout(
      sidebarPanel(
        width = 3,
        h5("Filtros:"),
        selectInput(
          "filtro_neighborhood_tabla",
          label = "Neighborhood:",
          choices = c("Todos", unique(sort(restaurants_enriquecido$neighborhood)))
        ),
        selectInput(
          "filtro_cuisine_tabla",
          label = "Cuisine:",
          choices = c("Todos", unique(sort(restaurants_enriquecido$cuisine)))
        ),
        selectInput(
          "filtro_popularidad_tabla",
          label = "Popularidad:",
          choices = c("Todos", "Alta", "Media", "Baja")
        ),
        br(),
        actionButton(
          "limpiar_filtros",
          label = "🔄 Limpiar filtros",
          class = "btn btn-primary"
        )
      ),
      
      mainPanel(
        width = 9,
        DTOutput("tabla_restaurantes")
      )
    )
  )
)

# ============================================================================
# SERVER - LÓGICA DE LA APP
# ============================================================================

server <- function(input, output, session) {
  
  # ==========================================================================
  # REACTIVIDAD: Actualizar selectores de filtros
  # ==========================================================================
  
  # Resetear filtros cuando se hace clic en "Limpiar filtros"
  observeEvent(input$limpiar_filtros, {
    updateSelectInput(session, "filtro_neighborhood_tabla", selected = "Todos")
    updateSelectInput(session, "filtro_cuisine_tabla", selected = "Todos")
    updateSelectInput(session, "filtro_popularidad_tabla", selected = "Todos")
  })
  
  # ==========================================================================
  # TAB 1: MAPA DE CALIFICACIONES POR ZONA
  # ==========================================================================
  
  output$mapa_calificaciones <- renderLeaflet({
    # Paleta de colores numérica para avg_star
    pal_calificaciones <- colorNumeric(
      palette = c("#e74c3c", "#f39c12", "#2ecc71"),  # Rojo -> Naranja -> Verde
      domain = c(3, 5),
      na.color = "#cccccc"
    )
    
    # Crear mapa base
    leaflet(tracts_con_neighborhood) %>%
      addProviderTiles("CartoDB.Positron") %>%
      
      # Agregar polígonos coloreados por avg_star
      addPolygons(
        fillColor = ~pal_calificaciones(avg_star),
        fillOpacity = 0.7,
        color = "white",
        weight = 1,
        label = ~paste0(
          neighborhood, " | ⭐ ", avg_star,
          " | Positivas: ", pct_positive, "%"
        ),
        labelOptions = labelOptions(
          style = list("font-weight" = "bold"),
          textsize = "12px"
        )
      ) %>%
      
      # Agregar leyenda
      addLegend(
        pal = pal_calificaciones,
        values = ~avg_star,
        opacity = 0.7,
        title = "Promedio<br/>estrellas",
        position = "bottomright"
      ) %>%
      
      # Ajustar vista
      setView(lng = -115.13, lat = 36.16, zoom = 11)
  })
  
  # ==========================================================================
  # TAB 2: SENTIMIENTO POR TIPO DE COCINA
  # ==========================================================================
  
  output$grafico_cocinas <- renderPlotly({
    p <- ggplot(sentiment_cuisine,
                aes(x = avg_star,
                    y = reorder(cuisine, avg_star),
                    fill = pct_positive,
                    text = paste0(
                      "Cocina: ", cuisine,
                      "<br>Promedio: ", avg_star, " ⭐",
                      "<br>% Positivas: ", pct_positive, "%",
                      "<br>Reseñas: ", n_reviews
                    ))) +
      geom_col() +
      scale_fill_gradient(low = "#a8d5e2", high = "#1a6985",
                          name = "% Positivas") +
      labs(x = "Promedio de estrellas", y = NULL) +
      theme_minimal()

    ggplotly(p, tooltip = "text")
  })
  
  output$tabla_cocinas <- renderDT({
    datatable(
      sentiment_cuisine %>%
        arrange(desc(avg_star)) %>%
        select(
          cuisine,
          n_reviews,
          n_restaurantes,
          avg_star,
          pct_positive,
          pct_negative,
          pct_neutral
        ),
      colnames = c(
        "Cuisine", "Reseñas", "Restaurantes",
        "Promedio ⭐", "% Positivas", "% Negativas", "% Neutrales"
      ),
      options = list(
        pageLength = 15,
        lengthChange = FALSE,
        searching = FALSE,
        ordering = TRUE
      ),
      rownames = FALSE
    ) %>%
      formatRound(c("avg_star", "pct_positive", "pct_negative", "pct_neutral"), 2)
  })
  
  # ==========================================================================
  # TAB 3: HEATMAP COCINA × ZONA
  # ==========================================================================
  
  output$heatmap_cocina_zona <- renderPlotly({
    # Filtrar por mínimo de reviews
    datos_heatmap <- sentiment_neighborhood_cuisine %>%
      filter(n_reviews >= input$min_reviews_heatmap) %>%
      arrange(desc(avg_star))
    
    # Crear heatmap con plotly
    plot_ly(
      datos_heatmap,
      x = ~neighborhood,
      y = ~cuisine,
      z = ~avg_star,
      type = "heatmap",
      colorscale = list(
        list(0, "#e74c3c"),    # Rojo
        list(0.5, "#f39c12"),  # Naranja
        list(1, "#2ecc71")     # Verde
      ),
      colorbar = list(title = "Avg Star"),
      hovertemplate = paste0(
        "<b>%{y}</b> en <b>%{x}</b><br>",
        "⭐ %{z:.2f}<br>",
        "Reseñas: ",  # Se agregará con customdata
        "<extra></extra>"
      ),
      customdata = datos_heatmap$n_reviews
    ) %>%
      layout(
        title = "Calificación promedio: Cocina × Neighborhood",
        xaxis = list(title = "Neighborhood", tickangle = -45),
        yaxis = list(title = "Cuisine"),
        height = 600,
        margin = list(b = 100)
      )
  })
  
  # ==========================================================================
  # TAB 4: MAPA DE RESTAURANTES
  # ==========================================================================
  
  # Filtrar datos según selectores
  datos_mapa_restaurantes <- reactive({
    datos <- restaurants_enriquecido
    
    # Filtro neighborhood
    if (input$filtro_neighborhood_mapa != "Todos") {
      datos <- datos %>% filter(neighborhood == input$filtro_neighborhood_mapa)
    }
    
    # Filtro cuisine
    if (input$filtro_cuisine_mapa != "Todos") {
      datos <- datos %>% filter(cuisine == input$filtro_cuisine_mapa)
    }
    
    datos
  })
  
  output$mapa_restaurantes <- renderLeaflet({
    datos <- datos_mapa_restaurantes()
    
    # Convertir a sf si no lo es
    if (!inherits(datos, "sf")) {
      datos_sf <- st_as_sf(
        datos,
        coords = c("longitude", "latitude"),
        crs = 4326
      )
    } else {
      datos_sf <- datos
    }
    
    # Crear mapa
    leaflet(datos_sf) %>%
      addProviderTiles("CartoDB.Positron") %>%
      
      # Agregar circles con color según popularidad
      addCircleMarkers(
        radius = 6,
        color = ~color_popularidad(popularidad),
        fillOpacity = 0.7,
        stroke = TRUE,
        weight = 1,
        opacity = 1,
        popup = ~paste0(
          "<b>", name, "</b><br>",
          neighborhood, " | ", cuisine, "<br>",
          "Reseñas: ", review_count, "<br>",
          "Popularidad: ", popularidad
        ),
        label = ~name
      ) %>%
      
      setView(lng = -115.13, lat = 36.16, zoom = 11)
  })
  
  # ==========================================================================
  # TAB 5: EXPLORADOR DE RESTAURANTES
  # ==========================================================================
  
  # Filtrar datos según selectores
  datos_tabla_restaurantes <- reactive({
    datos <- restaurants_enriquecido
    
    # Filtro neighborhood
    if (input$filtro_neighborhood_tabla != "Todos") {
      datos <- datos %>% filter(neighborhood == input$filtro_neighborhood_tabla)
    }
    
    # Filtro cuisine
    if (input$filtro_cuisine_tabla != "Todos") {
      datos <- datos %>% filter(cuisine == input$filtro_cuisine_tabla)
    }
    
    # Filtro popularidad
    if (input$filtro_popularidad_tabla != "Todos") {
      datos <- datos %>% filter(popularidad == input$filtro_popularidad_tabla)
    }
    
    datos
  })
  
  output$tabla_restaurantes <- renderDT({
    datos <- datos_tabla_restaurantes() %>%
      select(
        name,
        neighborhood,
        cuisine,
        review_count,
        popularidad,
        avg_star_neighborhood
      ) %>%
      arrange(desc(review_count))
    
    datatable(
      datos,
      colnames = c(
        "Restaurante", "Neighborhood", "Cuisine",
        "Reseñas", "Popularidad", "Rating Zona"
      ),
      options = list(
        pageLength = 15,
        lengthChange = TRUE,
        searching = TRUE,
        ordering = TRUE,
        autoWidth = TRUE
      ),
      rownames = FALSE
    ) %>%
      formatRound("avg_star_neighborhood", 2)
  })
  
}

# ============================================================================
# EJECUTAR APP
# ============================================================================

shinyApp(ui, server)
