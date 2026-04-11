# =========================
# 0) Preparation: data and config
# =========================

# Carga objetos, tablas y funciones auxiliares definidos en dataset.R
source("dataset.R", local = TRUE)
# Carga variables de configuracion (por ejemplo, la clave de Google)
source("config.R")
# Paquete para mapas base y geocodificacion
library(ggmap)
# Registra la clave de Google en ggmap sin escribir en archivos del sistema
register_google(key = GOOGLE_API_KEY, write = FALSE)

# =========================
# 1) App libraries
# =========================

# Framework principal de Shiny
library(shiny)
# Componentes estilo dashboard
library(shinydashboard)
# Tablas interactivas
library(DT)

# =========================
# 2) Sidebar: navigation
# =========================

# Barra lateral con el menu principal
sidebar <- dashboardSidebar(
  # Menu de navegacion con items y sub-items
  sidebarMenu(
    # Pantalla de bienvenida
    menuItem("Navigation", tabName = "nav", icon = icon("dashboard")),
    # Mapa interactivo por ciudad/categoria
    menuItem("Map", icon = icon("map-signs"), tabName = "maps"),
    # Seccion de exploracion con subpestanas
    menuItem(
      "Exploration", icon = icon("microscope"), tabName = "exploration",
      menuSubItem("Data View", tabName = "data_view", icon = icon("table")),
      menuSubItem("City-Wide Visualization", tabName = "city_viz", icon = icon("signal")),
      menuSubItem("Heat Map", tabName = "city_heat", icon = icon("fire"))
    ),
    # Visualizaciones especificas de restaurantes
    menuItem(
      "Restaurant Visualization", tabName = "rest_viz_start", icon = icon("utensils"),
      menuSubItem("General Visualizations", tabName = "rest_viz", icon = icon("glasses")),
      menuSubItem("Restaurant Heat Map", tabName = "rest_heat_map", icon = icon("fire"))
    ),
    # Analisis de texto de reviews
    menuItem("Text Analysis", tabName = "text_analysis", icon = icon("text-width"))
  )
)

# =========================
# 3) Body: tabs
# =========================

# Cuerpo principal del dashboard con todas las pestanas
body <- dashboardBody(
  # Contenedor de pestanas
  tabItems(
    # Pestana de bienvenida
    tabItem(
      tabName = "nav",
      h1("Welcome to our Yelp app!"),
      h2("Completed by Xiang Gao, Siqi Hu, Alexander Ilyin, Ziyuan Yan")
    ),
    # Pestana de mapa interactivo
    tabItem(
      tabName = "maps", class = "active",
      fluidRow(
        # Selector de ciudad
        box(
          selectInput("city_name", "City:", choice = c(" ", unique_cities))
        ),
        # Selector de categoria base
        box(
          selectInput("base_category", "Category of Business:", choice = c(" ", unique_categories))
        ),
        # Separador visual
        hr()
      ),
      # Salida del mapa Leaflet
      leafletOutput("yelp", width = 1000, height = 600)
    ),
    # Pestana de tabla de datos
    tabItem(
      tabName = "data_view",
      fluidRow(
        DT::dataTableOutput("table_display")
      )
    ),
    # Pestana de visualizaciones por ciudad
    tabItem(
      tabName = "city_viz",
      fluidRow(
        # Selector de ciudad
        box(
          selectInput("city_name_viz", "City:", choice = c(" ", unique_cities))
        ),
        hr(),
        # Grafico de promedio de estrellas
        plotOutput("yelp_cityviz_ratings"),
        br(),
        # Grafico de promedio de reviews
        plotOutput("yelp_cityviz_reviews")
      )
    ),
    # Pestana de mapa de calor por ciudad
    tabItem(
      tabName = "city_heat",
      fluidRow(
        box(
          selectInput("city_name3", "City:", choice = c(" ", unique_cities)),
          plotOutput("heat_map_city", width = 1300),
          height = 500, width = 50
        )
      )
    ),
    # Pestana de visualizacion de restaurantes
    tabItem(
      tabName = "rest_viz",
      fluidRow(
        box(plotOutput("rest_popularity_reviews"), solidHeader = TRUE, collapsible = TRUE),
        box(plotOutput("rest_popularity_pie"), solidHeader = TRUE, collapsible = TRUE)
      ),
      br(),
        box(
          selectInput(
            "city_name2", "City:",
            choice = c(
              " ",
              if (exists("unique_cities_reviews")) unique_cities_reviews else unique_cities
            )
          ),
          plotOutput("clustering"),
          collapsible = TRUE, width = 100
        )
    ),
    # Pestana de mapa de calor de restaurantes
    tabItem(
      tabName = "rest_heat_map",
      fluidRow(
        box(
          selectInput(
            "city_name5", "City:",
            choice = c(
              " ",
              if (exists("unique_cities_reviews")) unique_cities_reviews else unique_cities
            )
          ),
          plotOutput("rest_heat", width = 1300),
          height = 500, width = 50
        )
      )
    ),
    # Pestana de analisis de texto
    tabItem(
      tabName = "text_analysis",
      fluidRow(
        box(plotOutput("sentiment_by_cuisine"), solidHeader = TRUE, collapsible = TRUE, height = 600),
        box(
          selectInput(
            "city_name4", "City:",
            choice = c(
              " ",
              if (exists("unique_cities_reviews")) unique_cities_reviews else unique_cities
            )
          ),
          plotOutput("bigram_wordcloud"),
          solidHeader = TRUE, collapsible = TRUE, height = 600
        )
      )
    )
  )
)

# =========================
# 4) Dashboard UI
# =========================

# Ensambla el dashboard completo (header + sidebar + body)
ui <- dashboardPage(
  # Encabezado con titulo
  dashboardHeader(title = "Yelp Data Visualizer"),
  # Sidebar definida arriba
  sidebar,
  # Cuerpo con todas las pestanas
  body,
  # Tema visual del dashboard
  skin = "red"
)

# =========================
# 4.1) Cache de geocodificacion y mapas base
# =========================
# Evita llamadas repetidas a Google Maps y acelera la app.
geocode_cache <- new.env(parent = emptyenv())
map_cache <- new.env(parent = emptyenv())

# Limites para muestreo en graficos pesados (evita que la app demore minutos)
heatmap_max_points <- suppressWarnings(as.integer(Sys.getenv("HEATMAP_MAX_POINTS", "6000")))
if (is.na(heatmap_max_points) || heatmap_max_points < 500) {
  heatmap_max_points <- 6000
}
kmeans_max_points <- suppressWarnings(as.integer(Sys.getenv("KMEANS_MAX_POINTS", "4000")))
if (is.na(kmeans_max_points) || kmeans_max_points < 500) {
  kmeans_max_points <- 4000
}

get_geocode_cached <- function(place) {
  key <- toupper(trimws(place))
  if (identical(key, "")) {
    return(NULL)
  }
  if (exists(key, envir = geocode_cache, inherits = FALSE)) {
    return(geocode_cache[[key]])
  }
  res <- tryCatch(geocode(key), error = function(e) NULL)
  geocode_cache[[key]] <- res
  res
}

get_map_cached <- function(place, zoom = 11) {
  key <- paste0(toupper(trimws(place)), "|", zoom)
  if (exists(key, envir = map_cache, inherits = FALSE)) {
    return(map_cache[[key]])
  }
  res <- tryCatch(get_map(place, zoom = zoom), error = function(e) NULL)
  map_cache[[key]] <- res
  res
}

# =========================
# 5) Server logic
# =========================

# Funcion server con la logica reactiva de la app
server <- function(input, output, session) {
  # Render de la tabla principal
  output$table_display <- DT::renderDataTable({
    # Tabla con scroll horizontal para muchas columnas
    datatable(table_display, options = list(scrollX = TRUE))
  })

  # Reactivo: filtra por ciudad y categoria base
  filtered_data <- reactive({
    # Asegura que existan ambos inputs
    req(input$city_name, input$base_category)
    city_sel <- trimws(input$city_name)
    cat_sel <- trimws(input$base_category)
    # Si no hay filtros seleccionados, devuelve dataset vacio
    if (identical(city_sel, "") && identical(cat_sel, "")) {
      return(yelp_geo[0, ])
    }
    # Usamos el dataset con coordenadas validas para el mapa
    df <- yelp_geo
    # Filtra por ciudad si aplica
    if (!identical(city_sel, "")) {
      df <- df %>% filter(city_upper == toupper(city_sel))
    }
    # Filtra por categoria si aplica
    if (!identical(cat_sel, "")) {
      df <- df %>% filter(base_category == cat_sel)
    }
    df
  })

  # Render del mapa Leaflet con puntos de negocios
  output$yelp <- renderLeaflet({
    # Dataframe filtrado
    df <- filtered_data()
    # Valida que haya datos para mostrar
    shiny::validate(shiny::need(nrow(df) > 0, "No hay datos para los filtros seleccionados."))
    city_sel <- trimws(input$city_name)
    # Construye el mapa con marcadores
    map <- leaflet(df, options = leafletOptions(preferCanvas = TRUE)) %>% addTiles()
    if (!identical(city_sel, "")) {
      geoloc <- get_geocode_cached(city_sel)
      if (!is.null(geoloc) && all(c("lon", "lat") %in% names(geoloc))) {
        map <- map %>% setView(lng = geoloc$lon, lat = geoloc$lat, zoom = 12)
      } else {
        map <- map %>% fitBounds(
          lng1 = min(df$longitude), lat1 = min(df$latitude),
          lng2 = max(df$longitude), lat2 = max(df$latitude)
        )
      }
    } else {
      map <- map %>% fitBounds(
        lng1 = min(df$longitude), lat1 = min(df$latitude),
        lng2 = max(df$longitude), lat2 = max(df$latitude)
      )
    }
    map %>%
      addCircleMarkers(
        lng = ~df$longitude, lat = ~df$latitude,
        radius = 4, stroke = FALSE, fillOpacity = 0.6,
        popup = paste(
          df$name, "<br>",
          df$address, "<br>",
          df$stars, "stars", "<br>",
          df$review_count, "reviews", "<br>",
          input$base_category, "<br>"
        ),
        clusterOptions = markerClusterOptions()
      )
  })

  # Reactivo para graficos por ciudad (promedios por categoria)
  filtered_data_viz <- reactive({
    # Asegura que exista el input de ciudad
    req(input$city_name_viz)
    # Si no hay ciudad seleccionada, devuelve tibble vacio
    city_sel <- trimws(input$city_name_viz)
    if (identical(city_sel, "")) {
      return(tibble(
        base_category = character(),
        total = integer(),
        avg_review_count = double(),
        avg_stars = double()
      ))
    }
    # Agrupa por categoria y calcula totales y promedios
    yelp.plot %>%
      filter(city_upper == toupper(city_sel)) %>%
      group_by(base_category) %>%
      summarise(
        total = n(),
        avg_review_count = mean(review_count),
        avg_stars = mean(stars),
        .groups = "drop"
      )
  })

  # Grafico: promedio de estrellas por categoria
  output$yelp_cityviz_ratings <- renderPlot({
    # Datos agregados por categoria
    data <- filtered_data_viz()
    # Valida que existan datos
    shiny::validate(shiny::need(nrow(data) > 0, "No hay datos para esta ciudad."))
    # Barplot de promedio de estrellas
    ggplot(data, aes(base_category, avg_stars, fill = base_category)) +
      geom_bar(stat = "identity") +
      xlab("Category") + ylab("Average Rating") +
      theme(
        legend.title = element_blank(),
        axis.text.x = element_text(angle = 90, hjust = 1)
      )
  })
  # Grafico: promedio de reviews por categoria
  output$yelp_cityviz_reviews <- renderPlot({
    # Datos agregados por categoria
    data <- filtered_data_viz()
    # Valida que existan datos
    shiny::validate(shiny::need(nrow(data) > 0, "No hay datos para esta ciudad."))
    # Barplot de promedio de reviews
    ggplot(data, aes(base_category, avg_review_count, fill = base_category)) +
      geom_bar(stat = "identity") +
      xlab("Category") + ylab("Average Review Count") +
      theme(
        legend.position = "none",
        axis.text.x = element_text(angle = 90, hjust = 1)
      )
  })

  # (No se usa en el UI actual) Popularidad de restaurantes por cantidad
  output$rest_popularity <- renderPlot({
    restaurants_full %>%
      filter(cuisine != "Others") %>%
      group_by(cuisine) %>%
      summarise(n = n()) %>%
      ggplot(aes(x = fct_reorder(cuisine, n), y = n)) +
      geom_bar(stat = "identity") +
      coord_flip() +
      labs(
        x = "Cuisine", y = "Count",
        title = "Popularity of Restaurants by Quantity"
      )
  })

  # Grafico de pastel para top 5 cocinas
  output$rest_popularity_pie <- renderPlot({
    restaurants_full %>%
      filter(
        cuisine == "American" | cuisine == "Mexican" | cuisine == "Chinese" |
          cuisine == "Japanese" | cuisine == "Italian"
      ) %>%
      group_by(cuisine) %>%
      summarise(n = n()) %>%
      ggplot(aes(x = "", y = n, fill = cuisine)) +
      geom_bar(width = 1, stat = "identity", color = "white") +
      coord_polar("y", start = 0) +
      geom_text(
        aes(y = n, label = paste0(round(n / sum(n) * 100, 2), "%")),
        position = position_stack(vjust = 0.4),
        color = "white"
      ) +
      theme_void() +
      labs(
        title = "Popularity of Restaurants by Quantity",
        subtitle = "Restaurants Reviewed after 2015"
      )
  })

  # Popularidad de restaurantes por menciones en reviews
  output$rest_popularity_reviews <- renderPlot({
    restaurants_full %>%
      filter(cuisine != "Others") %>%
      group_by(cuisine) %>%
      summarise(n = n()) %>%
      ggplot(aes(x = fct_reorder(cuisine, n), y = n)) +
      geom_bar(stat = "identity") +
      coord_flip() +
      labs(
        x = "Cuisine", y = "Count",
        title = "Popularity of Restaurants by Review Mentions",
        subtitle = "Restaurants after 2015"
      )
  })

  # Grafico de sentimientos por cocina
  output$sentiment_by_cuisine <- renderPlot({
    # Valida que exista el dataframe de sentimientos
    shiny::validate(shiny::need(nrow(topSentiments_review) > 0, "No hay datos de sentimiento para mostrar."))
    ggplot(topSentiments_review, aes(x = sentiment, y = proportion, fill = sentiment)) +
      geom_bar(stat = "identity") +
      facet_wrap(~cuisine) +
      labs(
        title = "Sentiments in Top 5 Cuisine",
        subtitle = "Restaurants Reviewed after 2015"
      )
  })

  # Reactivo: datos para clustering por ciudad
  clustering_df <- reactive({
    # Asegura ciudad seleccionada
    req(input$city_name2)
    city_sel <- toupper(trimws(input$city_name2))
    if (identical(city_sel, "")) {
      return(tibble(longitude = numeric(), latitude = numeric(), city = character(), neighborhood = character()))
    }
    # Usamos el dataset prefiltrado de LAS VEGAS para rapidez
    df_source <- if (exists("restaurants_reviews_lv_geo")) restaurants_reviews_lv_geo else restaurants_reviews_full_geo
    df_source %>%
      filter(city_upper == city_sel) %>%
      select(longitude, latitude, city, neighborhood)
  })
  # Reactivo: limpia coordenadas para kmeans
  clustering_df_clean <- reactive({
    # Copia de datos base
    df <- clustering_df()
    # Convierte a numerico y elimina filas invalidas
    df$longitude <- as.numeric(df$longitude)
    df$latitude <- as.numeric(df$latitude)
    df <- df[is.finite(df$longitude) & is.finite(df$latitude), ]
    if (nrow(df) > kmeans_max_points) {
      set.seed(1234)
      df <- df %>% slice_sample(n = kmeans_max_points)
    }
    df
  })
  # Reactivo: calcula kmeans con numero seguro de clusters
  clusters <- reactive({
    # Datos limpios
    df <- clustering_df_clean()
    # Si hay muy pocos puntos, no se puede clusterizar
    if (nrow(df) < 2) {
      return(NULL)
    }
    # Define k como maximo 10 y nunca mayor a nrow - 1
    k <- min(10, nrow(df) - 1)
    if (k < 1) {
      return(NULL)
    }
    # Semilla para reproducibilidad
    set.seed(1234)
    # Ejecuta kmeans sobre coordenadas
    kmeans(df[, c("longitude", "latitude")], centers = k)
  })
  # Grafico: clustering en mapa base
  output$clustering <- renderPlot({
    city_sel <- trimws(input$city_name2)
    shiny::validate(shiny::need(!identical(city_sel, ""), "Seleccione una ciudad."))
    # Datos y clusters
    df <- clustering_df_clean()
    cl <- clusters()
    # Valida existencia de clusters y puntos
    shiny::validate(shiny::need(!is.null(cl) && nrow(df) >= 2, "No hay suficientes puntos para clustering."))

    # Asigna cluster a cada punto
    df$clusters <- as.factor(cl$cluster)

    # Mapa base con zoom fijo
    map <- get_map_cached(toupper(trimws(input$city_name2)), zoom = 11)
    # Puntos coloreados por cluster
    ggmap(map) +
      geom_point(aes(x = longitude, y = latitude, color = clusters), data = df) +
      theme(legend.position = "none") +
      labs(title = "K-Means Clustering by Neighborhood")
  })

  # Reactivo: datos para mapa de calor por ciudad
  city_heat_df <- reactive({
    req(input$city_name3)
    city_sel <- toupper(trimws(input$city_name3))
    if (identical(city_sel, "")) {
      return(yelp_geo[0, ])
    }
    # Usamos dataset con coordenadas validas
    df <- yelp_geo %>%
      filter(city_upper == city_sel)
    if (nrow(df) > heatmap_max_points) {
      set.seed(1234)
      df <- df %>% slice_sample(n = heatmap_max_points)
    }
    df
  })
  # Render del mapa de calor por ciudad
  output$heat_map_city <- renderPlot({
    df <- city_heat_df()
    shiny::validate(shiny::need(nrow(df) > 0, "Seleccione una ciudad con datos."))
    shiny::validate(shiny::need(nrow(df) >= 10, "Muy pocos puntos para generar el mapa de calor."))
    map <- get_map_cached(toupper(trimws(input$city_name3)), zoom = 11)
    shiny::validate(shiny::need(!is.null(map), "No se pudo cargar el mapa base para esta ciudad."))
    ggmap(map, extent = "device") +
      geom_density2d(data = df, aes(x = longitude, y = latitude), linewidth = 0.3) +
      stat_density2d(
        data = df,
        aes(x = longitude, y = latitude, fill = after_stat(level), alpha = after_stat(level)),
        linewidth = 0.01, bins = 16, geom = "polygon"
      ) +
      scale_fill_gradient(low = "green", high = "red") +
      scale_alpha(range = c(0, 0.3), guide = "none") +
      facet_wrap(~base_category, ncol = 5, nrow = 2) +
      labs(x = "Longitude", y = "Latitude", title = "Heatmap of Business Distribution") +
      theme(legend.position = "none")
  })

  # Reactivo: datos para mapa de calor de restaurantes
  rest_heat_df <- reactive({
    req(input$city_name5)
    city_sel <- toupper(trimws(input$city_name5))
    if (identical(city_sel, "")) {
      return(restaurants_reviews_full_geo[0, ])
    }
    # Usamos el dataset prefiltrado de LAS VEGAS para rapidez
    df_source <- if (exists("restaurants_reviews_lv_geo")) restaurants_reviews_lv_geo else restaurants_reviews_full_geo
    df <- df_source %>%
      filter(
        city_upper == city_sel &
          cuisine %in% c("American", "Mexican", "Italian", "Chinese", "Japanese")
      )
    if (nrow(df) > heatmap_max_points) {
      set.seed(1234)
      df <- df %>% slice_sample(n = heatmap_max_points)
    }
    df
  })
  # Render del mapa de calor de restaurantes
  output$rest_heat <- renderPlot({
    df <- rest_heat_df()

    # Valida que existan datos para las cocinas seleccionadas
    shiny::validate(
      shiny::need(nrow(df) > 0, "No hay datos para esta ciudad / cocinas seleccionadas.")
    )
    shiny::validate(shiny::need(nrow(df) >= 10, "Muy pocos puntos para generar el mapa de calor."))

    # Mapa base centrado en la ciudad
    map <- get_map_cached(toupper(trimws(input$city_name5)), zoom = 12)
    shiny::validate(shiny::need(!is.null(map), "No se pudo cargar el mapa base para esta ciudad."))

    # Densidad de puntos por cocina
    ggmap(map, extent = "device") +
      geom_density2d(data = df, aes(x = longitude, y = latitude), linewidth = 0.3) +
      stat_density2d(
        data = df,
        aes(x = longitude, y = latitude, fill = after_stat(level), alpha = after_stat(level)),
        linewidth = 0.01, bins = 16, geom = "polygon"
      ) +
      scale_fill_gradient(low = "green", high = "red") +
      scale_alpha(range = c(0, 0.3), guide = "none") +
      facet_wrap(~cuisine) +
      labs(x = "Longitude", y = "Latitude", title = "Heatmap of Restaurant Distribution") +
      theme(legend.position = "none")
  })

  # Reactivo: reviews por ciudad para analisis de texto
  reviews_by_city <- reactive({
    req(input$city_name4)
    city_sel <- toupper(trimws(input$city_name4))
    if (identical(city_sel, "")) {
      return(restaurants_reviews_full_clean[0, ])
    }
    # Usamos el dataset prefiltrado de LAS VEGAS para rapidez
    df_source <- if (exists("restaurants_reviews_lv")) restaurants_reviews_lv else restaurants_reviews_full_clean
    df_source %>%
      filter(city_upper == city_sel) %>%
      filter(!is.na(text) & text != "")
  })
  # Reactivo: genera bigramas desde reviews
  bigram_generate <- reactive({
    # Si ya existe el bigrama precalculado, lo usamos directamente
    city_sel <- toupper(trimws(input$city_name4))
    if (identical(city_sel, "LAS VEGAS") && exists("bigram_lv")) {
      return(bigram_lv)
    }
    reviews_by_city <- reviews_by_city() %>%
      mutate(text = as.character(text))
    # Si no hay textos, devuelve tibble vacio
    if (nrow(reviews_by_city) == 0) {
      return(tibble(bigram = character(), n = integer()))
    }
    # Tokeniza a bigramas y filtra ruido
    bigrams <- reviews_by_city %>%
      unnest_tokens(bigram, text, token = "ngrams", n = 2) %>%
      separate(bigram, c("word1", "word2"), sep = " ") %>%
      filter(
        !word1 %in% stop_words$word,
        !word2 %in% stop_words$word,
        !str_detect(word2, pattern = "[[:digit:]]"),
        !str_detect(word1, pattern = "[[:punct:]]"),
        !str_detect(word2, pattern = "[[:punct:]]"),
        !str_detect(word1, pattern = "\\b(.)\\b"),
        !str_detect(word2, pattern = "\\b(.)\\b")
      ) %>%
      unite("bigram", c(word1, word2), sep = " ") %>%
      filter(!bigram %in% bigram_stoplist) %>%
      count(bigram) %>%
      top_n(30)
  })
  # Render del wordcloud de bigramas
  output$bigram_wordcloud <- renderPlot({
    city_sel <- toupper(trimws(input$city_name4))
    shiny::validate(shiny::need(!identical(city_sel, ""), "Seleccione una ciudad."))
    shiny::validate(
      shiny::need(nrow(reviews_by_city()) > 0, "No review text available for this city")
    )
    bg <- bigram_generate()
    shiny::validate(shiny::need(nrow(bg) > 0, "No hay suficiente texto en esta ciudad para bigramas."))

    wordcloud(
      bg$bigram, bg$n,
      scale = c(3, 0.6),
      colors = brewer.pal(12, "Paired")
    )
  })
}

# =========================
# 6) Run app
# =========================

# Ejecuta la aplicacion Shiny
shinyApp(ui = ui, server = server)
