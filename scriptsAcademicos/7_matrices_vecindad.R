#Paquetes

install.packages("spdep")

library(spData)
library(sf)
library(spdep)
library(ggplot2)

#mapa de Columbus, Ohio
# En este ejemplo, trabajamos con el shapefile de los 49 vecindarios de Columbus, Ohio.
# Este shapefile está incluido dentro del paquete spData.

# Leemos el archivo

map <- st_read(system.file("shapes/columbus.gpkg", 
                           package="spData")[1], quiet=TRUE)


#Como Columbus, Ohio está en el estado de Ohio (EE.UU.), una buena opción es usar un sistema de 
#referencia proyectado en UTM Zona 17N, que es el que cubre esa región.

#El EPSG correspondiente es:
  
#EPSG: 26917 — NAD83 / UTM Zone 17N
st_crs(map) <- 4326
map <- st_transform(map, 26917)


st_crs(map)$Name #ahora es WGS84
st_crs(map)$IsGeographic 
st_crs(map)$units_gdal 
st_crs(map)$srid 
st_crs(map)$proj4string
st_crs(map)$epsg

#1. Matriz basada en contigüidad #####

# Usamos poly2nb() de spdep para construir la lista de vecinos espaciales.
# poly2nb() detecta qué áreas son contiguas, es decir, comparten al menos un punto 
#o un borde.

nb <- spdep::poly2nb(map, queen = TRUE)

head(nb) # Cada elemento de nb es un vector que contiene los índices de las áreas vecinas a cada área

#Graficando

plot(st_geometry(map), border = "lightgray")
# Sobre ese mapa, superponemos la red de vecinos con plot.nb()
plot.nb(nb, st_geometry(map), add = TRUE)


# Supongamos que queremos analizar el área número 20.


# Creamos una columna 'neighbors' para clasificar cada área según su relación con el área 20.
map$neighbors <- "other"          # Inicialmente todas son "other"
map$neighbors[20] <- "area"        # Asignamos el valor "area" a la observación 20.
map$neighbors[nb[[20]]] <- "neighbors"  # Le ponemos el nombre "neighbors" a los vecino de 
#la observacion 20 utilizando nb



# Usamos ggplot2 para graficar el mapa, coloreando según la categoría de cada área
ggplot(map) +
  geom_sf(aes(fill = neighbors)) +
  theme_bw() +
  scale_fill_manual(values = c("gray30", "gray", "white")) +
  labs(title = paste("Área seleccionada y vecinos: Área"))

nb[[20]]
#2. Vecindad basada en k vecinos más cercanos (k-nearest neighbors)####

# Además de la vecindad por contigüidad, es posible definir vecinos
# basándonos en la distancia: cada área tiene como vecinos sus k vecinos más cercanos.
# Este método es útil cuando las áreas no son contiguas o hay espacios vacíos entre ellas.

# Primero, calculamos los centroides de cada área.
# Los centroides son los puntos representativos de cada polígono.
coo <- st_centroid(map)

plot(st_geometry(coo), border = "lightgray")

# Luego, usamos knearneigh() para obtener una matriz con los 3 vecinos más cercanos (k = 3)
# knn2nb() convierte esa matriz en una lista de vecinos de clase "nb" compatible con spdep
nb <- knn2nb(knearneigh(coo, k = 3))

# Visualizamos el mapa base
plot(st_geometry(map), border = "lightgray")



# Sobre el mapa, superponemos las conexiones entre cada área y sus 3 vecinos más cercanos
plot.nb(nb, st_geometry(map), add = TRUE)

#3. Vecindad basada en distancia fija####

# Creamos una estructura de vecindad donde cada área
# considera como vecinas a aquellas áreas cuyos centroides estén dentro
# de un cierto rango de distancia.

# Primero, calculamos los centroides de cada vecindario.
# st_centroid() devuelve un objeto sf de puntos, que representa los centroides
# de cada polígono (vecindario).
coo <- st_centroid(map)


st_crs(coo)$Name 
st_crs(coo)$IsGeographic 
st_crs(coo)$units_gdal 
st_crs(coo)$srid 
st_crs(coo)$proj4string
st_crs(map)$epsg

# Creamos la lista de vecinos usando dnearneigh().
# Esta función define vecinos entre áreas cuyos centroides están separados por
# una distancia entre d1 (mínima) y d2 (máxima).

nb <- dnearneigh(x = coo, d1 = 0, d2 = 200000)

#200000 metros, 200 kilometros

# Visualizamos el mapa base con st_geometry()
# st_geometry() extrae únicamente la geometría (polígonos) de un objeto sf.
# Es decir, no muestra atributos, solo la forma espacial.
# plot() dibuja esas geometrías.
plot(st_geometry(map), border = "lightgray")

# Sobre ese mapa base, plot.nb() añade las líneas que conectan los vecinos detectados.
# La lista nb contiene las relaciones de vecindad basadas en distancia,
# y st_geometry(map) proporciona las coordenadas de cada área para dibujar correctamente.
plot.nb(nb, st_geometry(map), add = TRUE)



# - El argumento "add = TRUE" permite superponer las líneas de vecindad
#   sobre el mapa ya dibujado.


# Cálculo de la distancia mínima que garantiza al menos un vecino


# En algunos análisis es necesario asegurar que cada área tenga al menos un vecino.
# Para esto, podemos calcular cuál es la menor distancia que asegura que cada área
# esté conectada con al menos otra.

# Recalculamos los centroides para mayor claridad (aunque coo ya existía).
coo <- st_centroid(map)

# Generamos la lista de vecinos basada en el 1 vecino más cercano (k = 1).
# knearneigh() encuentra el vecino más cercano para cada área.
# knn2nb() convierte esa matriz de vecinos en un objeto de clase nb (spdep).
nb1 <- knn2nb(knearneigh(coo, k = 1))

# Calculamos las distancias entre cada área y su vecino más cercano.
# nbdists() devuelve una lista con las distancias entre cada par de vecinos conectados.
dist1 <- nbdists(nb1, coo)


# - El valor máximo (Max.) representa la distancia mínima que garantiza
#   que cada área tenga al menos un vecino.
# - Esta es una estrategia útil para definir un umbral de vecindad en análisis exploratorios.
max_dist <- max(unlist(dist1))
max_dist
                
nb <- dnearneigh(x = coo, d1 = 0, d2 = 297118.3)
plot(st_geometry(map), border = "lightgray")
plot.nb(nb, st_geometry(map), add = TRUE)

#4. Vecindad de Orden \(k\) Basada en Contigüidad#####

# Exploramos cómo identificar vecinos de orden superior.
# Vecinos de primer orden son las áreas que comparten frontera directa.
# Vecinos de segundo orden son los vecinos de los vecinos (es decir, áreas 
#conectadas indirectamente a través de una intermediaria).

# Cargamos la lista de vecinos de primer orden usando poly2nb().
# En este caso, usamos queen = TRUE (contigüidad Queen: comparte borde o vértice).
nb <- poly2nb(map, queen = TRUE)

# Usamos nblag() para crear una lista de vecinos de primer y segundo orden.
# maxlag = 2 indica que queremos vecinos de orden 1 y 2.
nblags <- spdep::nblag(neighbours = nb, maxlag = 2)

# Mirando vecinos de primer orden

# Graficamos el mapa base
plot(st_geometry(map), border = "lightgray")

# Superponemos la red de vecinos de primer orden
plot.nb(nblags[[1]], st_geometry(map), add = TRUE)

# - plot.nb() usa la geometría de map para dibujar las líneas de conexión.

# Mirando vecinos de segundo orden

# Graficamos el mapa base
plot(st_geometry(map), border = "lightgray")

# Superponemos la red de vecinos de segundo orden
plot.nb(nblags[[2]], st_geometry(map), add = TRUE)

# - Vecinos de segundo orden son áreas conectadas a través de un 
#intermediario.
# - Este concepto es relevante en difusión espacial, donde efectos se 
#transmiten a áreas vecinas indirectas.


# Vecinos acumulados de primer a segundo orden

# Usamos nblag_cumul() para crear una única lista que combine
# vecinos de primer y segundo orden en un solo objeto.
nblagsc <- spdep::nblag_cumul(nblags)

# Graficamos el mapa base
plot(st_geometry(map), border = "lightgray")

# Superponemos la red acumulada de vecinos de primer y segundo orden
plot.nb(nblagsc, st_geometry(map), add = TRUE)

# - En algunos modelos espaciales, incluir vecinos de orden superior mejora
#la captura de efectos espaciales.


#5. Matriz de pesos espaciales basada en la inversa de la distancia#####

# Paso 1: Obtener los centroides de cada área
coo <- st_centroid(map)

# Paso 2: Crear la lista de vecinos basada en contigüidad (Queen)
nb <- poly2nb(map, queen = TRUE)

# Paso 3: Calcular las distancias entre vecinos
# nbdists() devuelve una lista, donde cada elemento es un vector
# con las distancias entre un área y sus vecinos
dists <- nbdists(nb, coo)

# Paso 4: Convertir las distancias a pesos basados en la inversa
# Para cada conjunto de vecinos, calculamos 1/distancia
# Esto asigna mayor peso a vecinos más cercanos y menor peso a vecinos lejanos
ids <- lapply(dists, function(x) {1 / x})

# Paso 5: Crear la lista de pesos espaciales
# nb2listw() crea un objeto de clase "listw", que es el formato estándar
# para matrices de pesos espaciales en spdep
# glist = ids le pasa la lista de pesos calculados
# style = "B" significa que estamos usando pesos brutos, sin estandarización
nbw <- nb2listw(nb, glist = ids, style = "B")

# Inspeccionamos los pesos de los primeros 3 elementos
nbw$weights[1:3]




# Visualización de la matriz de pesos espaciales


# Convertimos la lista de pesos espaciales a una matriz completa
m2 <- listw2mat(nbw)

# Usamos levelplot para visualizar la matriz como un mapa de calor
# t(m2) transpone la matriz para que el eje x/y esté alineado con las áreas
lattice::levelplot(t(m2),
                   scales = list(y = list(at = c(10, 20, 30, 40),
                                          labels = c(10, 20, 30, 40))))


