#Paquetes
install.packages("sf")
install.packages("terra")
install.packages("dplyr")
install.packages("spData")
install.packages("ggplot2")
install.packages("units")
install.packages("tmap")



library(sf)
library(terra)
library(dplyr)
library(spData)
library(ggplot2)
library(units)
library(tmap)


#1. Operaciones espaciales sobre datos vectoriales####

# 1.1 SUBSETTING ESPACIAL####

# El subsetting espacial consiste en tomar un objeto espacial y 
# devolver un subconjunto que se relaciona con otro objeto en 
#el espacio.

# En este ejemplo, utilizamos los datasets `nz` y `nz_height`
#del paquete `spData`.
# - `nz`: contiene datos geográficos de 16 regiones en Nueva 
#Zelanda.
# - `nz_height`: contiene 101 puntos representando las 
#montañas más altas de Nueva Zelanda.


# SELECCIONAR UNA REGIÓN: CANTERBURY, esto para usarlo con 
#el fin de ejemplificar la operación espacial

# Filtramos la región de Canterbury en el dataset `nz`
canterbury = nz |> filter(Name == "Canterbury")

#Vemos la representación d elas dos capas

ggplot() +  geom_sf(data = nz)

ggplot() +  
  geom_sf(data = canterbury,  colour = "#08519c", fill = "#08306b") 


ggplot() +  geom_sf(data = nz)+
  geom_sf(data = canterbury,  colour = "#08519c", fill = "#08306b") 

# SUBSETTING ESPACIAL USANDO `[` CON `st_intersects()`

# Extraemos los puntos de `nz_height` que se encuentran dentro 
#de Canterbury

#Vemos los puntos
ggplot() +  geom_sf(data = nz) + 
  geom_sf(data = nz_height)

#hacemos la operación

canterbury_height = nz_height[canterbury, ,op = st_intersects]

# Esto devuelve un objeto `sf` con los puntos de `nz_height` 
# que tienen intersección con la región de Canterbury.

ggplot() +  geom_sf(data = nz) + 
geom_sf(data = canterbury,  colour = "#08519c", fill = "#08306b" , alpha = .5) +
  geom_sf(data = canterbury_height)


# USANDO `st_disjoint()` PARA OBTENER LOS PUNTOS QUE NO INTERSECTAN

# `st_disjoint()` devuelve los puntos que NO intersectan con Canterbury
NO_canterbury_height = nz_height[canterbury, , op = st_disjoint]

# Esto devuelve los puntos que están fuera de Canterbury.

ggplot() +  geom_sf(data = NO_canterbury_height) 

ggplot() +  geom_sf(data = nz) + 
  geom_sf(data = canterbury,  colour = "#08519c", fill = "#08306b" , alpha = .5) +
  geom_sf(data = NO_canterbury_height)


# USANDO `st_intersects()` PARA OBTENER UNA LISTA DE 
#RELACIONES ESPACIALES


# `st_intersects()` devuelve una lista de relaciones espaciales.
# Cada elemento indica con qué región intersecta cada punto.

sel_sgbp = st_intersects(x = nz_height, y = canterbury)

# Verificar la clase del objeto resultante
class(sel_sgbp)  # Devuelve "sgbp" (Sparse Geometry Binary Predicate)

# Convertimos la lista en un vector lógico (TRUE si intersecta, FALSE si no)
sel_logical = lengths(sel_sgbp) > 0

# Aplicamos el vector lógico para filtrar los puntos de `nz_height`
canterbury_height2 = nz_height[sel_logical, ]



ggplot() +  geom_sf(data = nz) + 
  geom_sf(data = canterbury_height2)

ggplot() +  geom_sf(data = nz) + 
  geom_sf(data = canterbury,  colour = "#08519c", 
          fill = "#08306b" , alpha = .5) +
  geom_sf(data = canterbury_height2)


# USANDO `st_filter()` COMO MÉTODO ALTERNATIVO


# `st_filter()` fue creado para mejorar la compatibilidad 
#entre `sf` y `dplyr`.
# Devuelve el mismo resultado que el operador `[` y 
#`st_intersects()`.

canterbury_height3 = nz_height |> 
  st_filter(y = canterbury, .predicate = st_intersects)


ggplot() +  geom_sf(data = nz) + 
  geom_sf(data = canterbury,  colour = "#08519c", 
          fill = "#08306b" , alpha = .5) +
  geom_sf(data = canterbury_height3)

#Otros ejemplos, solo lso que están dentro. En este caso,
#Coinciden
canterbury_height4 = nz_height |> 
  st_filter(y = canterbury, .predicate = st_within)



#Los que comparten algún borde. 

canterbury_height5 = nz |> 
  st_filter(y = canterbury, .predicate = st_touches)

ggplot() +  
  geom_sf(data = canterbury_height5)


ggplot() +  
  geom_sf(data = canterbury_height5) + 
  geom_sf(data = canterbury,  colour = "#08519c", 
          fill = "#08306b" , alpha = .5)



# st_within(): Verifica si una geometría está 
#completamente dentro de otra, sin tocar el borde.
# ✔ Ejemplo: Un punto dentro de un polígono.

# st_touches(): Verifica si dos geometrías se tocan en la 
#frontera, pero sin superponerse.
# ✔ Ejemplo: Dos polígonos que comparten un borde o un 
#punto en el borde de un polígono.
# ✖ No cuenta si una geometría está dentro de la otra.


# st_intersects(): Verifica si dos geometrías tienen algún
#punto en común (borde o interior).
# ✔ Ejemplo: Dos polígonos que se solapan, un punto dentro 
#de un polígono o tocando su borde.
# ✖ No cuenta si no hay ninguna intersección.

# st_overlaps(): Verifica si dos geometrías tienen una intersección
#parcial (no están contenidas una en otra).
# ✔ Ejemplo: Dos polígonos que se superponen pero ninguno está 
#completamente dentro del otro.
# ✖ No cuenta si una geometría está completamente dentro de la 
#otra.

# st_covers(): Verifica si una geometría contiene completamente a 
#otra, incluyendo su borde.
# ✔ Ejemplo: Un polígono que contiene otro polígono o línea 
#completamente.
# ✖ No cuenta si hay partes de B fuera de A.

# st_covered_by(): Verifica si una geometría está completamente 
#contenida dentro de otra, incluyendo su borde.
# ✔ Ejemplo: Un punto dentro o en el borde de un polígono.
# ✖ No cuenta si hay partes de A fuera de B.

#1.2 Unión espacial#### 


# En los datos no espaciales, las uniones se realizan con una 
#"clave" común entre tablas.
# En los datos espaciales, la unión se basa en relaciones 
#espaciales, lo que permite 
# transferir atributos de una capa espacial a otra basada en su 
#ubicación geográfica.

# La función `st_join()` en `sf` agrega columnas desde un objeto 
#fuente (`y`) a un objeto destino (`x`) basado en su relación 
#espacial.


#Creamos puntos aleatorios en el mundo para ejemplificar

# Para reproducibilidad, establecemos una semilla aleatoria
set.seed(2018)  

# Obtenemos los límites espaciales del mundo
bb = st_bbox(world)  

# Creamos 10 puntos con coordenadas aleatorias dentro de los 
#límites del mundo
random_df = data.frame(
  x = runif(n = 10, min = bb[1], max = bb[3]),  # Coordenada X aleatoria dentro de los límites
  y = runif(n = 10, min = bb[2], max = bb[4])   # Coordenada Y aleatoria dentro de los límites
)

# Convertimos estos puntos en un objeto espacial `sf`
random_points = random_df |>  
  st_as_sf(coords = c("x", "y"), crs = "EPSG:4326")  # Definimos las coordenadas y CRS geográfico

#repasando...

# - `random_points`: conjunto de puntos dispersos en la superficie terrestre sin atributos adicionales.
# - `world`: contiene información sobre los países, como `name_long` (nombre del país).
# - El objetivo es determinar en qué país cae cada punto.

#Filtramos los países que contienen alf;un punto

# Para evitar procesar países sin puntos, filtramos solo aquellos 
#que se intersectan al menos un punto.


world_random = world[random_points, ,op = st_intersects]  


ggplot() + geom_sf(data = world)  +
  geom_sf(data = world_random,  colour = "#08519c", 
          fill = "#08306b" ) +
  geom_sf(data = random_points, colour = "red", 
          fill = "red" )
  

View(world)
View(random_points)
View(world_random)

# Contamos cuántos países contienen al menos un punto aleatorio
nrow(world_random)  # Devuelve 4, es decir, cuatro países contienen puntos aleatorios.


# Ahora hacemos la UNIÓN ESPACIAL USANDO `st_join()`


# `st_join()` agrega la columna `name_long` (nombre del país) a
#los puntos aleatorios.
# Esto nos permite saber en qué país se encuentra cada punto.
colnames(world)
random_joined = st_join(random_points, world["name_long"], join = st_intersects)  


ggplot() + geom_sf(data = world)  +
  geom_sf(data = world_random,  colour = "#08519c", 
          fill = "#08306b" ) +
  geom_sf(data = random_joined, colour = "red", 
          fill = "red" )

View(random_joined)

# Ahora `random_joined` contiene los puntos originales con un 
#nuevo atributo: el país en el que caen. No te que algunos
#no tienen nombre debido a que no caen en ninguna parte.


# Acerca de `st_join()`


# - `st_join()` realiza por defecto una "left join", lo que 
#significa que:
#   - Mantiene todos los puntos de `random_points`, incluso aquellos
#sin país.
#   - Si un punto no cae en un país, la columna `name` tendrá 
#un valor `NA`.
# - Si se desea una "inner join" (es decir, excluir los puntos sin 
#país), se usa `left = FALSE`:
# - Si se desea un criterio diferente (por ejemplo, 
#`st_within()` para puntos estrictamente dentro de los países),
#   se puede cambiar con el argumento `join =`.

random_joined_Inner = st_join(random_points, world["name_long"], join = st_intersects,
                              left=FALSE) 

ggplot() + geom_sf(data = world)  +
  geom_sf(data = world_random,  colour = "#08519c", 
          fill = "#08306b" ) +
  geom_sf(data = random_joined_Inner, colour = "red", 
          fill = "red" )

View(random_joined_Inner)





#1.3 Uniones Basadas en Distancia ####

# Algunas veces, dos conjuntos de datos geográficos no se 
#intersectan directamente, pero están estrechamente relacionados 
#espacialmente debido a su proximidad. En estos casos, podemos 
#usar uniones basadas en distancia.

# En este ejemplo, utilizamos datos de estaciones de bicicletas en
#Londres:
# - `cycle_hire`: ubicaciones oficiales de alquiler de bicicletas.
# - `cycle_hire_osm`: ubicaciones de alquiler según OpenStreetMap (OSM).


# Graficamos las estaciones oficiales de bicicletas (azul) y 
#las de OSM (rojo)
plot(st_geometry(cycle_hire), col = "blue")  
plot(st_geometry(cycle_hire_osm), add = TRUE, pch = 3, col = "red")

# Aunque las estaciones parecen estar cercanas, verificamos si 
#alguna coincide exactamente:
any(st_intersects(cycle_hire, cycle_hire_osm, sparse = FALSE))  
# Esto devuelve `FALSE`, lo que indica que no hay coincidencias 
#exactas.

# UNIÓN ESPACIAL BASADA EN DISTANCIA
 View(cycle_hire)
 View(cycle_hire_osm)

# Si queremos asignar la capacidad de `cycle_hire_osm` a 
#`cycle_hire`, pero no hay coincidencias exactas, usamos 
#`st_is_within_distance()`.

# Definimos una distancia de 20 metros para considerar puntos 
 #cercanos.
sel = st_is_within_distance(cycle_hire, cycle_hire_osm, 
                            dist = set_units(20, "m"))

# Verificamos cuántos puntos en `cycle_hire` tienen una estación
#cercana en `cycle_hire_osm`
summary(lengths(sel) > 0) 

# Esto muestra que hay 438 puntos en el objeto objetivo cycle_hire 
# dentro de la distancia umbral de cycle_hire_osm.

# ¿Cómo recuperar los valores asociados con los respectivos 
# puntos de cycle_hire_osm?


# Para agregar la información de `cycle_hire_osm` a `cycle_hire`,
#usamos `st_join()`

z = st_join(cycle_hire, cycle_hire_osm, st_is_within_distance,
            dist = set_units(20, "m"))

# Contamos las filas antes y después de la unión
nrow(cycle_hire)  # Número original de puntos
nrow(z)  # Mayor que `cycle_hire`, indicando duplicación de filas


 # IMPORTANTE 

# El número de filas en `z` es mayor porque algunas estaciones en 
#`cycle_hire` tienen múltiples coincidencias en `cycle_hire_osm`
#dentro de 20 metros. Si queremos obtener un único valor por punto,
#podemos agregar los valores por punto usando métodos de agregación 
#como `mean()`.

# Si un `id` tiene varias estaciones cercanas, tomamos el promedio
#de `capacity`
View(z)
z = z |>  
  group_by(id) |>  
  summarize(capacity = mean(capacity))  

# Verificamos que el número de filas vuelva a coincidir con 
#`cycle_hire`
nrow(z) == nrow(cycle_hire)  # Esto debería devolver `TRUE`

View(z)
# Para verificar que los valores se han transferido correctamente, 
# comparamos los datos originales `cycle_hire_osm` con los 
#resultantes en `z`.
dev.off()
plot(cycle_hire_osm["capacity"])  # Muestra la capacidad original en OSM
plot(z["capacity"])  # Muestra la capacidad tras la unión y agregación

#No cambia mucho la distriución espacial, es util. 


# - `st_is_within_distance()` permite encontrar objetos cercanos 
#sin intersección directa.
# - `st_join()` con `st_is_within_distance` une atributos basados 
#en proximidad.
# - Si una estación en `cycle_hire` tiene múltiples coincidencias en `cycle_hire_osm`,
#   la unión espacial generará duplicados, que se pueden manejar con agregación.
# - Este método es útil para unir datos espaciales cuando la proximidad es más relevante que la intersección.



#1.4 Agregación Espacial####

# En la agregación de datos, se reduce la cantidad de filas del 
#conjunto de datos original.
# Las funciones estadísticas como `mean()`, `sum()`, `median()`
#condensan múltiples valores
# en un solo valor basado en una variable de agrupación.

# La agregación espacial aplica este mismo concepto, pero 
#considerando la geometría de los datos.


# EJEMPLO: CALCULAR LA ALTURA PROMEDIO DE LOS PUNTOS ALTOS POR
#REGIÓN

# `nz_height` contiene los puntos más altos de Nueva Zelanda.
# `nz` contiene las 16 regiones principales de Nueva Zelanda.


# `aggregate()` aplica la función de agregación `FUN` a `x`, 
#agrupando por `by`.
# La geometría resultante de `nz_agg` será la misma que `nz`.
nz_agg = aggregate(x = nz_height, by = nz, FUN = mean)

View(nz)
View(nz_height)
View(nz_agg)

# Para verificar que `nz_agg` conserva la geometría de `nz` podemos usar:
identical(st_geometry(nz), st_geometry(nz_agg))  # TRUE

ggplot() +  geom_sf(data = nz)+
  geom_sf(data = nz_height)

#El resultado, el promedio de altura de las regiones donde 
#existen puntos

ggplot() +
  geom_sf(data = nz_agg,  colour = "#08519c", fill = "#08306b") +
  geom_sf(data = nz_height,  colour = "red", fill = "red")

colnames(nz_agg)

tm_shape(nz_agg) + 
  tm_polygons("elevation",
              fill.scale = tm_scale(values = "brewer.greens"))


# Otro enfoque: `st_join()`, `group_by()` Y `summarize()` 
#(TIDY APPROACH)

# En este método, primero realizamos un `st_join()` para unir 
#`nz_height` a `nz`
# Luego usamos `group_by()` para agrupar por nombre de la región
# Finalmente aplicamos `summarize()` para calcular la altura
#promedio en cada grupo

View(nz)
View(nz_agg2)

nz_agg2 = st_join(x = nz, y = nz_height) |>  
  group_by(Name) |>  
  summarize(elevation = mean(elevation, na.rm = TRUE))

#Resultado:

ggplot() +
  geom_sf(data = nz_agg2,  colour = "#08519c", fill = "#08306b") 


tm_shape(nz_agg2) + 
  tm_polygons("elevation",
              fill.scale = tm_scale(values = "brewer.greens"))

tm_shape(nz_agg) + 
  tm_polygons("elevation",
              fill.scale = tm_scale(values = "brewer.greens"))

# Sobre `aggregate()` Y `group_by() |> summarize()`

# - Ambos métodos producen un objeto `sf` con la misma geometría 
#que `nz`.
# - En `aggregate()`, las regiones sin coincidencias reciben 
#valores `NA`.
# - En `group_by() |> summarize()`, las regiones sin coincidencias 
#se conservan (ver las tablas, en nz_agg2 salen los nombres)
# - El método `tidy` (`group_by() |> summarize()`) permite mayor
#flexibilidad, ya que se pueden aplicar otras funciones como 
#`median()`, `sd()`, etc.

#1.5 Unión de Capas Incongruentes####

# En análisis espacial, la congruencia entre capas es clave para la
#agregación.
# - Una capa es **congruente** con otra si comparten bordes.
# - Una capa **incongruente** no tiene una coincidencia exacta de 
#fronteras.
# - Esto puede generar problemas en operaciones como la agregación 
#espacial.

# En este ejemplo, usamos:
# - `incongruent`: Polígonos pequeños con valores regionales de 
#ingresos.
# - `aggregating_zones`: Polígonos más grandes donde queremos 
#transferir los valores.
# - `value`: Columna en `incongruent` con los ingresos regionales 
#en millones de euros.

#El enfoque: INTERPOLACIÓN ESPACIAL BASADA EN ÁREA

# En este método, los valores de `incongruent` se transfieren a 
#`aggregating_zones` en proporción al área de superposición.
# - Cuanto mayor sea la intersección entre los polígonos de entrada
#y salida, mayor será el valor asignado.

#Si un polígono grande se superpone con varios polígonos pequeños, 
#recibe una parte de sus valores dependiendo de cuánto del área de 
#esos pequeños cubre. Por ejemplo, si el poligono grande contiene
#el 30% del territorio del poligono pequeño, entonces el poligono
#grande recibe el 30% de ese ingreso.


ggplot() +
  geom_sf(data = incongruent,  colour = "Black", fill = "Black", alpha = .5)

# Filtramos solo la columna de interés (valores de ingresos 
#regionales)
iv = incongruent["value"]

ggplot() +
  geom_sf(data = iv,  colour = "#9c0808", fill = "red", alpha = .5)

ggplot() +
  geom_sf(data = aggregating_zones,  colour = "#08519c",
          fill = "#08306b",  alpha = .5)


ggplot() +
  geom_sf(data = iv,  colour = "#9c0808", fill = "#9c0808", 
          alpha = 0.1) +
  geom_sf(data = aggregating_zones,  colour = "#08519c", 
          fill = "#08306b", alpha = 0.1)

# Aplicamos interpolación espacial basada en área con 
#`st_interpolate_aw()`
agg_aw = st_interpolate_aw(iv, aggregating_zones, extensive = TRUE)

# Visualizamos los valores agregados a las zonas de agregación
agg_aw$value  # Resultado esperado: valores interpolados en las 
#zonas más grandes


# NOTA IMPORTANTE: VARIABLES EXTENSIVAS VS. INTENSIVAS


# - En este caso, `value` representa ingresos totales, que es una 
# **variable extensiva**.
# - Las variables extensivas se **suman** cuando cambian de escala
#(ejemplo: ingresos totales, OJO, en niveles!!).
# - Las variables intensivas como *densidad de población* o 
# *porcentajes* deben manejarse con `extensive = FALSE`, 
# *para obtener promedios en vez de sumas.
View(incongruent)
# Para manejar variables intensivas como promedios:
agg_aw_avg = st_interpolate_aw(iv, aggregating_zones, 
                               extensive = FALSE)

#Resumen visual

colnames(iv)

ggplot() + 
  geom_sf(data = iv, aes(fill = value))

ggplot() + 
  geom_sf(data = agg_aw, aes(fill = value)) #Mira la escala

ggplot() + 
  geom_sf(data = agg_aw_avg, aes(fill = value)) #Mira la escala

#2. Operaciones espaciales sobre datos raster####
crs(elev)

elev = rast(system.file("raster/elev.tif", package = "spData"))
grain = rast(system.file("raster/grain.tif", package = "spData"))


#2.1 SUBSETTING ####

#seleccionar una parte de un conjunto de datos espaciales 
#(como un raster o un shapefile) usando coordenadas, un polígono o 
#alguna otra regla espacial. 

#aprenderemos cómo extraer valores específicos
# de un objeto raster utilizando:
# 1. Índices de celdas
# 2. Coordenadas (con `cellFromXY()` y `terra::extract()`)
# 3. Otro objeto raster para realizar el subsetting
# 4. Máscaras raster (con valores lógicos o NA)


st_crs(elev)$Name
st_crs(elev)$proj4string
st_crs(elev)$epsg
st_crs(elev)$IsGeographic 
st_crs(elev)$units_gdal

#Supongamos que conocemos las coordenadas de un lugar especifico en un raster y queremoss extraer

#

# EXTRAYENDO UN VALOR USANDO COORDENADAS (cellFromXY)
tm_shape(elev) +
  tm_raster() 
 

# Convertimos coordenadas a un ID de celda dentro del raster

#Encuentra el valor de la celda que cubre un punto ubicado en las 
#coordenadas 0.1, 0.1. 

#Partimos de que conocemos las coordenadas
# de ese punto.


id <- cellFromXY(elev, xy = matrix(c(0.1, 0.1), ncol = 2))

id 

# Extraemos el valor del raster en esa celda
 elev[id]


# Alternativamente, usamos `terra::extract()`
terra::extract(elev, matrix(c(0.1, 0.1), ncol = 2))


# SUBSETTING USANDO OTRO RASTER

#Ahora partimos de la idea de que tenemos un raster que queremos utilizar como "molde"
#para extraer información de un tarter más grande.

# Creamos un nuevo raster de referencia para filtrar 'elev'
clip <- rast(xmin = 0.9, xmax = 1.8, ymin = -0.45, ymax = 0.45,
             resolution = 0.3, vals = rep(1, 9))


tm_shape(clip) +
  tm_raster() 
tm_shape(elev) +
  tm_raster() 
# Aplicamos el subsetting: extraemos las celdas de 'elev' dentro del 'clip'
elev[clip]

# También podemos usar `terra::extract()`
terra::extract(elev, ext(clip))

#SUBSETTING con datos vectoriales

#Cargamos datos.

#srtm representing elevation (meters above sea level) in southwestern Utah
#A vector (sf) object zion representing Zion National Park

srtm = rast(system.file("raster/srtm.tif", package = "spDataLarge"))
zion = read_sf(system.file("vector/zion.gpkg", package = "spDataLarge"))
zion = st_transform(zion, st_crs(srtm))

tm_shape(srtm) +
  tm_raster() 

tm_shape(zion) +  
  tm_polygons()

tm_shape(srtm) +
  tm_raster() +         # Esto muestra el raster
  tm_shape(zion) +
  tm_borders()          


#Recortando datos

srtm_masked = mask(srtm, zion)

tm_shape(srtm_masked) +
  tm_raster()

#operacion inversa

srtm_inv_masked = mask(srtm, zion, inverse = TRUE)

tm_shape(srtm_inv_masked) +
  tm_raster()

# SUBSETTING CON ID DE CELDAS Y USO DEL ARGUMENTO 'drop'


#Ac'a asumimos que conocemos la ubicación de la celda, no als coordenadas.

# Extraemos las dos primeras celdas, asegurándonos de que el 
#resultado sea raster con drop =F
elv_1_2 <- elev[1:2, drop = FALSE]  

elv_1_2[1]
elv_1_2[2]


tm_shape(elev) +
  tm_raster()

tm_shape(elv_1_2) +
  tm_raster()


# USO DE MÁSCARAS logicas RASTER PARA FILTRAR PIXELES

# Creamos una máscara raster (valores TRUE o NA aleatorios)
rmask <- elev
values(rmask) <- sample(c(NA, TRUE), 36, replace = TRUE)

# Aplicamos la máscara a 'elev'
x = mask(elev, rmask)


tm_shape(elev) +
  tm_raster() 

tm_shape(x) +
  tm_raster(palette = "grays")

tm_shape(elev) +
  tm_raster(col = "blue") +  # Primer raster, color azul
  tm_shape(x) +
  tm_raster(palette = "grays")





# REEMPLAZANDO VALORES EN UN RASTER USANDO CONDICIONES

# Sustituyendo valores menores a 20 con NA

elev_P = elev
elev_P[elev_P < 20] <- NA


tm_shape(elev) +
  tm_raster() 


tm_shape(elev_P) +
  tm_raster() 

# - Podemos modificar valores dentro del raster aplicando condiciones
#booleanas.

#2.2 Álgebra de mapas####

#2.2.1 Operaciobes locales#####

# Las operaciones locales (o 'local operations') se aplican a cada 
#celda 
# individualmente (cell-by-cell) en uno o varios rásteres. Esto 
#incluye:
#   - Operaciones aritméticas (suma, resta, multiplicación, etc.).
#   - Transformaciones matemáticas (por ej. logaritmos).
#   - Comparaciones lógicas (por ej. elev > 5).
#   - Reclasificaciones basadas en rangos de valores.



# A continuación, realizamos algunas operaciones locales (cell-by-cell).
# Estos ejemplos muestran cómo se pueden aplicar expresiones 
#aritméticas y lógicas a cada celda.

# Suma de un ráster consigo mismo (equivale a duplicar los valores).
elev_suma <- elev + elev

tm_shape(elev) +
  tm_raster() 

tm_shape(elev_suma) +
  tm_raster() 

# Elevar al cuadrado (toma cada valor de elev y lo eleva a la segunda potencia).
elev_cuad <- elev^2

tm_shape(elev_cuad) +
  tm_raster()

# Logaritmo (natural) de cada celda en el ráster.
elev_log <- log(elev)

tm_shape(elev_log) +
  tm_raster() 

# Comparación lógica: definimos un nuevo ráster que indica con 
#TRUE (1) las celdas donde elev > 5 y con FALSE (0) donde no.
elev_mayor_5 <- elev > 5  #Util, pensar en aplicaciones.

tm_shape(elev_mayor_5) +
  tm_raster()

 plot(elev_mayor_5, main = "elev > 5")

#  Reclasificación de valores


# Otra operación local común es la reclasificación de valores en 
 #intervalos.
# Por ejemplo, podemos agrupar valores de elevación en tres clases:
#  - [0, 12) -> Clase 1
#  - [12, 24) -> Clase 2
#  - [24, 36) -> Clase 3
#
# Para ello, definimos una matriz de reclasificación (rcl) con tres
 #columnas:
#   1) Límite inferior
#   2) Límite superior
#   3) Valor asignado a ese rango
rcl <- matrix(c(
  0, 12, 1,
  12, 24, 2,
  24, 36, 3
), ncol = 3, byrow = TRUE)

rcl

# Aplicamos esta matriz a nuestro ráster 'elev' con la función
#classify().
elev_recl <- classify(elev, rcl = rcl)


tm_shape(elev_recl) +
tm_raster(style = "cat")

# Ahora, las celdas con valores entre 0-12 tendrán el valor 1, las celdas entre
# 12-24 el valor 2 y las celdas entre 24-36 el valor 3.
# plot(elev_recl, main = "Ráster reclasificado (1, 2, 3)")

#  Cálculo del NDVI usando un archivo multiespectral de Landsat 8

# El NDVI (Índice de Vegetación de Diferencia Normalizada) se calcula
#a partirde dos bandas de una imagen satelital: la banda roja (Red) y
#la banda  infrarroja cercana (NIR). Cada celda de NDVI resulta de la
#operación local:
#
#              NDVI = (NIR - Red) / (NIR + Red)
#
# Los valores de NDVI oscilan entre -1 y 1. Valores positivos 
#(en especial > 0.2) suelen indicar vegetación densa. A continuación, 
#mostramos cómo se hace esta operación con un ejemplo de Landsat 8.

#corresponde a una zona de Alemania, específicamente una región en el estado de Mecklemburgo-Pomerania 
#Occidental (Mecklenburg-Vorpommern), al noreste de Alemania.

# Leemos el archivo 'landsat.tif' como un objeto ráster multidimensional.
# Este ráster tiene 4 bandas: Blue, Green, Red, NIR.
multi_rast <- rast(system.file("raster/landsat.tif", package = "spDataLarge"))


# Visualizar en color natural (Red = 3, Green = 2, Blue = 1)
plotRGB(multi_rast, r = 3, g = 2, b = 1, stretch = "lin",
        main = "Imagen RGB (Color Natural)")



# Landsat 8 Nivel 2: los valores de los píxeles (DN - Digital Numbers) 
# son valores enteros almacenados para ahorrar espacio y facilitar el
#procesamiento.
# Sin embargo, estos DN no representan directamente la reflectancia real 
#de la superficie.
# 
# Para convertir los DN a valores de reflectancia (entre 0 y 1), 
# que son físicamente interpretables (proporción de luz reflejada), 
# es necesario aplicar un factor de escala y un offset.
# 
# Fórmula de conversión recomendada por USGS para Landsat 8 Nivel 2:
# Reflectancia = (DN * 0.0000275) - 0.2


multi_rast <- (multi_rast * 0.0000275) - 0.2

# Debido a la presencia de nubes o errores atmosféricos, algunos valores podrían
# resultar negativos. Para simplificar, forzamos esos valores a 0.
multi_rast[multi_rast < 0] <- 0

# Definimos una función en R que calcula el NDVI, recibiendo como
#parámetros  'nir' y 'red' para luego hacer la fórmula 
#(nir - red)/(nir + red).
ndvi_fun <- function(nir, red) {
  (nir - red) / (nir + red)
}

# En este ráster, la banda 4 corresponde a NIR y la banda 3 a Red, 
#por lo que hacemos un subconjunto para quedarnos sólo con esas dos 
#bandas. Luego, usamos la función lapp() para aplicar ndvi_fun 
#cell-by-cell.
ndvi_rast <- lapp(multi_rast[[c(4, 3)]], fun = ndvi_fun)

# Graficamos el NDVI resultante (descomentar para visualizar):
 plot(ndvi_rast, main = "NDVI calculado a partir de Landsat 8")

tm_shape(ndvi_rast) +
   tm_raster(palette = "-RdYlGn", title = "NDVI") +
   tm_layout(main.title = "Índice NDVI", legend.outside = TRUE)


#2.2.2   Operaciones focales#####
 
# - A diferencia de las funciones "locales" que operan sobre cada 
#celda de forma independiente (cell-by-cell), las "operaciones focales" toman en cuenta 
#   una celda central y sus vecinas dentro de un vecindario (también llamado 
#   kernel, filtro o moving window).
# - El vecindario suele ser de tamaño 3x3 celdas (la celda central y sus ocho 
#   vecinas), pero puede adoptar cualquier otra forma o dimensión.
# - Una operación focal aplica una función de agregación (p.ej. mínimo, promedio, 
#   varianza) a los valores de todas las celdas dentro del vecindario, y asigna 
#   el resultado a la celda central. Luego avanza a la siguiente celda central 
#   hasta procesar todo el ráster.
# - Esto se conoce también como "filtrado espacial" o "convolución".



#  Definimos el vecindario: una matriz 3x3 con todos sus elementos en 1,
#    de modo que cada celda del vecindario tiene el mismo peso.
#    Esto implica que se considerarán los 9 valores 
#(celda central + 8 vecinas).
w_3x3 <- matrix(1, nrow = 3, ncol = 3)

w_3x3

# 3. Aplicamos focal() para obtener el valor mínimo en cada vecindario 
#3x3. De esta forma, el valor asignado a la celda central será el mínimo
# de las 9 celdas.

r_focal_min <- focal(
  x   = elev,       # Ráster de entrada
  w   = w_3x3,      # Ventana de vecindario 3x3
  fun = min,        # Función de agregación (mínimo)
  na.rm = TRUE      # Ignorar valores NA (si existieran)
)

# 5. Visualización :
plot(elev, main = "Ráster de entrada (0-35)")
 plot(r_focal_min, main = "Resultado focal (mínimo 3x3)")


 tm_shape(elev) +
   tm_raster()
 elev
 
 tm_shape(r_focal_min) +
   tm_raster()
 r_focal_min


 #2.2.3 Operaciones zonales #####
 
 # - Las operaciones zonales (zonal operations) aplican una función de agregación 
 #   a múltiples celdas de un ráster. 
 # - Para ello se utiliza un segundo ráster (normalmente categórico) que define 
 #   las “zonas” o “filtros zonales”. 
 # - A diferencia de las operaciones focales, no se requiere que las celdas 
 #   pertenezcan a vecindarios contiguos. Lo importante es que todas las celdas 
 #   con la misma categoría en el ráster zonal se agrupan para calcular la estadística.
 
 tm_shape(elev) +
   tm_raster()

 tm_shape(grain) +
   tm_raster()

 z <- zonal(elev, grain, fun = "mean")
z 


