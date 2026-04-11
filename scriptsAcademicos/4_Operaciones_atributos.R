#Paquetes

library(sf)      
library(terra)   
library(dplyr)   
library(spData) 
library(ggplot2)
library(tmap)

#1 Manipulación de atributos en datos vectoriales####

methods(class = "sf") # Métodos para objetos sf


class(world) 
View(world)

dim(world)

#La función st_drop_geometry() conserva únicamente los datos
#de atributos de un objeto sf, es decir, elimina su geometría.

world_df = st_drop_geometry(world)
class(world_df)
ncol(world_df)

#1.1 Subconjuntos de atributos en datos vectoriales####

world[1:6, ]    # subseleccionar filas por posición
world[, 1:3]    # subseleccionar columnas por posición
world[1:6, 1:3] # subseleccionar filas y columnas por posición
world[, c("name_long", "pop")] # subseleccionar columnas por nombre
world[, c(T, T, F, F, F, F, F, T, T, F, F)] # subseleccionar columnas mediante índices lógicos
world[, 888] # un índice que representa una columna inexistente


# Una demostración de la utilidad de los vectores lógicos en R para 
#seleccionar subconjuntos de datos. Este ejemplo muestra cómo crear
#un nuevo objeto 'small_countries' con países cuya superficie es 
#menor a 10,000 km cuadrados.

# Paso 1: Crear un vector lógico que indique si la superficie de cada
#país es menor a 10,000 km cuadrados
i_small = world$area_km2 < 10000  # Genera un vector lógico 
#(TRUE si el país cumple la condición, FALSE en caso contrario)

# Paso 2: Resumir el vector lógico para ver cuántos países cumplen la
#condición
summary(i_small)  
# El resumen muestra el número de valores TRUE 
#(países con área < 10,000 km²) y FALSE 
#(países con área >= 10,000 km²)

# Paso 3: Utilizar el vector lógico para filtrar el conjunto de datos
#'world' y crear un subconjunto 'small_countries'
small_countries = world[i_small, ]  
print(small_countries)  # Muestra solo los países con superficie 
#menor a 10,000 km²

# Alternativa más concisa: usar el vector lógico directamente sin una variable intermedia
small_countries = world[world$area_km2 < 10000, ]  
print(small_countries)  # Mismo resultado

# Alternativa con la función base 'subset()'
# subset() es una función que permite filtrar datos de forma clara y 
#legible
small_countries = subset(world, area_km2 < 10000)  
print(small_countries)  # Mismo resultado usando subset()

# Este ejemplo demuestra cómo los vectores lógicos son útiles para 
#realizar operaciones de filtrado en R. Permiten seleccionar filas 
#basadas en condiciones específicas de manera eficiente y legible.
# Además, R ofrece múltiples métodos para filtrar datos, lo que 
#brinda flexibilidad según las preferencias del usuario.

#Tanto Base R como dplyr}pueden segmentar 
#Sf data frames. La elección depende del balance entre eficiencia, 
#reproducibilidad y facilidad de escritura.


# Seleccionar solo dos columnas: 'name_long' y 'pop'
world1 = select(world, name_long, pop)

# Verificar los nombres de las columnas seleccionadas
names(world1)  

# Nota: Es equivalente a usar world[, c("name_long", "pop")] en base R.
# Sin embargo, 'select()' es más legible y fácil de usar.

# Seleccionar un rango de columnas utilizando el operador ':'
# Esto selecciona todas las columnas desde 'name_long' hasta 'pop' (ambos incluidos)
world2 = select(world, name_long:pop)

# Excluir columnas específicas usando el operador '-'
# Aquí eliminamos 'subregion' y 'area_km2'
world3 = select(world, -subregion, -area_km2)

# Seleccionar y renombrar columnas al mismo tiempo usando 
#'nuevo_nombre = nombre_original'
world4 = select(world, name_long, population = pop)

# Comparación con la forma equivalente en Base R (requiere más código)
world5 = world[, c("name_long", "pop")]  # Selecciona las columnas
names(world5)[names(world5) == "pop"] = "population"  # Renombra 'pop' a 'population'

# select() también permite operaciones avanzadas con 'helper functions':
# contains(): Selecciona columnas que contienen un texto específico en su nombre
# starts_with(): Selecciona columnas que comienzan con un prefijo específico
# num_range(): Selecciona columnas con nombres que contienen números en un rango específico

# Ejemplo de uso de helper functions
world6 = select(world, starts_with("pop"))  # Selecciona todas las columnas cuyo nombre comienza con "pop"
world7 = select(world, contains("area"))  # Selecciona todas las columnas que contienen "area" en su nombre

# En dplyr, la mayoría de las funciones devuelven un data frame.
# Sin embargo, si queremos extraer una sola columna como un vector, usamos pull().
# Esto puede ser útil cuando necesitamos trabajar con una sola variable fuera de un tibble.

# Extraer la columna 'pop' como un vector numérico usando pull()
pop_vector = pull(world, pop)

# En base R, se pueden obtener los mismos resultados con las siguientes opciones:
pop_vector_base1 = world$pop  # Usando el operador $ (acceso directo a la columna)
pop_vector_base2 = world[["pop"]]  # Usando corchetes dobles [[]] para extraer la columna por nombre

# Todas estas opciones devuelven el mismo vector numérico de la columna 'pop'.

# slice() es el equivalente de select(), pero para filas en lugar 
#de columnas.
# Permite seleccionar subconjuntos de filas de un data frame.
# Por ejemplo, para seleccionar las filas de la 1 a la 6:
world_sliced = slice(world, 1:6) #Conserva geometría

# En base R, esto se podría hacer con:
world_sliced_base = world[1:6, ]  # Seleccionar filas por índice de
#posición


# filter() es la versión de dplyr de la función subset() en base R.
# Se usa para seleccionar filas que cumplen con una condición específica.

# Filtrar países con un área menor a 10,000 km²
world8 = filter(world, area_km2 < 10000)

ggplot() +
  geom_sf(data = world8)

# Filtrar países con una esperanza de vida mayor a 82 años
world9 = filter(world, lifeExp > 82)


ggplot() +
  geom_sf(data = world9)

# En base R, estas operaciones se podrían hacer con subset():
world8_base1 = subset(world, area_km2 < 10000)  # Filtrar por área
world8_base2 = subset(world, lifeExp > 82)  # Filtrar por esperanza de vida


# - pull() extrae una columna como vector en lugar de un tibble.
# - slice() se usa para seleccionar filas por posición, análogo a select() para columnas.
# - filter() facilita la selección de filas según condiciones, 
#de forma más intuitiva que subset() en base R.

#1.2 Encadenamiento de comandos con pipes####

# En `dplyr`, el operador 'pipe' (`%>%` o `|>`, desde R 4.1.0) 
#permite encadenar funciones de manera clara y legible.
# Con los pipes, la salida de una función se convierte 
#automáticamente en la entrada de la siguiente.

# Ejemplo: Filtrar solo países de Asia, luego seleccionar las 
#columnas 'name_long' y 'continent',
# y finalmente extraer las primeras 5 filas.

world9 = world |>  
  filter(continent == "Asia") |>  
  select(name_long, continent) |>  
  slice(1:5)  

# Este código se ejecuta de arriba hacia abajo, de manera secuencial
#y legible.

# Alternativa sin pipes: uso de funciones anidadas

# En este caso, las funciones se escriben una dentro de otra,
#dificultando la lectura.
# Se ejecutan desde el interior hacia el exterior, lo que puede
#hacer que el código sea menos intuitivo.

world10 = slice(
  select(
    filter(world, continent == "Asia"),
    name_long, continent
  ),
  1:5
)

# Comparado con el código anterior, este enfoque es más difícil de
#leer y mantener.

# Otra alternativa: dividir el código en múltiples líneas 
#auto-contenidas

# Este método se recomienda al desarrollar paquetes en R, ya que 
#permite inspeccionar los resultados intermedios con nombres 
#distintos. Sin embargo, puede hacer que el código sea más extenso 
#y desordenado en análisis interactivos.

world11_filtered = filter(world, continent == "Asia")  # Filtrar países de Asia
world11_selected = select(world9_filtered, name_long, continent)  # Seleccionar columnas relevantes
world11 = slice(world9_selected, 1:5)  # Seleccionar las primeras 5 filas

#1.3 Agregación de atributos vectoriales####

# La agregación implica resumir datos utilizando una o más 
#variables de agrupación.
# Esto se usa comúnmente para calcular estadísticas resumidas en 
#función de categorías.
# En este caso, queremos calcular la suma de la población (`pop`)
#por continente (`continent`).
# El dataset `world` contiene ambas variables necesarias para 
#realizar esta agregación.

# MÉTODO 1: Usando `aggregate()` en base R

# Se utiliza la función `aggregate()` para calcular la suma de la 
#población por continente.
# La sintaxis usa una fórmula `pop ~ continent`, lo que indica que
#`pop` se agrupa por `continent`.
colnames(world)
world_agg1 = aggregate(pop ~ continent  , FUN = sum,
                       data = world, na.rm = TRUE)

world_agg1$pop_km2 <- (world_agg1$pop/ world_agg1$area_km2)# Verificar la clase del objeto resultante
class(world_agg1)  # Devuelve "data.frame"

# El resultado es un data frame con seis filas (una por continente) y dos columnas:
# - `continent`: Nombre del continente
# - `pop`: Suma de la población en ese continente


# MÉTODO 2: `aggregate()` aplicado a objetos espaciales (`sf`)

# La función `aggregate()` es genérica y se comporta de manera 
#diferente según el tipo de datos.
# Cuando se usa con objetos `sf` (datos espaciales), se activa 
#automáticamente `aggregate.sf()`,
# lo que permite agregar datos espaciales con la geometría de los 
#continentes.

str(world)

world_agg2 = aggregate(world["pop"],
by = list(world$continent), FUN = sum, na.rm = TRUE)

# Verificar la clase del objeto resultante
class(world_agg2)  # Devuelve "sf" y "data.frame"


names(world)
ggplot() +
  geom_sf(data = world, aes(fill = pop))

ggplot() +
  geom_sf(data = world_agg2, aes(fill = pop))

# Verificar el número de filas
nrow(world_agg2)  # Devuelve 8, incluyendo el océano abierto

# MÉTODO 3: Usando `dplyr` con `group_by()` y `summarize()`

# En `dplyr`, la función `group_by()` define las variables de 
#agrupación.
# Luego, `summarize()` calcula la agregación deseada.

world_agg3 = world |>  
  group_by(continent) |>  
  summarize(pop = sum(pop, na.rm = TRUE))

ggplot() +
  geom_sf(data = world_agg3, aes(fill = pop))

# Este enfoque proporciona mayor flexibilidad, legibilidad y 
#control sobre los nombres de las columnas.

# MÉTODO 4: Extensión del método `dplyr` para múltiples 
#agregaciones

# Una de las ventajas de `dplyr` es la posibilidad de calcular 
#múltiples métricas al mismo tiempo.
# En este caso, no solo sumamos la población, sino que también
#calculamos:
# - El área total del continente (`sum(area_km2)`)
# - El número de países en cada continente (`n()` cuenta el 
#número de filas en cada grupo)

world_agg4 = world |>  
  group_by(continent) |>  
  summarize(Pop = sum(pop, na.rm = TRUE), 
            Area = sum(area_km2), N = n() )


ggplot() +
  geom_sf(data = world_agg4, aes(fill = Pop))


ggplot() +
  geom_sf(data = world_agg4, aes(fill = Area))

# En este resultado:
# - `Pop`: Población total del continente
# - `Area`: Área total sumada de los países dentro del continente
# - `N`: Número de países en el continente

# Resumen:
# - `aggregate()` en base R es útil para cálculos rápidos, pero 
#menos flexible.
# - `aggregate()` con objetos `sf` permite agregar datos espaciales.
# - `group_by() |> summarize()` en `dplyr` es más intuitivo y 
#flexible.
# - `dplyr` permite calcular múltiples métricas de manera simultánea.

#Por ultimo, 

# Combinando lo aprendido sobre dplyr: encadenamiento de múltiples
#funciones.
# Este código resume datos de países agrupados por continente 
#usando dplyr.
# Se calculan la población total, el área, la densidad poblacional y
#se ordenan los continentes según el número de países que contienen,
#manteniendo solo los tres más poblados.

world_agg5 = world |>  
  st_drop_geometry() |>  # Eliminar la geometría para mejorar el rendimiento
  select(pop, continent, area_km2) |>  # Seleccionar columnas relevantes
  group_by(Continent = continent) |>  # Agrupar por continente
  summarize(  
    Pop = sum(pop, na.rm = TRUE),  # Calcular la población total por continente
    Area = sum(area_km2),  # Calcular el área total por continente
    N = n()  # Contar el número de países en cada continente
  ) |>  
  mutate(Density = round(Pop / Area)) |>  # Calcular la densidad de población (hab/km²)
  slice_max(Pop, n = 3) |>  # Mantener solo los 3 continentes con más países
  arrange(desc(N))  # Ordenar por número de países en orden descendente


#No es un objeto espacial. 

#1.4 Unión de atributos en datos vectoriales####

# En este ejemplo, combinamos datos de producción de café con el dataset `world`.
# Los datos de café están en `coffee_data`, que proviene del paquete `spData`.
# Este dataset contiene:
# - `name_long`: Nombre del país productor de café.
# - `coffee_production_2016`: Producción estimada de café en 2016 (sacos de 60 kg).
# - `coffee_production_2017`: Producción estimada de café en 2017 (sacos de 60 kg).


# Realizamos una unión 'left join', que conserva todas las filas de `world`
# y añade los datos de `coffee_data` cuando hay coincidencias en `name_long`.
View(world)
View(coffee_data)


world_coffee =  left_join(world, coffee_data, 
                          by = join_by(name_long == name_long)) 

# El mensaje en la consola indica que se hizo la unión usando 
#`name_long` como clave.

# Verificamos la clase del objeto resultante.
class(world_coffee)  
# Devuelve: "sf" "tbl_df" "tbl" "data.frame"
# Esto significa que sigue siendo un objeto espacial (`sf`),
#pero con nuevas columnas.

# `names()` nos permite ver las columnas del nuevo dataset `world_coffee`.
names(world_coffee)  

# Entre las columnas añadidas, encontramos:
# - `coffee_production_2016`: Producción de café en 2016.
# - `coffee_production_2017`: Producción de café en 2017.


# Podemos visualizar los datos en un mapa o gráfico utilizando `plot()`.
plot(world_coffee["coffee_production_2017"])  


#USANDO INNER JOIN PARA COMBINAR DATOS EN R

# `coffee_data` tiene 47 registros, pero `world` tiene 177 países.
# Queremos unirlos manteniendo SOLO los países que tienen una 
#coincidencia en `name_long`.


# Realizar una INNER JOIN para mantener solo los países que aparecen en ambos datasets
world_coffee_inner = inner_join(world, coffee_data)

# Verificar cuántas filas tiene el nuevo dataset
nrow(world_coffee_inner)  
# Devuelve 45: significa que algunas filas de `coffee_data` no encontraron coincidencia en `world`.

colnames(world_coffee_inner)

ggplot() +
  geom_sf(data = world_coffee_inner, aes(fill = coffee_production_2017))

plot(world_coffee_inner["coffee_production_2017"]) 

# `setdiff()` nos permite encontrar los nombres de países en 
#`coffee_data` que NO están en `world`. Esto ayuda a diagnosticar 
#errores en la unión.

setdiff(coffee_data$name_long, world$name_long)

# El resultado indica que hay dos registros sin coincidencia:
# - "Congo, Dem. Rep. of"
# - "Others"

# "Others" no existe en `world`, por lo que se excluye.
# "Congo, Dem. Rep. of" parece ser una versión abreviada del 
#nombre en `world`.

#Cual debería ser el nombre

drc = stringr::str_subset(world$name_long, "Dem*.+Congo")

drc

# CORREGIMOS NOMBRES DE PAÍSES PARA QUE COINCIDAN

# Usamos `grepl()` para encontrar registros con "Congo," en su 
#nombre y renombrarlo.
coffee_data$name_long[grepl("Congo,", 
coffee_data$name_long)] = "Democratic Republic of the Congo"

# Repetimos el INNER JOIN ahora con el nombre corregido
world_coffee_match = inner_join(world, coffee_data)

# Verificamos el número de filas después de la corrección
nrow(world_coffee_match)  
# Devuelve 46: ahora sí coinciden todos los países productores de café.

#El Orden importa!!

# En los ejemplos anteriores, combinamos `world` con `coffee_data` 
#(`world` era la tabla principal).
# Ahora hacemos la operación en sentido contrario: usamos 
#`coffee_data` como tabla principal.

coffee_world = left_join(coffee_data, world)

# Verificamos la clase del objeto resultante
class(coffee_world)  
# Devuelve "tbl_df" "tbl" "data.frame", lo que indica que ya NO es un objeto `sf`.

# Esto significa que la salida de una unión suele coincidir con el formato de su primer argumento.
# Como `coffee_data` no es un objeto espacial, la salida tampoco lo es.

#1.5 Creación de atributos con datos vectoriales####

# A veces queremos crear nuevas columnas basadas en columnas existentes.
# En este caso, queremos calcular la densidad de población 
#dividiendo `pop` entre `area_km2`.

# MÉTODO 1: USANDO BASE R
world_new = world  # No sobrescribimos el dataset original
world_new$pop_dens = world_new$pop / world_new$area_km2  # Crear nueva columna

# MÉTODO 2: USANDO `mutate()` EN `dplyr`
world_new2 = world |>  
  mutate(pop_dens = pop / area_km2)  

# DIFERENCIA ENTRE `mutate()` Y `transmute()`:
# - `mutate()` agrega la nueva columna y mantiene todas las existentes.
# - `transmute()` agrega la nueva columna, pero elimina todas las demás 
#(excepto la geometría si es un objeto `sf`).

# UNIR COLUMNAS -

# Queremos combinar `continent` y `region_un` en una nueva columna
#`con_reg`.
# Usamos `:` como separador y eliminamos las columnas originales 
#(`remove = TRUE`).

world_unite = world |>  
  tidyr::unite("con_reg", 
               continent:region_un, sep = ":", remove = TRUE)

# Ahora `con_reg` contiene valores como "South America:Americas" 
#para Colombia.


# `separate()` hace lo opuesto a `unite()`: divide una columna en 
#múltiples columnas.
# Aquí volvemos a separar `con_reg` en `continent` y `region_un`.

world_separate = world_unite |>  
  tidyr::separate(con_reg, c("continent", "region_un"), sep = ":")


# RENOMBRAR COLUMNAS USANDO `rename()` Y `setNames()`

# MÉTODO 1: USANDO `rename()` EN `dplyr`
# Queremos cambiar `name_long` a `name`.
world = world |>  
  rename(name = name_long)

# MÉTODO 2: USANDO `setNames()` PARA RENOMBRAR TODAS LAS COLUMNAS
# Necesitamos un vector con los nuevos nombres en el mismo orden de las columnas originales.

new_names = c("i", "n", "c", "r", "s", "t", "a", "p", "l", "gP", "geom")  
world_new_names = world |>  
  setNames(new_names)


# ELIMINAR INFORMACIÓN ESPACIAL USANDO `st_drop_geometry()`


# En objetos `sf`, a veces queremos eliminar la geometría para 
#acelerar cálculos.
# `st_drop_geometry()` hace esto de forma segura, sin afectar los 
#datos originales.

world_data = world |>  
  st_drop_geometry()

# Verificar la clase del objeto resultante
class(world_data)  
# Devuelve "tbl_df" "tbl" "data.frame", lo que indica que ya NO es un objeto espacial `sf`.

# IMPORTANTE: NO usar `select(world, -geom)`, 
#ya que puede romper la estructura del dataset.

#2. Manipulación de objetos ráster##### 

#Creamos un ejemplo

# Usamos `rast()` para crear un objeto raster llamado `elev`, 
#que representa elevaciones.

elev = rast(nrows = 6, ncols = 6,   # Definir número de filas y columnas
xmin = -1.5, xmax = 1.5,  # Extensión espacial en la dirección x
ymin = -1.5, ymax = 1.5,  # Extensión espacial en la dirección y
vals = 1:36)  # Asignar valores a las celdas (1 a 36)

# Este raster tiene:
# - 6 filas y 6 columnas.
# - Extensión espacial desde -1.5 a 1.5 en ambas direcciones.
# - Valores numéricos de 1 a 36.


# Los objetos raster también pueden contener valores categóricos de tipo
#`factor`.

# Definir categorías de tipos de suelo
grain_order = c("clay", "silt", "sand")

# Generar valores aleatorios de estos tipos para 36 celdas
grain_char = sample(grain_order, 36, replace = TRUE)
grain_char
# Convertir a factor con niveles definidos
grain_fact = factor(grain_char, levels = grain_order)

# Crear un raster categórico con estos valores
grain = rast(nrows = 6, ncols = 6,
             xmin = -1.5, xmax = 1.5,
             ymin = -1.5, ymax = 1.5,
             vals = grain_fact)

# Este raster almacena tipos de suelo en cada celda en lugar de
#valores numéricos.

# Los objetos raster almacenan una tabla de atributos llamada 
#"Raster Attribute Table (RAT)".
# Se puede ver esta tabla con `cats()` o modificarla con `levels()`.

#Vendo la tabla

cats(grain)

#Modificando la tabla de atributos

# Creamos una copia del raster para no modificar el original
grain2 = grain  

# Definir una tabla de atributos para el raster
levels(grain2) = data.frame(value = c(0, 1, 2), 
wetness = c("wet", "moist", "dry"))

# Verificar los niveles asignados
levels(grain2)

#Visualizando

cats(grain2)
cats(elev) #Las tablas de atributos ráster son más comunes en los
#categóricos. 

tm_shape(grain) + tm_raster()

tm_shape(elev) + tm_raster()

#2.1 Subconjuntos de objetos ráster####


# Extraer el valor de la celda en la fila 1, columna 1
elev[1, 1]  

# Alternativamente, extraer el valor de la celda usando su ID
elev[1]  


# Si tenemos un raster con múltiples capas, podemos extraer valores para todas las capas a la vez.

# Crear un raster multicapa combinando `grain` y `elev`
two_layers = c(grain, elev)

# Extraer el valor de la celda ID 1 en ambas capas
two_layers[1]  

# Esto devuelve un data frame con una fila y dos columnas (una por cada capa).

# También podemos extraer todos los valores de un raster multicapa con `values()`
values(two_layers)


# MODIFICANDO VALORES DE UN RASTER


# Podemos modificar valores en celdas específicas usando subsetting.

# Cambiar el valor de la celda superior izquierda a 0
elev[1, 1] = 0  

# Verificar los cambios
elev[]

# Modificar múltiples celdas a la vez
elev[1, c(1, 2)] = 0  

elev[]


# MODIFICAR VALORES EN RASTERS MULTICAPA

# Para modificar valores en un raster con múltiples capas, podemos 
#usar matrices.
#Creamos ejemplo

two_layers = c(grain, elev) 

two_layers[]

# Definir valores nuevos para las capas
two_layers[1] = cbind(c(1), c(4))  

# Verificar los cambios
two_layers[]


# - Podemos extraer valores de un raster usando índices de fila/columna o IDs de celda.
# - En rasters multicapa, el subsetting devuelve valores para todas 
#las capas.
# - `values()` nos permite extraer todos los valores de un raster.
# - Podemos modificar valores en celdas específicas y en capas 
#múltiples usando matrices.

# Estos métodos permiten analizar y manipular datos espaciales 
#rasterizados en R de manera eficiente.

#2.2 ####
