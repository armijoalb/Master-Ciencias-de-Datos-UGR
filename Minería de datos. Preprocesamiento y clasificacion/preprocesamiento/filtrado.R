library(parallel)

# se carga el archivo con las funcione de lectura de datos
source("lecturaDatos.R")

# se fija la ruta donde se encuentran los datos
path <- "./data/"
file <- "datos.csv"

# lectura de los datos
datos <- lecturaDatos(path,file)

# se obtiene el porcentaje de valores perdidos de cada fila. La
# siguiente sentencia aplica una funcion al conjunto completo de
# datos, sumando todos los valores perdidos de cada instancia. El
# resultado de la funcion (antes de la division) sera una lista 
# con una entrada para cada instancia: el numero de datos perdidos
# presentes en ella. La division y la multiplicacion por 100 se
# usan para determinar el porcentaje de datos perdidos (100 por
# cien si todos los valores de una instancia fueran perdidos).
# El valor 1 del segundo argumento indica que la funcion se aplicara
# sobre todas las filas
system.time(res1 <- apply(datos, 1, function(x) sum(is.na(x))) / ncol(datos) * 100)

# posibilidad de ejecutar esta operacion en parelelo
cores <- detectCores()
cluster <- makeCluster(cores-2)
system.time(res2 <- parRapply(cluster, datos, function(x) sum(is.na(x)))/ncol(datos)*100)
names(res2) <- NULL
stopCluster(cluster)

# se marcan aquellas filas con valor de porcentaje mayor de 5. Como
# hay 51 variables, equivale a instancias en que hay 3 variables sin 
# valor (redondeando). mal es una lista de indices de instancias en
# que se cumple la condicion indicada
mal <- (res1 > 5)

# datos filtrados: se quitan las filas con muchos valores perdidos
filtrados <- datos[!mal,]

# se muestra el numero de filas del conjunto inicial y del resultante
cat("Instancias archivo original: ",nrow(datos)," instancias en filtrado: ",nrow(filtrados),"\n")

# se guardan los datos filtrados
escrituraDatos(path,"datosFiltrados.csv",filtrados)
