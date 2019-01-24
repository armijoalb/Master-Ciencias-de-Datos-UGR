
# se indica que se requiere el paquete Hmisc. En caso de no
# estar disponible habra que instalarlo
require(Hmisc)

# igual con el paquete fBasics
require(fBasics)

# se carga la definicion de funciones de lectura de datos
# (aunque si se ha trabajado antes con el archivo su definicion
# ya deberia estar en el entorno)
source("lecturaDatos.R")

# se comienza leyendo el conjunto de datos en forma csv
datos <- lecturaDatos("./data/","datos.csv")

# se determina la dimension del conjunto de datos: numero de
# filas y de columnas: de instancias y de variables
instancias <- nrow(datos)
variables <- ncol(datos) 

# se muestra la cabecera del conjunto de datos
head(datos)

# se muestra la parte final del conjunto de datos
tail(datos)

# se muestra la lista de nombres de variables del conjunto de datos
names(datos)

# muestra informacion de resumen sobre el conjunto de datos, sobre
# todas sus variables
summary(datos)

# se muestra informacion de las tres primeras variables
summary(datos[,1:3])

# otra funcion interesante es str, que muestra informacion sobre las variables
# de una forma mas compacta, y permite determinar si una variable es discreta,
# continua de forma sencilla
str(datos)

# tambien es posible visualizar los datos en forma de tabla (e incluso editarlos)
# View(datos)
# fix(datos)

# se muestra informacion sobre la primera variable. Observad que
# la funcion entiende que el indice no se refiere a una fila sino
# a una columna
Hmisc::describe(datos[1])

# se muestra informacion apra la variable class (categorica)
Hmisc::describe(datos[51])

# informacion detallada sobre la distribucion de valores de
# la variable separation
indiceVariable <- 1
info <- fBasics::basicStats(datos[indiceVariable])

# se muestra su densidad, junto con la linea vertical
# indicando el valor medio para determinar el grado
# de asimetria (skewness). Como la cola de la cola de la
# derecha es mayor, indica que la masa de la distribucion
# esta concentrada a la izquierda
d <- density(datos[,indiceVariable])
plot(d, main="Densidad de variable 1",xlab="Variable", ylab="Densidad")
polygon(d, col="red")
abline(v=info[7,], col="blue", lw=3)

# otra posible forma: paso a tibble
require(tidyverse)
datos <- as.tibble(datos)

# formas de seleccion de columnas
datos$separation
datos[[1]]
seleccion <- select(datos, separation, propensity)

# seleccion de filas
seleccion <- slice(datos, 1:100)

# seleccion de la forma usual
seleccion <- datos[1:100,]
seleccion <- datos[,1:4]




