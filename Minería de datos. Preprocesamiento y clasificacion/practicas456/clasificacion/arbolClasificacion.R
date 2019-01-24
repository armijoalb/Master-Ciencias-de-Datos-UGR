library(party)
library(caret)

# se carga la funcionalidad de lectura de datos
source("./lecturaDatos.R")

# se fija ls ruta de localizacion de los datos
path <- "./data/"
file <- "datos.csv"

# se llama a la funcion que carga los datos
datos <- lecturaDatos(path,file)

# se obtiene el nombre de la variable clase
# primero se obtiene la posicion de la variable clase: se asume 
# que es la ultima (pero no podemos estar seguros)
posicionClase <- length(names(datos))
variableClase <- names(datos)[posicionClase]

# se compone una formula con el nombre de la variable clase y ~
formulaClase <- as.formula(paste(variableClase,"~.",sep=""))

# se divide el conjunto de datos en train y test
# se crea la particion: esto obtiene de forma aleatoria un porcentaje
# de instancias dado por p. Esta funcionalidad esta disponible en el
# paquete caret
inTrain <- createDataPartition(y=datos[,posicionClase], p = .75, 
                               list = FALSE)

# ahora se obtienen los conjuntos de test y de entrenamiento
training <- datos[ inTrain,]
testing  <- datos[-inTrain,]

# construye el modelo
ct <- ctree(formulaClase, training)

# muestra el arbol de forma grafica, pero no tiene demasiado sentido
# al ser demasiado grande
plot(ct)

# se muestra en modo texto: observad que este metodo no precisa
# discretizacion previa al poder ir considerando diferentes cortes
# en variables numericas. Esto hace que puedan aparecer varias
# veces en el arbol
print(ct)

# se realiza la prediccion
testPred <- predict(ct, newdata = testing)

# se compara con los datos de test
results <- table(testPred, testing$class)

# se suman los valores de la diagonal
sumDiag <- sum(diag(results))

# se suman todos los valores de la matriz
sumTotal <- sum(results)

# se calcula el porcentaje de aciertos
fiabilidad <- sumDiag/sumTotal

# Se calcula el error
error <- 1-fiabilidad

# usamos un conjunto de datos mas sencillo para ver la forma
# que tendria el arbol generado
datos <- iris

# se obtiene el nombre de la variable clase
# primero se obtiene la posicion de la variable clase: se asume 
# que es la ultima (pero no podemos estar seguros)
posicionClase <- length(names(datos))
variableClase <- names(datos)[posicionClase]

# se compone una formula con el nombre de la variable clase y ~
formulaClase <- as.formula(paste(variableClase,"~.",sep=""))

# se divide el conjunto de datos en train y test
# se crea la particion: esto obtiene de forma aleatoria un porcentaje
# de instancias dado por p. Esta funcionalidad esta disponible en el
# paquete caret
inTrain <- createDataPartition(y=datos[,posicionClase], p = .75, 
                               list = FALSE)

# ahora se obtienen los conjuntos de test y de entrenamiento
training <- datos[ inTrain,]
testing  <- datos[-inTrain,]

# construye el modelo
ct <- ctree(formulaClase, training)

# ahora si vale la pena visualizar el arbol
plot(ct)

# se visualiza la tabla con las predicciones realizadas
predicciones <- predict(ct, testing)

# tambien pueden estimarse las probabilidades de asignacion
# de cada instancias a cada clase
probabilidades <- predict(ct, testing, type="prob")

