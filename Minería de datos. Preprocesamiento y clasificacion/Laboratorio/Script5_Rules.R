library(RWeka)

# Construyo los ejemplos de training y test
set.seed(9)
train = sample(1:nrow(iris),2*nrow(iris)/3)
iris.test = iris[-train,]


# Aplico el algoritmo Ripper
model.Ripper = JRip(Species~., iris, subset=train)

summary(model.Ripper)


model.Ripper.pred = predict(model.Ripper, newdata = iris.test)

# Acierto
(sum(model.Ripper.pred == iris.test[,5])/nrow(iris.test)*100)


model.Ripper


# Aplico el algoritmo PART

model.Part = PART(Species~., iris, subset=train)
summary(model.Part)

model.Part.pred = predict(model.Part, newdata = iris.test)

# Acierto
(sum(model.Part.pred == iris.test[,5])/nrow(iris.test)*100)


model.Part


# Validacion Cruzada en iris sobre los 2 algoritmos anteriores
model.Ripper = JRip(Species~., iris)
cv_JRip = evaluate_Weka_classifier(model.Ripper,numFolds=nrow(iris))
# Acierto
cv_JRip$details[1]

model.Part = PART(Species~., iris)
cv_Part = evaluate_Weka_classifier(model.Part,numFolds=nrow(iris))
cv_Part$details[1]


# Ejercicio: Evaluemos los algoritmos anteriores con all CrossVal-one leave
# sobre la base de datos "Auto"

datos = Auto

# Pongo la variable "origin" (datos[,8]) como de tipo factor
datos[,8] = as.factor(datos[,8])

model.Ripper = JRip(origin~., datos)
cv_JRip = evaluate_Weka_classifier(model.Ripper,numFolds=nrow(datos))
# Acierto
cv_JRip$details[1]


model.Part = PART(origin~., datos)
cv_Part = evaluate_Weka_classifier(model.Part,numFolds=nrow(datos))
cv_Part$details[1]


# Experimento: Muestra una grafica que se vea la evolucion tanto de Ripper
# como de PART al aumentar el numero de particiones en la validacion 
# cruzada sobre la base de datos "Auto"



 ######################################################################
#               Usando bases de datos de Weka                         # 
######################################################################
library(RWeka)

a =system.file(package="RWeka")
list.files(a)

list.files("C:/Archivos de programa/R/R-3.1.2/library/RWeka/arff")

# Podemos encontrar mas bases de datos en
# http://repository.seasr.org/Datasets/UCI/arff/

# Nos descargamos la base de datos "wine" y la aniadimos a la
# carpeta "arff" del package "RWeka"

# Cargamos los datos
datos = read.arff(system.file("arff", "wine.arff",package = "RWeka"))

names(datos)
summary(datos)

# En esta base de datos consiste clasificar en 3 clases los vinos en base a
# parametros quimicos

# En esta base de datos, la variable de clasificacion es la primera
# Unicamente, fijamos que esta variable es la de clasificacion
datos[,1] = as.factor(datos[,1])

# Vamos a observar el comportamiento de los 2 algoritmos anteriores
# aplicando "all CrossVal one leave"

# CV con Ripper
model.Ripper = JRip(datos[,1]~., datos)
cv_JRip = evaluate_Weka_classifier(model.Ripper,numFolds=10)
# Acierto
cv_JRip$details[1]

# CV con PART
model.Part = PART(datos[1]~., datos)
cv_Part = evaluate_Weka_classifier(model.Part,numFolds=10)
# Acierto
cv_Part$details[1]


# Ademas de Ripper y PART, RWeka incluye otros 2 algoritmos de clasificacion
# de reglas
# OneR

model_OneR_iris = OneR(Species~.,data=iris, subset=train)

model_OneR_iris

model_OneR_iris_pred = predict(model_OneR_iris, iris.test)

# Construyo una funcion para calcular el acierto
acierto <- function(bag.datos){
  return (sum (sapply(1:length(bag.datos$y), function(x){
    if (is.na(bag.datos$predicted[x])){
      0
    } 
    else if (as.numeric(bag.datos$y[x])==as.numeric(bag.datos$predicted[x])){
      1
    }
    else{
      0
    }
  }))/length(bag.datos$y))
}


resul = as.data.frame(cbind(predicted = model_OneR_iris_pred, y=iris.test[,5]))

acierto(resul)


# Lo aplico tambien a la base de datos "wine"

# Si está en la carpeta asignada por el sistema descomento este
# wine = read.arff(system.file("arff", "wine.arff",package = "RWeka"))

# Si está en el directorio de trabajo actual uso este
wine = read.arff("wine.arff")

set.seed(9)
train_wine = sample(1:nrow(wine), 2*nrow(wine)/3)
wine.test = wine[-train_wine,]

model_OneR_wine = OneR(class~., data=wine, subset=train_wine)
model_OneR_wine_pred = predict(model_OneR_wine, wine.test)

resul = as.data.frame(cbind(predicted = model_OneR_wine_pred, y=wine.test[,1]))

acierto(resul)


# El otro algoritmo es M5Rules. Este algoritmo usa M5P, una version de
# C4.5 para construir un arbol, y luego transforma el arbol en un
# conjunto de reglas

# Requiere que la variable de clasificacion sea numerica
iris$Species = as.numeric(iris$Species)
model_M5R_iris = M5Rules(iris$Species~.,iris, subset=train)
model_M5R_iris
model_M5R_iris_pred = predict(model_M5R_iris, iris.test)
model_M5R_iris_pred
model_M5R_iris_pred = round(model_M5R_iris_pred)
model_M5R_iris_pred
resul = as.data.frame(cbind(predicted = model_M5R_iris_pred, y=iris.test[,5]))
acierto(resul)

# Ahora lo repito para la base de datos wine
wine$class = as.numeric(wine$class)
model_M5R_wine = M5Rules(class~.,wine, subset=train_wine)
model_M5R_wine
model_M5R_wine_pred = predict(model_M5R_wine, wine.test)
model_M5R_wine_pred
model_M5R_wine_pred = round(model_M5R_wine_pred)

resul = as.data.frame(cbind(predicted = model_M5R_wine_pred, y=wine.test[,1]))
acierto(resul)



