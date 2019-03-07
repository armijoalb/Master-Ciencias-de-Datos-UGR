## ------------------------------------------------------------------------
set.seed(42)
datos.train = read.csv('train.csv',na.strings = c("","NA","?"))
datos.test = read.csv('test.csv',na.strings = c("","NA","?"))

head(datos.train)

## ------------------------------------------------------------------------
require(ggplot2)

clase = datos.train$C
table(clase)


ggplot(datos.train, aes(as.factor(datos.train$C)))+geom_bar(color="black",fill="deepskyblue") + 
  xlab("Clase") + labs(title="Distribución de los datos de la clase")
  
  

## ------------------------------------------------------------------------
histogram_by = function(datos,var, bins=5){
  
  ggplot(datos,aes_string(x=var)) +
        geom_histogram(fill='lightblue', color="black", bins=bins)
}

histogram_by(datos.train,names(datos.train)[1])
lapply(names(datos.train)[1:50],histogram_by,datos=datos.train)

## ------------------------------------------------------------------------
summary(datos.train)

## ------------------------------------------------------------------------
posicionClase <- length(names(datos.train))
variableClase <- names(datos.train)[posicionClase]

library(party)
library(caret)

formulaClase <- as.formula(paste(variableClase,"~.",sep=""))

inTrain <- createDataPartition(y=datos.train[,posicionClase], p = .75, 
                               list = FALSE)

training <- datos.train[ inTrain,]
testing  <- datos.train[-inTrain,]

ct <- ctree(formulaClase, training)

plot(ct)
print(ct)
testPred <- predict(ct, newdata = testing, type="response")

testPred.results = testPred

# se compara con los datos de test
results <- table(testPred.results, testing$C)

# se suman los valores de la diagonal
sumDiag <- sum(diag(results))

# se suman todos los valores de la matriz
sumTotal <- sum(results)

# se calcula el porcentaje de aciertos
fiabilidad <- sumDiag/sumTotal
fiabilidad

# Se calcula el error
error <- 1-fiabilidad
error

## ------------------------------------------------------------------------
# Añadimos librería para trabajar con el ruido.
library(NoiseFiltersR)

datos.train$C = as.factor(datos.train$C)
out = IPF(formulaClase,data=datos.train)

# Comprobarmos si seguimos teniendo datos con ruido.
summary(out$cleanData)

## ------------------------------------------------------------------------
summary(datos.test)

## ------------------------------------------------------------------------
# Funciones para detectar outliers.
vector_es_outlier_IQR = function (datos, indice.de.columna, coef = 1.5){
  columna.datos = datos[,indice.de.columna]
  cuartil.primero = quantile(columna.datos)[2]  #quantile[1] es el m?nimo y quantile[5] el m?ximo.
  cuartil.tercero = quantile(columna.datos)[4] 
  iqr = cuartil.tercero - cuartil.primero
  extremo.superior.outlier = (iqr * coef) + cuartil.tercero
  extremo.inferior.outlier = cuartil.primero - (iqr * coef)
  es.outlier  = columna.datos > extremo.superior.outlier |
    columna.datos < extremo.inferior.outlier
  return (es.outlier)
}

vector_claves_outliers_IQR = function(datos, indice, coef = 1.5){
  columna.datos = datos[,indice]
  vector.de.outliers = vector_es_outlier_IQR(datos, indice, coef)
  return (which(vector.de.outliers  == TRUE))
}
v = 1:(ncol(datos.train)-1)
indices.outliers.en.alguna.columan = sapply( v, vector_claves_outliers_IQR,datos=na.omit(datos.train))

## ------------------------------------------------------------------------
Nombres_de_Filas = function (datos, vector_TF_datos_a_incluir) {
  numero.de.filas = nrow(datos)
  
  if (is.null(row.names(datos)))
    row.names(datos) = rep(1:numero.de.filas)
  
  nombres.de.filas = rep("", numero.de.filas)
  nombres.de.filas[vector_TF_datos_a_incluir==TRUE] = 
        row.names(datos)[vector_TF_datos_a_incluir==TRUE]
  return (nombres.de.filas)
}

MiBoxPlot_IQR_Univariate_Outliers = function (datos, indice.de.columna, coef = 1.5){
  # Importante: Para que aes busque los par?metros en el ?mbito local, 
  # debe incluirse  environment = environment()
  
  datos = as.data.frame(datos)
  vector.TF.outliers.IQR = vector_es_outlier_IQR(datos, indice.de.columna, coef)
  print(length(which(vector.TF.outliers.IQR == TRUE)))
  nombres.de.filas = Nombres_de_Filas(datos, vector.TF.outliers.IQR)
  nombre.de.columna = colnames(datos,indice.de.columna)
  
  ggboxplot = ggplot(data = datos, 
                     aes(x=factor(""), 
                     y=datos[,indice.de.columna]) , 
                     environment = environment()) + 
    xlab(nombre.de.columna) + ylab("") +
    geom_boxplot(outlier.colour = "red") +
    geom_text(aes(label = nombres.de.filas) , position = position_jitter(width = 0.1)) 
  
  #X11()
  ggboxplot
}

lapply(1:50,MiBoxPlot_IQR_Univariate_Outliers,datos=na.omit(datos.train))

## ------------------------------------------------------------------------
menor.que = function(data,indice){
  menores = which( data[,indice] < -60000 )
}
datos.minimos = sapply(1:50, menor.que, data=datos.train)
datos.minimos
head(datos.train[datos.minimos[[2]],],20)
tail(datos.train[datos.minimos[[2]],],-12)

## ------------------------------------------------------------------------
library(Amelia)
library(mice)

imputados = Amelia::amelia(datos.train,m=1, parallel="multicore",noms="C")
incompletas = mice::nic(imputados$imputations$imp1)

tail(imputados$imputations$imp1[datos.minimos[[2]],],12)

## ------------------------------------------------------------------------
formulaClase <- as.formula(paste(variableClase,"~.",sep=""))

inTrain <- createDataPartition(y=datos.train[,posicionClase], p = .75, 
                               list = FALSE)

training <- imputados$imputations$imp1[ inTrain,]
testing  <- imputados$imputations$imp1[-inTrain,]

ct <- ctree(formulaClase, training)

testPred <- predict(ct, newdata = testing, type="response")

testPred.results = testPred

# se compara con los datos de test
results <- table(testPred.results, testing$C)

# se suman los valores de la diagonal
sumDiag <- sum(diag(results))

# se suman todos los valores de la matriz
sumTotal <- sum(results)

# se calcula el porcentaje de aciertos
fiabilidad <- sumDiag/sumTotal
fiabilidad

# Se calcula el error
error <- 1-fiabilidad
error

## ------------------------------------------------------------------------
test.resultados = predict(ct, newdata=datos.test, type="response")
test.resultados

ids = 1:length(test.resultados)
resultados.test.con.imputacion.solo = data.frame(Id=ids,
                               Prediction=test.resultados)
head(resultados.test.con.imputacion.solo)
write.csv(resultados.test.con.imputacion.solo,
          file="./test-arboles-solo-imputacion1.csv",row.names=FALSE,quote = FALSE)

## ------------------------------------------------------------------------
datos.imputados = imputados$imputations$imp1
corrMatrix = cor(na.omit(datos.train[,-ncol(datos.train)]))
corrMatrix

# Obtenemos las variables altamente correladas
altamenteCorreladas = caret::findCorrelation(corrMatrix, cutoff = 0.8)
altamenteCorreladas

# Dataset con variables poco correladas con imputación de valores.
datos.imputados.sin.alta.correlacion = datos.imputados[,-altamenteCorreladas]

## ------------------------------------------------------------------------
library(GGally)
ggpairs(datos.imputados.sin.alta.correlacion)

## ------------------------------------------------------------------------
ggplot(data=datos.imputados.sin.alta.correlacion)+
  geom_bar(aes(x=C))
print(table(datos.imputados.sin.alta.correlacion$C))

ggplot(data=datos.imputados.sin.alta.correlacion)+
  geom_point(aes(x=X3,y=X12,colour=C))
ggplot(data=datos.imputados.sin.alta.correlacion)+
  geom_point(aes(x=X3,y=X16,colour=C))
ggplot(data=datos.imputados.sin.alta.correlacion)+
  geom_point(aes(x=X3,y=X18,colour=C))
ggplot(data=datos.imputados.sin.alta.correlacion)+
  geom_point(aes(x=X3,y=X23,colour=C))
ggplot(data=datos.imputados.sin.alta.correlacion)+
  geom_point(aes(x=X3,y=X27,colour=C))
ggplot(data=datos.imputados.sin.alta.correlacion)+
  geom_point(aes(x=X3,y=X34,colour=C))
ggplot(data=datos.imputados.sin.alta.correlacion)+
  geom_point(aes(x=X3,y=X42,colour=C))

## ------------------------------------------------------------------------
posicionClase <- length(names(datos.imputados.sin.alta.correlacion))
variableClase <- names(datos.imputados.sin.alta.correlacion)[posicionClase]

formulaClase <- as.formula(paste(variableClase,"~.",sep=""))

inTrain <- createDataPartition(y=datos.imputados.sin.alta.correlacion[,posicionClase], p = .75, 
                               list = FALSE)

training <- datos.imputados.sin.alta.correlacion[ inTrain,]
testing  <- datos.imputados.sin.alta.correlacion[-inTrain,]

ct <- ctree(formulaClase, training)

testPred <- predict(ct, newdata = testing, type="response")

testPred.results = testPred

# se compara con los datos de test
results <- table(testPred.results, testing$C)

# se suman los valores de la diagonal
sumDiag <- sum(diag(results))

# se suman todos los valores de la matriz
sumTotal <- sum(results)

# se calcula el porcentaje de aciertos
fiabilidad <- sumDiag/sumTotal

# Se calcula el error
error <- 1-fiabilidad
print(fiabilidad)
print(error)

## ------------------------------------------------------------------------
test.resultados = predict(ct, newdata=datos.test, type="response")
test.resultados

ids = 1:length(test.resultados)
resultados.test.con.imputacion.solo = data.frame(Id=ids,
                               Prediction=test.resultados)
head(resultados.test.con.imputacion.solo)
write.csv(resultados.test.con.imputacion.solo,
          file="./test-arboles-imputacion-y-correlacion2.csv",row.names=FALSE,quote = FALSE)

## ------------------------------------------------------------------------
library(imbalance)
# Hacemos oversampling
new_train = oversample(datos.imputados.sin.alta.correlacion,method="ADASYN",classAttr = "C")

imbalanceRatio(new_train,"C")

ggplot(data=new_train)+
  geom_point(aes(x=X3,y=X12,colour=C))
ggplot(data=new_train)+
  geom_point(aes(x=X3,y=X16,colour=C))
ggplot(data=new_train)+
  geom_point(aes(x=X3,y=X18,colour=C))
ggplot(data=new_train)+
  geom_point(aes(x=X3,y=X23,colour=C))
ggplot(data=new_train)+
  geom_point(aes(x=X3,y=X27,colour=C))
ggplot(data=new_train)+
  geom_point(aes(x=X3,y=X34,colour=C))
ggplot(data=new_train)+
  geom_point(aes(x=X3,y=X42,colour=C))

## ------------------------------------------------------------------------
posicionClase <- length(names(new_train))
variableClase <- names(new_train)[posicionClase]

formulaClase <- as.formula(paste(variableClase,"~.",sep=""))

inTrain <- createDataPartition(y=new_train[,posicionClase], p = .75, 
                               list = FALSE)
training <- new_train[ inTrain,]
testing  <- new_train[-inTrain,]

ct <- ctree(formulaClase, training)

testPred <- predict(ct, newdata = testing, type="response")

testPred.results = testPred

# se compara con los datos de test
results <- table(testPred.results, testing$C)

# se suman los valores de la diagonal
sumDiag <- sum(diag(results))

# se suman todos los valores de la matriz
sumTotal <- sum(results)

# se calcula el porcentaje de aciertos
fiabilidad <- sumDiag/sumTotal

# Se calcula el error
error <- 1-fiabilidad
print(fiabilidad)
print(error)

## ------------------------------------------------------------------------
test.resultados = predict(ct, newdata=datos.test, type="response")
test.resultados

ids = 1:length(test.resultados)
resultados.test = data.frame(Id=ids,
                               Prediction=test.resultados)
head(resultados.test)
write.csv(resultados.test,
          file="./test-arboles-oversampling-adasyn.csv",row.names=FALSE,quote = FALSE)

## ------------------------------------------------------------------------
posicionClase <- length(names(new_train))
variableClase <- names(new_train)[posicionClase]

formulaClase <- as.formula(paste(variableClase,"~.",sep=""))
out = IPF(formulaClase,data=new_train)

new_train = out$cleanData

## ------------------------------------------------------------------------
posicionClase <- length(names(new_train))
variableClase <- names(new_train)[posicionClase]

formulaClase <- as.formula(paste(variableClase,"~.",sep=""))

inTrain <- createDataPartition(y=new_train[,posicionClase], p = .75, 
                               list = FALSE)
training <- new_train[ inTrain,]
testing  <- new_train[-inTrain,]

ct <- ctree(formulaClase, training)

testPred <- predict(ct, newdata = testing, type="response")

testPred.results = testPred

# se compara con los datos de test
results <- table(testPred.results, testing$C)

# se suman los valores de la diagonal
sumDiag <- sum(diag(results))

# se suman todos los valores de la matriz
sumTotal <- sum(results)

# se calcula el porcentaje de aciertos
fiabilidad <- sumDiag/sumTotal

# Se calcula el error
error <- 1-fiabilidad
print(fiabilidad)
print(error)

## ------------------------------------------------------------------------
test.resultados = predict(ct, newdata=datos.test, type="response")
test.resultados

ids = 1:length(test.resultados)
resultados.test = data.frame(Id=ids,
                               Prediction=test.resultados)
head(resultados.test)
write.csv(resultados.test,
          file="./test-arboles-oversampling-e-ipf.csv",row.names=FALSE,quote = FALSE)

## ------------------------------------------------------------------------
library(unbalanced)
n = ncol(datos.imputados.sin.alta.correlacion)
output = datos.imputados.sin.alta.correlacion$C
input = datos.imputados.sin.alta.correlacion[,-n]

data = ubTomek(input,output)
new_train = cbind(data$X,C=data$Y)
head(new_train)

imbalanceRatio(new_train,"C")

ggplot(data=new_train)+
  geom_point(aes(x=X3,y=X12,colour=C))
ggplot(data=new_train)+
  geom_point(aes(x=X3,y=X16,colour=C))
ggplot(data=new_train)+
  geom_point(aes(x=X3,y=X18,colour=C))
ggplot(data=new_train)+
  geom_point(aes(x=X3,y=X23,colour=C))
ggplot(data=new_train)+
  geom_point(aes(x=X3,y=X27,colour=C))
ggplot(data=new_train)+
  geom_point(aes(x=X3,y=X34,colour=C))
ggplot(data=new_train)+
  geom_point(aes(x=X3,y=X42,colour=C))


ggplot(data=new_train)+
  geom_bar(aes(x=C))


## ------------------------------------------------------------------------
posicionClase <- length(names(new_train))
variableClase <- names(new_train)[posicionClase]

formulaClase <- as.formula(paste(variableClase,"~.",sep=""))

inTrain <- createDataPartition(y=new_train[,posicionClase], p = .75, 
                               list = FALSE)
training <- new_train[ inTrain,]
testing  <- new_train[-inTrain,]

ct <- ctree(formulaClase, training)

testPred <- predict(ct, newdata = testing, type="response")

testPred.results = testPred

# se compara con los datos de test
results <- table(testPred.results, testing$C)

# se suman los valores de la diagonal
sumDiag <- sum(diag(results))

# se suman todos los valores de la matriz
sumTotal <- sum(results)

# se calcula el porcentaje de aciertos
fiabilidad <- sumDiag/sumTotal

# Se calcula el error
error <- 1-fiabilidad
print(fiabilidad)
print(error)

## ------------------------------------------------------------------------
test.resultados = predict(ct, newdata=datos.test, type="response")
test.resultados

ids = 1:length(test.resultados)
resultados.test = data.frame(Id=ids,
                               Prediction=test.resultados)
head(resultados.test)
write.csv(resultados.test,
          file="./test-arboles-tomek-links.csv",row.names=FALSE,quote = FALSE)

## ------------------------------------------------------------------------
n = ncol(new_train)
output = new_train$C
input = new_train[,-n]

data = ubCNN(input,output)
new_train = cbind(data$X,C=data$Y)
head(new_train)

imbalanceRatio(new_train,"C")

## ------------------------------------------------------------------------
posicionClase <- length(names(new_train))
variableClase <- names(new_train)[posicionClase]

formulaClase <- as.formula(paste(variableClase,"~.",sep=""))

inTrain <- createDataPartition(y=new_train[,posicionClase], p = .75, 
                               list = FALSE)
training <- new_train[ inTrain,]
testing  <- new_train[-inTrain,]

ct <- ctree(formulaClase, training)

testPred <- predict(ct, newdata = testing, type="response")

testPred.results = testPred

# se compara con los datos de test
results <- table(testPred.results, testing$C)

# se suman los valores de la diagonal
sumDiag <- sum(diag(results))

# se suman todos los valores de la matriz
sumTotal <- sum(results)

# se calcula el porcentaje de aciertos
fiabilidad <- sumDiag/sumTotal

# Se calcula el error
error <- 1-fiabilidad
print(fiabilidad)
print(error)

## ------------------------------------------------------------------------
test.resultados = predict(ct, newdata=datos.test, type="response")
test.resultados

ids = 1:length(test.resultados)
resultados.test = data.frame(Id=ids,
                               Prediction=test.resultados)
head(resultados.test)
write.csv(resultados.test,
          file="./test-arboles-tomek-links-mas-cnn.csv",row.names=FALSE,quote = FALSE)

## ------------------------------------------------------------------------
ggplot(data=new_train)+
  geom_point(aes(x=X3,y=X12,colour=C))
ggplot(data=new_train)+
  geom_point(aes(x=X3,y=X16,colour=C))
ggplot(data=new_train)+
  geom_point(aes(x=X3,y=X18,colour=C))
ggplot(data=new_train)+
  geom_point(aes(x=X3,y=X23,colour=C))
ggplot(data=new_train)+
  geom_point(aes(x=X3,y=X27,colour=C))
ggplot(data=new_train)+
  geom_point(aes(x=X3,y=X34,colour=C))
ggplot(data=new_train)+
  geom_point(aes(x=X3,y=X42,colour=C))

## ------------------------------------------------------------------------

n = ncol(datos.imputados.sin.alta.correlacion)
output = datos.imputados.sin.alta.correlacion$C
input = datos.imputados.sin.alta.correlacion[,-n]

data = ubTomek(input,output)
new_train = cbind(data$X,C=data$Y)
head(new_train)

datos.minimos = sapply(1:ncol(new_train), menor.que, data=new_train)
head(datos.minimos)

new_train = new_train[-as.vector(datos.minimos[[2]]),]

ggplot(data=new_train)+
  geom_point(aes(x=X3,y=X12,colour=C))
ggplot(data=new_train)+
  geom_point(aes(x=X3,y=X16,colour=C))
ggplot(data=new_train)+
  geom_point(aes(x=X3,y=X18,colour=C))
ggplot(data=new_train)+
  geom_point(aes(x=X3,y=X23,colour=C))
ggplot(data=new_train)+
  geom_point(aes(x=X3,y=X27,colour=C))
ggplot(data=new_train)+
  geom_point(aes(x=X3,y=X34,colour=C))
ggplot(data=new_train)+
  geom_point(aes(x=X3,y=X42,colour=C))


## ------------------------------------------------------------------------
posicionClase <- length(names(new_train))
variableClase <- names(new_train)[posicionClase]

formulaClase <- as.formula(paste(variableClase,"~.",sep=""))

inTrain <- createDataPartition(y=new_train[,posicionClase], p = .75, 
                               list = FALSE)
training <- new_train[ inTrain,]
testing  <- new_train[-inTrain,]

ct <- ctree(formulaClase, training)

testPred <- predict(ct, newdata = testing, type="response")

testPred.results = testPred

# se compara con los datos de test
results <- table(testPred.results, testing$C)

# se suman los valores de la diagonal
sumDiag <- sum(diag(results))

# se suman todos los valores de la matriz
sumTotal <- sum(results)

# se calcula el porcentaje de aciertos
fiabilidad <- sumDiag/sumTotal

# Se calcula el error
error <- 1-fiabilidad
print(fiabilidad)
print(error)

