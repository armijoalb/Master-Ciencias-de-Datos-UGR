# Alberto Armijo Ruiz
# Pruebas realizadas para la asignatura preprocesamiento y clasificación. Práctica competición Kaggle.
# Este archivo se ha generado a traves de ficheros .Rmd

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

## ------------------------------------------------------------------------
set.seed(42)
datos.train = read.csv('train.csv',na.strings = c("","NA","?"))
datos.test = read.csv('test.csv',na.strings = c("","NA","?"))

datos.train$C = as.factor(datos.train$C)
head(datos.train)

## ------------------------------------------------------------------------
menor.que = function(data,indice){
  menores = which( data[,indice] < -60000 )
}
datos.minimos = sapply(1:50, menor.que, data=datos.train)
#datos.minimos
#head(datos.train[datos.minimos[[2]],],20)
#tail(datos.train[datos.minimos[[2]],],-12)

## ------------------------------------------------------------------------
datos.train = datos.train[-as.vector(datos.minimos[[2]]),]
datos.minimos = sapply(1:50, menor.que, data=datos.train)

library(MASS)
#indices = sapply(datos.train,is.numeric)
#datos.train[indices] = lapply(datos.train[indices],scale)


## ----echo=FALSE----------------------------------------------------------
library(ggplot2)
histogram_by = function(datos,var, bins=20){
  
  ggplot(datos,aes_string(x=var)) +
    geom_histogram(fill='lightblue', color="black", bins=bins)
}

lapply(names(datos.train)[1:50],histogram_by,datos=datos.train)

## ------------------------------------------------------------------------
posicionClase <- length(names(datos.train))
variableClase <- names(datos.train)[posicionClase]

library(party)
library(caret)

formulaClase <- as.formula(paste(variableClase,"~.",sep=""))

inTrain <- createDataPartition(y=datos.train[,posicionClase], p = .8, 
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
print(fiabilidad)

# Se calcula el error
error <- 1-fiabilidad
print(error)

## ------------------------------------------------------------------------
library(rpart)
r.tree = rpart(formulaClase,data=training,method="class")
print(r.tree)

testPred = predict(r.tree,newdata = testing, type="class")
results = table(testPred,testing$C)

sumDiag = sum(diag(results))
sumTotal = sum(results)
fiabilidad = sumDiag/sumTotal
error = 1-fiabilidad
print(fiabilidad)
print(error)

printcp(r.tree)
p.tree = prune(r.tree,
               cp=r.tree$cptable[which.min(r.tree$cptable[,"xerror"]),"CP"])

testPred.pruned = predict(p.tree, newdata=testing, type="class")
results = table(testPred.pruned, testing$C)
sumDiag = sum(diag(results))
sumTotal = sum(results)
fiabilidad = sumDiag/sumTotal
error = 1-fiabilidad
print(fiabilidad)
print(error)

## ------------------------------------------------------------------------
# Predicción datos test
test.resultados = predict(r.tree, newdata=datos.test, type = "class")


ids = 1:length(test.resultados)
resultados.test = data.frame(Id=ids,
                             Prediction=test.resultados)
head(resultados.test)
write.csv(resultados.test,
          file="./test-arboles-rpart2.csv",row.names=FALSE,quote = FALSE)

## ------------------------------------------------------------------------
# Predicción datos test
test.resultados = predict(p.tree, newdata=datos.test, type="class")

ids = 1:length(test.resultados)
resultados.test = data.frame(Id=ids,
                             Prediction=test.resultados)
head(resultados.test)
write.csv(resultados.test,
          file="./test-arboles-rpart-pruned.csv",row.names=FALSE,quote = FALSE)

## ------------------------------------------------------------------------
calculateErrorAndAccuracy=function(data.predicted, real.data){
  results = table(data.predicted,real.data)
  sumDiag = sum(diag(results))
  sumTotal = sum(results)
  fiabilidad = sumDiag/sumTotal
  error = 1-fiabilidad
  print(fiabilidad)
  print(error) 
}

## ------------------------------------------------------------------------
library(tree)

fit.tree = tree(formulaClase,data=training)
tree.pred = predict(fit.tree,newdata=testing,type="class")
calculateErrorAndAccuracy(tree.pred,testing$C)

cv.tree = cv.tree(fit.tree,FUN=prune.misclass)
pruned.tree = prune.misclass(fit.tree,best=5)
cvtree.pred = predict(pruned.tree,newdata=testing,type="class")
calculateErrorAndAccuracy(cvtree.pred,testing$C)

test.resultados = predict(pruned.tree, newdata=datos.test, type="class")
ids = 1:length(test.resultados)
resultados.test = data.frame(Id=ids,
                             Prediction=test.resultados)
head(resultados.test)
write.csv(resultados.test,
          file="./test-arboles-ctree-pruned.csv",row.names=FALSE,quote = FALSE)

## ------------------------------------------------------------------------
library(Amelia)
library(mice)

cat("número de valores perdidos antes de imputar",
    mice::nic(datos.train))
imputados = amelia(datos.train,m=1,parallel="multicore",noms="C")
cat("número de valores perdidios tras la imputación",
    mice::nic(imputados$imputations$imp1))

datos.imputados = imputados$imputations$imp1

## ------------------------------------------------------------------------

posicionClase <- length(names(datos.imputados))
variableClase <- names(datos.imputados)[posicionClase]

formulaClase <- as.formula(paste(variableClase,"~.",sep=""))

inTrain <- createDataPartition(y=datos.imputados[,posicionClase], p = .8, 
                               list = FALSE)

training <- datos.imputados[ inTrain,]
testing  <- datos.imputados[-inTrain,]

r.tree.imputados = rpart(formulaClase,data=training,method="class")
print(r.tree.imputados)

testPred = predict(r.tree.imputados,newdata = testing, type="class")
results = table(testPred,testing$C)

sumDiag = sum(diag(results))
sumTotal = sum(results)
fiabilidad = sumDiag/sumTotal
error = 1-fiabilidad
print(fiabilidad)
print(error)

printcp(r.tree)
p.tree = prune(r.tree,
               cp=r.tree$cptable[which.min(r.tree$cptable[,"xerror"]),"CP"])

testPred.pruned = predict(p.tree, newdata=testing, type="class")
results = table(testPred.pruned, testing$C)
sumDiag = sum(diag(results))
sumTotal = sum(results)
fiabilidad = sumDiag/sumTotal
error = 1-fiabilidad
print(fiabilidad)
print(error)

## ------------------------------------------------------------------------
test.resultados = predict(r.tree.imputados, newdata=datos.test, type="class")

ids = 1:length(test.resultados)
resultados.test = data.frame(Id=ids,
                             Prediction=test.resultados)
head(resultados.test)
write.csv(resultados.test,
          file="./test-arboles-rpart-imputacion-Amelia.csv",row.names=FALSE,quote = FALSE)

## ------------------------------------------------------------------------
library(unbalanced)
library(imbalance)
n = ncol(datos.imputados)
output = datos.imputados$C
input = datos.imputados[,-n]

data = ubTomek(input,output)
new_train = cbind(data$X,C=data$Y)
head(new_train)

cat(imbalanceRatio(new_train,"C"))

## ------------------------------------------------------------------------
posicionClase <- length(names(new_train))
variableClase <- names(new_train)[posicionClase]

formulaClase <- as.formula(paste(variableClase,"~.",sep=""))

inTrain <- createDataPartition(y=new_train[,posicionClase], p = .8, 
                               list = FALSE)

training <- new_train[ inTrain,]
testing  <- new_train[-inTrain,]

r.tree.tomek.links = rpart(formulaClase,data=training,method="class")
print(r.tree.tomek.links)

testPred = predict(r.tree.tomek.links,newdata = testing, type="class")
calculateErrorAndAccuracy(testPred,testing$C)

## ------------------------------------------------------------------------
r.tree.tomek.links = rpart(formulaClase,data=new_train,method="class")

# Calculamos salida.
test.resultados = predict(r.tree.tomek.links, newdata=datos.test, type="class")

ids = 1:length(test.resultados)
resultados.test = data.frame(Id=ids,
                             Prediction=test.resultados)
head(resultados.test)
write.csv(resultados.test,
          file="./test-arboles-rpart-tomek-links.csv",row.names=FALSE,quote = FALSE)

## ------------------------------------------------------------------------
library(NoiseFiltersR)
posicionClase <- length(names(new_train))
variableClase <- names(new_train)[posicionClase]

formulaClase <- as.formula(paste(variableClase,"~.",sep=""))

out = IPF(formulaClase,data=new_train)

# Comprobarmos si seguimos teniendo datos con ruido.
summary(out$cleanData)

new_train = out$cleanData

## ------------------------------------------------------------------------
posicionClase <- length(names(new_train))
variableClase <- names(new_train)[posicionClase]

formulaClase <- as.formula(paste(variableClase,"~.",sep=""))

inTrain <- createDataPartition(y=new_train[,posicionClase], p = .8, 
                               list = FALSE)

training <- new_train[ inTrain,]
testing  <- new_train[-inTrain,]

r.tree.ipf = rpart(formulaClase,data=training,method="class")
print(r.tree.ipf)

testPred = predict(r.tree.ipf,newdata = testing, type="class")
calculateErrorAndAccuracy(testPred,testing$C)

## ------------------------------------------------------------------------
r.tree.ipf = rpart(formulaClase,data=new_train,method="class")

# Calculamos salida.
test.resultados = predict(r.tree.ipf, newdata=datos.test, type="class")

ids = 1:length(test.resultados)
resultados.test = data.frame(Id=ids,
                             Prediction=test.resultados)
head(resultados.test)
write.csv(resultados.test,
          file="./test-arboles-rpart-ipf.csv",row.names=FALSE,quote = FALSE)

## ------------------------------------------------------------------------
new_train = datos.imputados
posicionClase <- length(names(new_train))
variableClase <- names(new_train)[posicionClase]

formulaClase <- as.formula(paste(variableClase,"~.",sep=""))

out = IPF(formulaClase,data=new_train)

# Comprobarmos si seguimos teniendo datos con ruido.
summary(out$cleanData)

new_train = out$cleanData

## ------------------------------------------------------------------------
posicionClase <- length(names(new_train))
variableClase <- names(new_train)[posicionClase]

formulaClase <- as.formula(paste(variableClase,"~.",sep=""))

inTrain <- createDataPartition(y=new_train[,posicionClase], p = .8, 
                               list = FALSE)

training <- new_train[ inTrain,]
testing  <- new_train[-inTrain,]

r.tree.ipf = rpart(formulaClase,data=training,method="class")
print(r.tree.ipf)

testPred = predict(r.tree.ipf,newdata = testing, type="class")
calculateErrorAndAccuracy(testPred,testing$C)

## ------------------------------------------------------------------------
r.tree.ipf = rpart(formulaClase,data=new_train,method="class")

# Calculamos salida.
test.resultados = predict(r.tree.ipf, newdata=datos.test, type="class")

ids = 1:length(test.resultados)
resultados.test = data.frame(Id=ids,
                             Prediction=test.resultados)
head(resultados.test)
write.csv(resultados.test,
          file="./test-arboles-rpart-ipf-solo.csv",row.names=FALSE,quote = FALSE)

## ------------------------------------------------------------------------
library(imbalance)
imbalanceRatio(new_train,"C")

over = imbalance::oversample(new_train,ratio=0.65, method="SMOTE",classAttr = "C")
summary(over)

ggplot(over)+
  geom_bar(aes(x=C))

imbalanceRatio(over,"C")

## ------------------------------------------------------------------------
new_train = over
posicionClase <- length(names(new_train))
variableClase <- names(new_train)[posicionClase]

formulaClase <- as.formula(paste(variableClase,"~.",sep=""))

inTrain <- createDataPartition(y=new_train[,posicionClase], p = .8, 
                               list = FALSE)

training <- new_train[ inTrain,]
testing  <- new_train[-inTrain,]

r.tree.smote = rpart(formulaClase,data=training,method="class")
print(r.tree.ipf)

testPred = predict(r.tree.smote,newdata = testing, type="class")
calculateErrorAndAccuracy(testPred,testing$C)

## ------------------------------------------------------------------------
r.tree.smote = rpart(formulaClase,data=new_train,method="class")

# Calculamos salida.
test.resultados = predict(r.tree.smote, newdata=datos.test, type="class")

ids = 1:length(test.resultados)
resultados.test = data.frame(Id=ids,
                             Prediction=test.resultados)
head(resultados.test)
write.csv(resultados.test,
          file="./test-arboles-rpart-ipf-smote.csv",row.names=FALSE,quote = FALSE)

## ------------------------------------------------------------------------
posicionClase <- length(names(new_train))
variableClase <- names(new_train)[posicionClase]

formulaClase <- as.formula(paste(variableClase,"~.",sep=""))

out = IPF(formulaClase,data=new_train)

# Comprobarmos si seguimos teniendo datos con ruido.
summary(out$cleanData)

new_train = out$cleanData

## ------------------------------------------------------------------------
posicionClase <- length(names(new_train))
variableClase <- names(new_train)[posicionClase]

formulaClase <- as.formula(paste(variableClase,"~.",sep=""))

inTrain <- createDataPartition(y=new_train[,posicionClase], p = .8, 
                               list = FALSE)

training <- new_train[ inTrain,]
testing  <- new_train[-inTrain,]

r.tree.smote.ipf = rpart(formulaClase,data=training,method="class")
print(r.tree.smote.ipf)

testPred = predict(r.tree.smote.ipf,newdata = testing, type="class")
calculateErrorAndAccuracy(testPred,testing$C)

## ------------------------------------------------------------------------
r.tree.smote.ipf = rpart(formulaClase,data=new_train,method="class")

# Calculamos salida.
test.resultados = predict(r.tree.smote.ipf, newdata=datos.test, type="class")

ids = 1:length(test.resultados)
resultados.test = data.frame(Id=ids,
                             Prediction=test.resultados)
head(resultados.test)
write.csv(resultados.test,
          file="./test-arboles-rpart-ipf-smote2.csv",row.names=FALSE,quote = FALSE)

## ------------------------------------------------------------------------
aux = new_train
aux$C = as.numeric(aux$C)
corrMatrix = cor(aux)

# Obtenemos las variables altamente correladas
altamenteCorreladas = caret::findCorrelation(corrMatrix, cutoff = 0.8)
altamenteCorreladas

# Dataset con variables poco correladas con imputación de valores.
new_train = new_train[,-altamenteCorreladas]

## ------------------------------------------------------------------------
posicionClase <- length(names(new_train))
variableClase <- names(new_train)[posicionClase]

formulaClase <- as.formula(paste(variableClase,"~.",sep=""))

inTrain <- createDataPartition(y=new_train[,posicionClase], p = .8, 
                               list = FALSE)

training <- new_train[ inTrain,]
testing  <- new_train[-inTrain,]

r.tree.correlation = rpart(formulaClase,data=training,method="class")
print(r.tree.correlation)

testPred = predict(r.tree.correlation,newdata = testing, type="class")
calculateErrorAndAccuracy(testPred,testing$C)

## ------------------------------------------------------------------------
#r.tree.smote.ipf = rpart(formulaClase,data=new_train,method="class")

# Calculamos salida.
test.resultados = predict(r.tree.correlation, newdata=datos.test, type="class")

ids = 1:length(test.resultados)
resultados.test = data.frame(Id=ids,
                             Prediction=test.resultados)
head(resultados.test)
write.csv(resultados.test,
          file="./test-arboles-rpart-without-correlation.csv",row.names=FALSE,quote = FALSE)

## ------------------------------------------------------------------------
aux = na.omit(datos.train)
aux$C = as.numeric(aux$C)
corrMatrix = cor(aux)
corrMatrix

# Obtenemos las variables altamente correladas
altamenteCorreladas = caret::findCorrelation(corrMatrix, cutoff = 0.8)
altamenteCorreladas

# Dataset con variables poco correladas con imputación de valores.
new_train = new_train[,-altamenteCorreladas]

## ------------------------------------------------------------------------
posicionClase <- length(names(new_train))
variableClase <- names(new_train)[posicionClase]

formulaClase <- as.formula(paste(variableClase,"~.",sep=""))

inTrain <- createDataPartition(y=new_train[,posicionClase], p = .8, 
                               list = FALSE)

training <- new_train[ inTrain,]
testing  <- new_train[-inTrain,]

r.tree.correlation = rpart(formulaClase,data=training,method="class")
print(r.tree.correlation)

testPred = predict(r.tree.correlation,newdata = testing, type="class")
calculateErrorAndAccuracy(testPred,testing$C)

## ------------------------------------------------------------------------
r.tree.correlation = rpart(formulaClase,data=new_train,method="class")

# Calculamos salida.
test.resultados = predict(r.tree.correlation, newdata=datos.test, type="class")

ids = 1:length(test.resultados)
resultados.test = data.frame(Id=ids,
                             Prediction=test.resultados)
head(resultados.test)
write.csv(resultados.test,
          file="./test-arboles-rpart-without-correlation2.csv",row.names=FALSE,quote = FALSE)

## ------------------------------------------------------------------------
n = ncol(datos.imputados)
output = datos.imputados$C
input = datos.imputados[,-n]

data = ubTomek(input,output)
new_train = cbind(data$X,C=data$Y)
head(new_train)

## ------------------------------------------------------------------------
imbalanceRatio(new_train,classAttr = "C")
new_train = imbalance::oversample(new_train,method="ADASYN",classAttr = "C")
imbalanceRatio(new_train,"C")

## ------------------------------------------------------------------------
posicionClase <- length(names(new_train))
variableClase <- names(new_train)[posicionClase]

formulaClase <- as.formula(paste(variableClase,"~.",sep=""))

out = IPF(formulaClase,data=new_train)

# Comprobarmos si seguimos teniendo datos con ruido.
summary(out$cleanData)

new_train = out$cleanData

## ------------------------------------------------------------------------
posicionClase <- length(names(new_train))
variableClase <- names(new_train)[posicionClase]

formulaClase <- as.formula(paste(variableClase,"~.",sep=""))

trainDataCV = function(data,posicionClase,validation_number){
  inTrain <- createDataPartition(y=data[,posicionClase], p = .8, 
                                 list = FALSE)
  
  cat("Realizando validación",validation_number,"\n")
  
  training <- data[ inTrain,]
  testing  <- data[-inTrain,]
  
  tree.model = rpart(formulaClase,data=training,method="class")
  
  testPred = predict(tree.model,newdata = testing, type="class")
  resultados.test = calculateErrorAndAccuracy(testPred,testing$C)
  list(resultados=resultados.test,modelo=tree.model)
}

n = 1:10
cv = sapply(n,trainDataCV,data=new_train,posicionClase=posicionClase)
cv.results = mean(unlist(cv["resultados",]))
1-cv.results
which.min(unlist(cv["resultados",]))
best.model = cv["modelo",4]

## ------------------------------------------------------------------------
tree.ipf.smote.tomek = rpart(formulaClase,data=training,method="class")

# Calculamos salida.
test.resultados = predict(tree.ipf.smote.tomek, newdata=datos.test, type="class")

ids = 1:length(test.resultados)
resultados.test = data.frame(Id=ids,
                             Prediction=test.resultados)
head(resultados.test)
write.csv(resultados.test,
          file="./test-arboles-tomek-adasyn-ipf.csv",row.names=FALSE,quote = FALSE)

## ------------------------------------------------------------------------
lapply(names(new_train[1:50]),histogram_by,datos=new_train )

## ------------------------------------------------------------------------
aux = new_train[,c('X50','X15','X42','X40','X41','X46','X9','X35','X20','X2','X10','X31','X1','X37','X22','X6','X29','X30','X4','X28','X38','X19','C')]
new_train = aux

## ------------------------------------------------------------------------
posicionClase <- length(names(new_train))
variableClase <- names(new_train)[posicionClase]

formulaClase <- as.formula(paste(variableClase,"~.",sep=""))

n = 1:10
cv = sapply(n,trainDataCV,data=new_train,posicionClase=posicionClase)
cv.results = mean(unlist(cv["resultados",]))
1-cv.results

best.model = cv["modelo",which.min(unlist(cv["resultados",]))]
printcp(best.model$modelo)

## ------------------------------------------------------------------------
# Calculamos salida.
test.resultados = predict(best.model$modelo, newdata=datos.test, type="class")

ids = 1:length(test.resultados)
resultados.test = data.frame(Id=ids,
                             Prediction=test.resultados)
head(resultados.test)
write.csv(resultados.test,
          file="./test-arboles-tomek-adasyn-ipf-seleccion-1.csv",row.names=FALSE,quote = FALSE)

## ------------------------------------------------------------------------
new_train = datos.train

## ------------------------------------------------------------------------
posicionClase <- length(names(new_train))
variableClase <- names(new_train)[posicionClase]

formulaClase <- as.formula(paste(variableClase,"~.",sep=""))

n = 1:10
cv = sapply(n,trainDataCV,data=new_train,posicionClase=posicionClase)
cv.results = mean(unlist(cv["resultados",]))
cv.results

best.model = cv["modelo",which.min(unlist(cv["resultados",]))]
printcp(best.model$modelo)

## ------------------------------------------------------------------------
# Calculamos salida.
test.resultados = predict(best.model$modelo, newdata=datos.test, type="class")

ids = 1:length(test.resultados)
resultados.test = data.frame(Id=ids,
                             Prediction=test.resultados)
head(resultados.test)
write.csv(resultados.test,
          file="./test-arboles-rpart3.csv",row.names=FALSE,quote = FALSE)

## ------------------------------------------------------------------------
aux = na.omit(datos.train)
aux$C = as.numeric(aux$C)
corrMatrix = cor(aux)

# Obtenemos las variables altamente correladas
altamenteCorreladas = caret::findCorrelation(corrMatrix, cutoff = 0.8)
altamenteCorreladas

# Dataset con variables poco correladas con imputación de valores.
new_train = na.omit(datos.train[,-altamenteCorreladas])

## ------------------------------------------------------------------------
posicionClase <- length(names(new_train))
variableClase <- names(new_train)[posicionClase]

formulaClase <- as.formula(paste(variableClase,"~.",sep=""))

n = 1:10
cv = sapply(n,trainDataCV,data=new_train,posicionClase=posicionClase)
cv.results = mean(unlist(cv["resultados",]))
1-cv.results

best.model = cv["modelo",which.min(unlist(cv["resultados",]))]
printcp(best.model$modelo)

## ------------------------------------------------------------------------
# Calculamos salida.
test.resultados = predict(best.model$modelo, newdata=datos.test, type="class")

ids = 1:length(test.resultados)
resultados.test = data.frame(Id=ids,
                             Prediction=test.resultados)
head(resultados.test)
write.csv(resultados.test,
          file="./test-arboles-sin-correlacion.csv",row.names=FALSE,quote = FALSE)

## ------------------------------------------------------------------------
library(party)
library(ggplot2)
library(plyr)

trainct = ctree(C~.,data=datos.train)
trainct

ctree.varimp <- function(x,n = 0){
  if(is.null(x$psplit$variableName))
    return(NULL)
  
  res <- list(node = x$psplit$variableName, depth = n)
  
  c(list(res), ctree.varimp(x$left, n+1), ctree.varimp(x$right, n+1))
}

res <- ctree.varimp(trainct@tree)
res <- do.call(rbind, lapply(res, as.data.frame))
res$depth <- max(res$depth) + 1 - res$depth

res <- ddply(res, .(node), summarize, importancia = sum(depth))
res$node <- reorder( res$node, res$importancia, max )

ggplot(res, aes(x = node, weight = importancia)) + geom_bar() + 
  coord_flip() + ggtitle("Importancia de variables")

## ------------------------------------------------------------------------
new_train = read.csv('train.csv',na.strings = c("","NA","?"))
new_train$C = as.factor(new_train$C)

## ------------------------------------------------------------------------
posicionClase <- length(names(new_train))
variableClase <- names(new_train)[posicionClase]

formulaClase <- as.formula(paste(variableClase,"~.",sep=""))

n = 1:10
cv = sapply(n,trainDataCV,data=new_train,posicionClase=posicionClase)
cv.results = mean(unlist(cv["resultados",]))
cv.results

best.model = cv["modelo",which.min(unlist(cv["resultados",]))]
printcp(best.model$modelo)

## ------------------------------------------------------------------------
# Calculamos salida.
test.resultados = predict(best.model$modelo, newdata=datos.test, type="class")

ids = 1:length(test.resultados)
resultados.test = data.frame(Id=ids,
                             Prediction=test.resultados)
head(resultados.test)
write.csv(resultados.test,
          file="./test-arboles-sin-nada.csv",row.names=FALSE,quote = FALSE)

## ------------------------------------------------------------------------
new_train = datos.train
posicionClase <- length(names(new_train))
variableClase <- names(new_train)[posicionClase]

formulaClase <- as.formula(paste(variableClase,"~.",sep=""))

out = IPF(formulaClase,data=new_train)
new_train = out$cleanData

## ------------------------------------------------------------------------
posicionClase <- length(names(new_train))
variableClase <- names(new_train)[posicionClase]

formulaClase <- as.formula(paste(variableClase,"~.",sep=""))

n = 1:10
cv = sapply(n,trainDataCV,data=new_train,posicionClase=posicionClase)
cv.results = mean(unlist(cv["resultados",]))
1-cv.results

best.model = cv["modelo",which.min(unlist(cv["resultados",]))]
printcp(best.model$modelo)

## ------------------------------------------------------------------------
# Calculamos salida.
test.resultados = predict(best.model$modelo, newdata=datos.test, type="class")

ids = 1:length(test.resultados)
resultados.test = data.frame(Id=ids,
                             Prediction=test.resultados)
head(resultados.test)
write.csv(resultados.test,
          file="./test-arboles-solo-ipf-sin-imputacion.csv",row.names=FALSE,quote = FALSE)

## ------------------------------------------------------------------------
library(RWeka)
trainDataCVLMT = function(data,posicionClase,validation_number){
  inTrain <- createDataPartition(y=data[,posicionClase], p = .8, 
                                 list = FALSE)
  
  cat("Realizando validación",validation_number,"\n")
  
  training <- data[ inTrain,]
  testing  <- data[-inTrain,]
  
  tree.model = LMT(formulaClase,data=training)
  
  testPred = predict(tree.model,newdata = testing, type="class")
  resultados.test = calculateErrorAndAccuracy(testPred,testing$C)
  list(resultados=resultados.test,modelo=tree.model)
}
posicionClase <- length(names(new_train))
n = 1:10
cv = sapply(n,trainDataCVLMT,data=new_train,posicionClase=posicionClase)
cv.results = mean(unlist(cv["resultados",]))
1-cv.results

best.model = cv["modelo",which.min(unlist(cv["resultados",]))]

## ------------------------------------------------------------------------
# Calculamos salida.
test.resultados = predict(best.model$modelo, newdata=datos.test, type="class")

ids = 1:length(test.resultados)
resultados.test = data.frame(Id=ids,
                             Prediction=test.resultados)
head(resultados.test)
write.csv(resultados.test,
          file="./test-arboles-solo-ipf-sin-imputacion-y-lmts.csv",row.names=FALSE,quote = FALSE)

## ------------------------------------------------------------------------
# Hacemos validación.
imputados = Amelia::amelia(new_train,m=1, parallel="multicore",noms="C")
mice::nic(imputados$imputations$imp1)
new_train = imputados$imputations$imp1

## ------------------------------------------------------------------------
posicionClase <- length(names(new_train))
n = 1:10
cv = sapply(n,trainDataCVLMT,data=new_train,posicionClase=posicionClase)
cv.results = mean(unlist(cv["resultados",]))
1-cv.results

best.model = cv["modelo",which.min(unlist(cv["resultados",]))]

## ------------------------------------------------------------------------
# Calculamos salida.
test.resultados = predict(best.model$modelo, newdata=datos.test, type="class")

ids = 1:length(test.resultados)
resultados.test = data.frame(Id=ids,
                             Prediction=test.resultados)
head(resultados.test)
write.csv(resultados.test,
          file="./test-arboles-lmts-ipf-e-imputacion.csv",row.names=FALSE,quote = FALSE)

## ------------------------------------------------------------------------
aux = new_train
n = ncol(new_train)
output = new_train$C
input = new_train[,-n]

data = ubTomek(input,output)
new_train = cbind(data$X,C=data$Y)
head(new_train)

## ------------------------------------------------------------------------
posicionClase <- length(names(new_train))
n = 1:10
cv = sapply(n,trainDataCVLMT,data=new_train,posicionClase=posicionClase)
cv.results = mean(unlist(cv["resultados",]))
1-cv.results

best.model = cv["modelo",which.min(unlist(cv["resultados",]))]

## ------------------------------------------------------------------------
test.resultados = predict(best.model$modelo, newdata=datos.test, type="class")

ids = 1:length(test.resultados)
resultados.test = data.frame(Id=ids,
                             Prediction=test.resultados)
head(resultados.test)
write.csv(resultados.test,
          file="./test-arboles-lmts-ipf-imputacion-tomek.csv",row.names=FALSE,quote = FALSE)

## ------------------------------------------------------------------------
pairs(new_train[1:10],col=new_train$C)

## ------------------------------------------------------------------------
library(kernlab)
library(GGally)
# Normalizamos los datos.
aux_data = aux[,-ncol(aux)]
aux_data = as.data.frame(scale(aux_data))

# Obtenemos un valor para sigma utilizado en kpca (1/2*gamma²).
gamma2 = sum(apply(mi_training_data,2,var)) / ncol(mi_training_data)
mi_training_data = aux_data
gamma2 = sum(apply(mi_training_data,2,var)) / ncol(mi_training_data)
sigma = 1 / (2+gamma2)

# Hacemos kpca con un kernel radial para ver si separa mejor los datos.
kpc_prueba = kpca(~.,data=aux_data,kernel="rbfdot",kpar=list(sigma=sigma),features=10)
p = rotated(kpc_prueba)

labels = aux[,ncol(aux)]
kpca_training = cbind(as.data.frame(p),C=labels)
ggpairs(kpca_training, ggplot2::aes(colour=C))

## ------------------------------------------------------------------------
posicionClase <- length(names(kpca_training))
n = 1:10
cv = sapply(n,trainDataCVLMT,data=kpca_training,posicionClase=posicionClase)
cv.results = mean(unlist(cv["resultados",]))
1-cv.results

best.model = cv["modelo",which.min(unlist(cv["resultados",]))]

## ------------------------------------------------------------------------
posicionClase <- length(names(kpca_training))
n = 1:10
cv = sapply(n,trainDataCV,data=kpca_training,posicionClase=posicionClase)
cv.results = mean(unlist(cv["resultados",]))
cv.results

best.model = cv["modelo",which.min(unlist(cv["resultados",]))]

## ------------------------------------------------------------------------
new_train = datos.imputados
trainDataCVJ48 = function(data,posicionClase,validation_number){
  inTrain <- createDataPartition(y=data[,posicionClase], p = .8, 
                                 list = FALSE)
  
  cat("Realizando validación",validation_number,"\n")
  
  training <- data[ inTrain,]
  testing  <- data[-inTrain,]
  
  tree.model = J48(formulaClase,data=training)
  
  testPred = predict(tree.model,newdata = testing, type="class")
  resultados.test = calculateErrorAndAccuracy(testPred,testing$C)
  list(resultados=resultados.test,modelo=tree.model)
}
posicionClase <- length(names(new_train))
n = 1:10
cv = sapply(n,trainDataCVJ48,data=new_train,posicionClase=posicionClase)
cv.results = mean(unlist(cv["resultados",]))
1-cv.results

best.model = cv["modelo",which.min(unlist(cv["resultados",]))]

## ------------------------------------------------------------------------
test.resultados = predict(best.model$modelo, newdata=datos.test, type="class")

ids = 1:length(test.resultados)
resultados.test = data.frame(Id=ids,
                             Prediction=test.resultados)
head(resultados.test)
write.csv(resultados.test,
          file="./test-arboles-j48-solo-imputacion.csv",row.names=FALSE,quote = FALSE)

## ------------------------------------------------------------------------
posicionClase <- length(names(new_train))
variableClase <- names(new_train)[posicionClase]

formulaClase <- as.formula(paste(variableClase,"~.",sep=""))

out = IPF(formulaClase,data=new_train)

new_train = out$cleanData

## ------------------------------------------------------------------------
posicionClase <- length(names(new_train))
n = 1:10
cv = sapply(n,trainDataCVJ48,data=new_train,posicionClase=posicionClase)
cv.results = mean(unlist(cv["resultados",]))
1-cv.results

best.model = cv["modelo",which.min(unlist(cv["resultados",]))]

## ------------------------------------------------------------------------
test.resultados = predict(best.model$modelo, newdata=datos.test, type="class")

ids = 1:length(test.resultados)
resultados.test = data.frame(Id=ids,
                             Prediction=test.resultados)
head(resultados.test)
write.csv(resultados.test,
          file="./test-arboles-j48-imputacion-mas-ipf.csv",row.names=FALSE,quote = FALSE)

## ------------------------------------------------------------------------
posicionClase <- length(names(new_train))
n = 1:10
cv = sapply(n,trainDataCVLMT,data=new_train,posicionClase=posicionClase)
cv.results = mean(unlist(cv["resultados",]))
1-cv.results

best.model = cv["modelo",which.min(unlist(cv["resultados",]))]

## ------------------------------------------------------------------------
test.resultados = predict(best.model$modelo, newdata=datos.test, type="class")

ids = 1:length(test.resultados)
resultados.test = data.frame(Id=ids,
                             Prediction=test.resultados)
head(resultados.test)
write.csv(resultados.test,
          file="./test-arboles-lmts-imputacion-mas-ipf.csv",row.names=FALSE,quote = FALSE)

## ------------------------------------------------------------------------
aux = new_train
over = imbalance::oversample(aux,ratio=0.8, method="BLSMOTE",classAttr = "C")
imbalanceRatio(over,"C")
table(over$C)
new_train = over

## ------------------------------------------------------------------------
posicionClase <- length(names(new_train))
n = 1:10
cv = sapply(n,trainDataCVJ48,data=new_train,posicionClase=posicionClase)
cv.results = mean(unlist(cv["resultados",]))
1-cv.results

best.model = cv["modelo",which.min(unlist(cv["resultados",]))]

## ------------------------------------------------------------------------
test.resultados = predict(best.model$modelo, newdata=datos.test, type="class")

ids = 1:length(test.resultados)
resultados.test  = data.frame(Id=ids,
                              Prediction=test.resultados)
head(resultados.test)
write.csv(resultados.test,
          file="./test-arboles-j48-imputacion-ipf-blsmote.csv",row.names=FALSE,quote = FALSE)

## ------------------------------------------------------------------------
new_train = aux
n = ncol(new_train)
output = new_train$C
input = new_train[,-n]

data = ubTomek(input,output)
new_train = cbind(data$X,C=data$Y)
head(new_train)

## ------------------------------------------------------------------------
posicionClase <- length(names(new_train))
n = 1:10
cv = sapply(n,trainDataCVJ48,data=new_train,posicionClase=posicionClase)
cv.results = mean(unlist(cv["resultados",]))
1-cv.results

best.model = cv["modelo",which.min(unlist(cv["resultados",]))]

## ------------------------------------------------------------------------
test.resultados = predict(best.model$modelo, newdata=datos.test, type="class")

ids = 1:length(test.resultados)
resultados.test = data.frame(Id=ids,
                             Prediction=test.resultados)
head(resultados.test)
write.csv(resultados.test,
          file="./test-arboles-j48-imputacion-ipf-tomek-links.csv",row.names=FALSE,quote = FALSE)

## ------------------------------------------------------------------------
aux = new_train
over = imbalance::oversample(aux,ratio=0.8, method="BLSMOTE",classAttr = "C")
imbalanceRatio(over,"C")
table(over$C)
new_train = over

## ------------------------------------------------------------------------
posicionClase <- length(names(new_train))
n = 1:10
cv = sapply(n,trainDataCVJ48,data=new_train,posicionClase=posicionClase)
cv.results = mean(unlist(cv["resultados",]))
1-cv.results

best.model = cv["modelo",which.min(unlist(cv["resultados",]))]

## ------------------------------------------------------------------------
test.resultados = predict(best.model$modelo, newdata=datos.test, type="class")

ids = 1:length(test.resultados)
resultados.test = data.frame(Id=ids,
                             Prediction=test.resultados)
head(resultados.test)
write.csv(resultados.test,
          file="./test-arboles-j48-imputacion-ipf-tomek-links-mas-blsmote.csv",row.names=FALSE,quote = FALSE)

## ------------------------------------------------------------------------
posicionClase <- length(names(new_train))
variableClase <- names(new_train)[posicionClase]

formulaClase <- as.formula(paste(variableClase,"~.",sep=""))

out = IPF(formulaClase,data=new_train)

new_train = out$cleanData

posicionClase <- length(names(new_train))
n = 1:10
cv = sapply(n,trainDataCVJ48,data=new_train,posicionClase=posicionClase)
cv.results = mean(unlist(cv["resultados",]))
1-cv.results

best.model = cv["modelo",which.min(unlist(cv["resultados",]))]

test.resultados = predict(best.model$modelo, newdata=datos.test, type="class")

ids = 1:length(test.resultados)
resultados.test = data.frame(Id=ids,
                             Prediction=test.resultados)
head(resultados.test)
write.csv(resultados.test,
          file="./test-arboles-j48-imputacion-ipf-tomek-links-mas-blsmote-ipfdenuevo.csv",row.names=FALSE,quote = FALSE)

## ------------------------------------------------------------------------
library(caret)
new_train = datos.imputados
trans = preProcess(new_train[,-ncol(new_train)],
                   method=c("BoxCox","center","scale","pca"),pcaComp=15)
trainPC = predict(trans,new_train[,-ncol(new_train)])
trainPC = cbind(trainPC,C=new_train$C)
pairs(trainPC,col=new_train$C)

## ------------------------------------------------------------------------
library(Boruta)
boruta.bank_train <- Boruta(C~., data = new_train, doTrace = 2)
print(boruta.bank_train)

## ------------------------------------------------------------------------
new_train = datos.imputados
corrMatrix = cor(new_train[,-ncol(new_train)])
altamenteCorreladas = findCorrelation(corrMatrix,cutoff=0.8)
new_train = new_train[,-altamenteCorreladas]

## ------------------------------------------------------------------------
posicionClase <- length(names(new_train))
variableClase <- names(new_train)[posicionClase]

formulaClase <- as.formula(paste(variableClase,"~.",sep=""))

out = IPF(formulaClase,data=new_train)

new_train = out$cleanData

## ------------------------------------------------------------------------
aux = new_train
over = imbalance::oversample(aux,ratio=0.8, method="BLSMOTE",classAttr = "C")
imbalanceRatio(over,"C")
table(over$C)
new_train = over

## ------------------------------------------------------------------------
posicionClase <- length(names(new_train))
n = 1:10
cv = sapply(n,trainDataCVLMT,data=new_train,posicionClase=posicionClase)
cv.results = mean(unlist(cv["resultados",]))
1-cv.results

best.model = cv["modelo",which.min(unlist(cv["resultados",]))]

## ------------------------------------------------------------------------
test.resultados = predict(best.model$modelo, newdata=datos.test, type="class")

ids = 1:length(test.resultados)
resultados.test = data.frame(Id=ids,
                             Prediction=test.resultados)
head(resultados.test)
write.csv(resultados.test,
          file="./test-arboles-lmts-imputacion-sincorrelacion-ipf-blsmote.csv",row.names=FALSE,quote = FALSE)

## ------------------------------------------------------------------------
posicionClase <- length(names(new_train))
n = 1:10
cv = sapply(n,trainDataCVJ48,data=new_train,posicionClase=posicionClase)
cv.results = mean(unlist(cv["resultados",]))
1-cv.results

best.model = cv["modelo",which.min(unlist(cv["resultados",]))]

## ------------------------------------------------------------------------
test.resultados = predict(best.model$modelo, newdata=datos.test, type="class")

ids = 1:length(test.resultados)
resultados.test = data.frame(Id=ids,
                             Prediction=test.resultados)
head(resultados.test)
write.csv(resultados.test,
          file="./test-arboles-j48-imputacion-sincorrelacion-ipf-blsmote.csv",row.names=FALSE,quote = FALSE)

## ------------------------------------------------------------------------
n = ncol(new_train)
output = new_train$C
input = new_train[,-n]

data = ubTomek(input,output)
new_train = cbind(data$X,C=data$Y)
head(new_train)

## ------------------------------------------------------------------------
posicionClase <- length(names(new_train))
n = 1:10
cv = sapply(n,trainDataCVLMT,data=new_train,posicionClase=posicionClase)
cv.results = mean(unlist(cv["resultados",]))
1-cv.results

best.model = cv["modelo",which.min(unlist(cv["resultados",]))]

## ------------------------------------------------------------------------
test.resultados = predict(best.model$modelo, newdata=datos.test, type="class")

ids = 1:length(test.resultados)
resultados.test = data.frame(Id=ids,
                             Prediction=test.resultados)
head(resultados.test)
write.csv(resultados.test,
          file="./test-arboles-lmts-imputacion-sincorrelacion-ipf-blsmote-tomeklinks.csv",row.names=FALSE,quote = FALSE)

## ------------------------------------------------------------------------
posicionClase <- length(names(new_train))
n = 1:10
cv = sapply(n,trainDataCVJ48,data=new_train,posicionClase=posicionClase)
cv.results = mean(unlist(cv["resultados",]))
1-cv.results

best.model = cv["modelo",which.min(unlist(cv["resultados",]))]

## ------------------------------------------------------------------------
test.resultados = predict(best.model$modelo, newdata=datos.test, type="class")

ids = 1:length(test.resultados)
resultados.test = data.frame(Id=ids,
                             Prediction=test.resultados)
head(resultados.test)
write.csv(resultados.test,
          file="./test-arboles-j48-imputacion-sincorrelacion-ipf-blsmote-tomeklinks.csv",row.names=FALSE,quote = FALSE)

## ------------------------------------------------------------------------
set.seed(42)
datos.train = read.csv('train.csv',na.strings = c("","NA","?"))
datos.test = read.csv('test.csv',na.strings = c("","NA","?"))

datos.train$C = as.factor(datos.train$C)
head(datos.train)

## ------------------------------------------------------------------------
library(Amelia)
cat("número de valores perdidos antes de imputar",
    mice::nic(datos.train))
imputados = amelia(datos.train,m=1,parallel="multicore",noms="C")
cat("número de valores perdidios tras la imputación",
    mice::nic(imputados$imputations$imp1))

datos.imputados = imputados$imputations$imp1

## ------------------------------------------------------------------------
library(caret)
preproc = preProcess(datos.imputados[,-ncol(datos.imputados)],method=c("BoxCox","center","scale","pca"),pcaComp=20)
trainPCA = predict(preproc,datos.imputados[,-ncol(datos.imputados)])
trainPCA = cbind(trainPCA,C=datos.imputados$C)
# Hacemos lo mismo para test.
testPCA = predict(preproc,datos.test)

## ------------------------------------------------------------------------
library(RWeka)
trainDataCVLMT = function(data,posicionClase,validation_number){
  inTrain <- createDataPartition(y=data[,posicionClase], p = .8, 
                                 list = FALSE)
  
  cat("Realizando validación",validation_number,"\n")
  
  training <- data[ inTrain,]
  testing  <- data[-inTrain,]
  
  tree.model = LMT(formulaClase,data=training)
  
  testPred = predict(tree.model,newdata = testing, type="class")
  resultados.test = calculateErrorAndAccuracy(testPred,testing$C)
  list(resultados=resultados.test,modelo=tree.model)
}

trainDataCVJ48 = function(data,posicionClase,validation_number){
  inTrain <- createDataPartition(y=data[,posicionClase], p = .8, 
                                 list = FALSE)
  
  cat("Realizando validación",validation_number,"\n")
  
  training <- data[ inTrain,]
  testing  <- data[-inTrain,]
  
  tree.model = J48(formulaClase,data=training)
  
  testPred = predict(tree.model,newdata = testing, type="class")
  resultados.test = calculateErrorAndAccuracy(testPred,testing$C)
  list(resultados=resultados.test,modelo=tree.model)
}

## ------------------------------------------------------------------------
calculateErrorAndAccuracy=function(data.predicted, real.data){
  results = table(data.predicted,real.data)
  sumDiag = sum(diag(results))
  sumTotal = sum(results)
  fiabilidad = sumDiag/sumTotal
  error = 1-fiabilidad
  print(fiabilidad)
  print(error) 
}

## ------------------------------------------------------------------------
pairs(trainPCA,col=trainPCA$C)

## ------------------------------------------------------------------------
posicionClase = length(names(trainPCA))
variableClase = names(trainPCA)[posicionClase]
formulaClase = as.formula(paste(variableClase,"~.",sep=""))
n = 1:10
cv = sapply(n,trainDataCVLMT,data=trainPCA,posicionClase=posicionClase)
cv.results = mean(unlist(cv["resultados",]))
cv.results

best.model = cv["modelo",which.min(unlist(cv["resultados",]))]

## ------------------------------------------------------------------------
test.resultados = predict(best.model$modelo, newdata=testPCA, type="class")

ids = 1:length(test.resultados)
resultados.test = data.frame(Id=ids,
                             Prediction=test.resultados)
head(resultados.test)
write.csv(resultados.test,
          file="./test-arboles-lmt-imputacion-pca.csv",row.names=FALSE,quote = FALSE)

## ------------------------------------------------------------------------
posicionClase = length(names(datos.imputados))
variableClase = names(datos.imputados)[posicionClase]
formulaClase = as.formula(paste(variableClase,"~.",sep=""))
n = 1:10
cv = sapply(n,trainDataCVLMT,data=datos.imputados,posicionClase=posicionClase)
cv.results = mean(unlist(cv["resultados",]))
cv.results

## ------------------------------------------------------------------------
posicionClase = length(names(datos.imputados))
variableClase = names(datos.imputados)[posicionClase]
formulaClase = as.formula(paste(variableClase,"~.",sep=""))
n = 1:10
cv = sapply(n,trainDataCVJ48,data=datos.imputados,posicionClase=posicionClase)
cv.results = mean(unlist(cv["resultados",]))
cv.results

## ------------------------------------------------------------------------
posicionClase = length(names(trainPCA))
variableClase = names(trainPCA)[posicionClase]
formulaClase = as.formula(paste(variableClase,"~.",sep=""))
n = 1:10
cv = sapply(n,trainDataCVJ48,data=trainPCA,posicionClase=posicionClase)
cv.results = mean(unlist(cv["resultados",]))
cv.results
best.model = cv["modelo",which.min(unlist(cv["resultados",]))]

## ------------------------------------------------------------------------
test.resultados = predict(best.model$modelo, newdata=testPCA, type="class")

ids = 1:length(test.resultados)
resultados.test = data.frame(Id=ids,
                             Prediction=test.resultados)
head(resultados.test)
write.csv(resultados.test,
          file="./test-arboles-j48-imputacion-pca.csv",row.names=FALSE,quote = FALSE)

## ------------------------------------------------------------------------
library(NoiseFiltersR)
new_train = trainPCA
posicionClase <- length(names(new_train))
variableClase <- names(new_train)[posicionClase]

formulaClase <- as.formula(paste(variableClase,"~.",sep=""))

out = IPF(formulaClase,data=new_train)

# Comprobarmos si seguimos teniendo datos con ruido.
summary(out$cleanData)

new_train = out$cleanData
ipf_data = out$cleanData

## ------------------------------------------------------------------------
posicionClase = length(names(new_train))
variableClase = names(new_train)[posicionClase]
formulaClase = as.formula(paste(variableClase,"~.",sep=""))
n = 1:10
cv = sapply(n,trainDataCVJ48,data=new_train,posicionClase=posicionClase)
cv.results = mean(unlist(cv["resultados",]))
cv.results
best.model = cv["modelo",which.min(unlist(cv["resultados",]))]

cv2 = sapply(n,trainDataCVLMT,data=new_train,posicionClase=posicionClase)
cv2.results = mean(unlist(cv2["resultados",]))
cv2.results
best.model.lmt = cv2["modelo",which.min(unlist(cv2["resultados",]))]

## ------------------------------------------------------------------------
test.resultados = predict(best.model.lmt$modelo, newdata=testPCA, type="class")

ids = 1:length(test.resultados)
resultados.test = data.frame(Id=ids,
                             Prediction=test.resultados)
head(resultados.test)
write.csv(resultados.test,
          file="./test-arboles-lmt-imputacion-pca-ipf.csv",row.names=FALSE,quote = FALSE)


## ------------------------------------------------------------------------
test.resultados = predict(best.model$modelo, newdata=testPCA, type="class")

ids = 1:length(test.resultados)
resultados.test = data.frame(Id=ids,
                             Prediction=test.resultados)
head(resultados.test)
write.csv(resultados.test,
          file="./test-arboles-j48-imputacion-pca-ipf.csv",row.names=FALSE,quote = FALSE)


## ------------------------------------------------------------------------
library(unbalanced)
n = ncol(new_train)
output = new_train$C
input = new_train[,-n]

data = ubTomek(input,output)
new_train = cbind(data$X,C=data$Y)
head(new_train)

## ------------------------------------------------------------------------
library(imbalance)
aux = new_train
over = oversample(aux,ratio=0.8, method="BLSMOTE",classAttr = "C")
imbalanceRatio(over,"C")
table(over$C)
new_train = over

## ------------------------------------------------------------------------
posicionClase = length(names(new_train))
variableClase = names(new_train)[posicionClase]
formulaClase = as.formula(paste(variableClase,"~.",sep=""))
n = 1:10
cv = sapply(n,trainDataCVJ48,data=new_train,posicionClase=posicionClase)
cv.results = mean(unlist(cv["resultados",]))
cv.results
best.model = cv["modelo",which.min(unlist(cv["resultados",]))]

cv2 = sapply(n,trainDataCVLMT,data=new_train,posicionClase=posicionClase)
cv2.results = mean(unlist(cv2["resultados",]))
cv2.results
best.model.lmt = cv2["modelo",which.min(unlist(cv2["resultados",]))]

## ------------------------------------------------------------------------
test.resultados = predict(best.model$modelo, newdata=testPCA, type="class")

ids = 1:length(test.resultados)
resultados.test = data.frame(Id=ids,
                             Prediction=test.resultados)
head(resultados.test)
write.csv(resultados.test,
          file="./test-arboles-j48-imputacion-pca-ipf-tomek-blsmote.csv",row.names=FALSE,quote = FALSE)

## ------------------------------------------------------------------------
test.resultados = predict(best.model.lmt$modelo, newdata=testPCA, type="class")

ids = 1:length(test.resultados)
resultados.test = data.frame(Id=ids,
                             Prediction=test.resultados)
head(resultados.test)
write.csv(resultados.test,
          file="./test-arboles-lmt-imputacion-pca-ipf-tomek-blsmote.csv",row.names=FALSE,quote = FALSE)

## ------------------------------------------------------------------------
new_train = ipf_data
aux = new_train
over = oversample(aux,ratio=0.8, method="BLSMOTE",classAttr = "C")
imbalanceRatio(over,"C")
table(over$C)
new_train = over

## ------------------------------------------------------------------------
posicionClase = length(names(new_train))
variableClase = names(new_train)[posicionClase]
formulaClase = as.formula(paste(variableClase,"~.",sep=""))
n = 1:10
cv = sapply(n,trainDataCVJ48,data=new_train,posicionClase=posicionClase)
cv.results = mean(unlist(cv["resultados",]))
cv.results
best.model = cv["modelo",which.min(unlist(cv["resultados",]))]

## ------------------------------------------------------------------------
test.resultados = predict(best.model$modelo, newdata=testPCA, type="class")

ids = 1:length(test.resultados)
resultados.test = data.frame(Id=ids,
                             Prediction=test.resultados)
head(resultados.test)
write.csv(resultados.test,
          file="./test-arboles-j48-imputacion-pca-ipf-blsmote.csv",row.names=FALSE,quote = FALSE)

## ------------------------------------------------------------------------
library(kernlab)
aux = datos.imputados
kpc = kpca(~., data=aux[,-ncol(aux)],kernel="rbfdot",kpar=list(sigma=0.33),features=30)
data_kpc = as.data.frame(rotated(kpc))
data_kpc = cbind(data_kpc,C=as.factor(aux[,ncol(aux)]))
head(data_kpc)
pairs(data_kpc[1:10],col=data_kpc$C)

## ------------------------------------------------------------------------
posicionClase = length(names(data_kpc))
variableClase = names(data_kpc)[posicionClase]
formulaClase = as.formula(paste(variableClase,"~.",sep=""))
n = 1:10
cv = sapply(n,trainDataCVLMT,data=data_kpc,posicionClase=posicionClase)
cv.results = mean(unlist(cv["resultados",]))
cv.results
best.model.kpca = cv["modelo",which.min(unlist(cv["resultados",]))]

