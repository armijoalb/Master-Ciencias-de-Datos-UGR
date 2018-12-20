Acierto <- function(y1,y2){
  return (sum (sapply(1:length(y1), function(x){
    if (is.na(y2[x])){
      0
    } 
    else if (as.numeric(y1[x])==as.numeric(y2[x])){
      1
    }
    else{
      0
    }
  }))/length(y1))
}


library(ISLR)
library(splines)
setwd("C:/Users/Alberto Armijo Ruiz/Universidad/Minería de datos. Preprocesamiento y clasificacion/Laboratorio/Sesión 3")
bd <- read.csv("CoordenadasMunicipios.csv", header = T, sep = ",")
bd[,1] = as.numeric(bd[,1])

datos = as.data.frame(cbind(y= bd[,1], x1 = bd$longitud, x2 = bd$latitud))


set.seed(9)
muestra = sample(1:nrow(datos), 100)
train = as.data.frame(datos[-muestra, ])
test = as.data.frame(datos[muestra, ])

# modelo regresion logistica
d1 = as.data.frame(cbind(y= as.numeric(I(train$y==1)), x1 =train$x1, x2 =train$x2))
d2 = as.data.frame(cbind(y= as.numeric(I(train$y==2)), x1 =train$x1, x2 =train$x2))
d3 = as.data.frame(cbind(y= as.numeric(I(train$y==3)), x1 =train$x1, x2 =train$x2))
d4 = as.data.frame(cbind(y= as.numeric(I(train$y==4)), x1 =train$x1, x2 =train$x2))
d5 = as.data.frame(cbind(y= as.numeric(I(train$y==5)), x1 =train$x1, x2 =train$x2))
d6 = as.data.frame(cbind(y= as.numeric(I(train$y==6)), x1 =train$x1, x2 =train$x2))
d7 = as.data.frame(cbind(y= as.numeric(I(train$y==7)), x1 =train$x1, x2 =train$x2))
d8 = as.data.frame(cbind(y= as.numeric(I(train$y==8)), x1 =train$x1, x2 =train$x2))

mRL1 <- glm(y ~ ns(x1,4) + ns(x2,4) , data = d1)
mRL2 <- glm(y ~ ns(x1,4) + ns(x2,4) , data = d2)
mRL3 <- glm(y ~ ns(x1,4) + ns(x2,4) , data = d3)
mRL4 <- glm(y ~ ns(x1,4) + ns(x2,4) , data = d4)
mRL5 <- glm(y ~ ns(x1,4) + ns(x2,4) , data = d5)
mRL6 <- glm(y ~ ns(x1,4) + ns(x2,4) , data = d6)
mRL7 <- glm(y ~ ns(x1,4) + ns(x2,4) , data = d7)
mRL8 <- glm(y ~ ns(x1,4) + ns(x2,4) , data = d8)

SmRL <- cbind (predict(mRL1, newdata = train, type="response"), 
               predict(mRL2, newdata = train, type="response"),
               predict(mRL3, newdata = train, type="response"),
               predict(mRL4, newdata = train, type="response"),
               predict(mRL5, newdata = train, type="response"),
               predict(mRL6, newdata = train, type="response"),
               predict(mRL7, newdata = train, type="response"),
               predict(mRL8, newdata = train, type="response"))


salida = sapply(1:nrow(SmRL), function(x) {which.max(SmRL[x,])})

Acierto(train$y,salida)

SmRLTest <- cbind (predict(mRL1, newdata = test, type="response"), 
               predict(mRL2, newdata = test, type="response"),
               predict(mRL3, newdata = test, type="response"),
               predict(mRL4, newdata = test, type="response"),
               predict(mRL5, newdata = test, type="response"),
               predict(mRL6, newdata = test, type="response"),
               predict(mRL7, newdata = test, type="response"),
               predict(mRL8, newdata = test, type="response"))


salidaTest = sapply(1:nrow(SmRLTest), function(x) {which.max(SmRLTest[x,])})

Acierto(test$y,salidaTest)

# Pintado del resultado




# Pintado de espacios de decision
grid.lines = 200
x.pred <- seq(min(datos$x2), max(datos$x2), length.out = grid.lines)
z.pred <- seq(min(datos$x1), max(datos$x1), length.out = grid.lines)
xz <- expand.grid( x2 = x.pred, x1 = z.pred)
SmRLTotal <- cbind (predict(mRL1, newdata = xz, type="response"), 
               predict(mRL2, newdata = xz, type="response"),
               predict(mRL3, newdata = xz, type="response"),
               predict(mRL4, newdata = xz, type="response"),
               predict(mRL5, newdata = xz, type="response"),
               predict(mRL6, newdata = xz, type="response"),
               predict(mRL7, newdata = xz, type="response"),
               predict(mRL8, newdata = xz, type="response"))
salidaTotal1 = sapply(1:nrow(SmRLTotal), function(x) {which.max(SmRLTotal[x,])})


plot(xz[,2], xz[,1], col= salidaTotal1, pch='*')
points(datos$x1, datos$x2, col=datos$y, pch = "x")


# modelo arbol (Tipo CART)

library(tree)

train[,1] = as.factor(train[,1])
mT <- tree(y ~ x1 + x2 , data = train)


smt = predict(mT,newdata=train, type="class")
Acierto(train$y, smt)

plot(train$x1,train$x2,col=train$y)
points(train$x1, train$x2, col=train$y, pch = "x")

smtTest = predict(mT, newdata=test, type="class")
Acierto(test$y,smtTest)

salidaTotal2 = predict(mT, newdata = xz,type="class")
plot(xz[,2], xz[,1],col=salidaTotal2,pch='*')
points(datos$x1,datos$x2,col=datos$y, pch='x')

# Random Forest
library (randomForest)

mRF= randomForest(y ~ x1 + x2, data=train)

smRF = predict(mRF, newdata=train,type="class")
Acierto(train$y,smRF)

smRFTest = predict(mRF, newdata=test, type="class")
Acierto(test$y,smRFTest)

salidaTotal2 = predict(mRF, newdata=xz, type="class")
plot(xz[,2], xz[,1],col=salidaTotal2,pch='*')
points(datos$x1,datos$x2,col=datos$y, pch='x')


# Boosting
library (gbm)

mGBM = gbm(y ~ x1+x2,data=train, n.trees = 500, interaction.depth = 4)
smGBM = predict(mGBM, newdata=train, n.trees = 500)
length(smGBM)/nrow(train)
smGBM = matrix(smGBM, ncol=length(smGBM)/nrow(train))
ncol(smGBM)


smGBM.y = sapply(1:nrow(smGBM), function(x){
  which.max(smGBM[x,])
})


(sum(smGBM.y == as.numeric(train$y))/nrow(train))*100

smGBMTest = predict(mGBM,newdata = test, n.trees = 500)
smGBMTest = data.frame(smGBMTest)
smGBMTest.y = sapply(1:nrow(smGBMTest), function(x){
  which.max(smGBMTest[x,])
})

(sum(smGBMTest.y == as.numeric(test$y))/nrow(test))*100

salidaTotal3 = predict(mGBM, newdata=xz, n.trees = 500)
salidaTotal3 = data.frame(salidaTotal3)
salidaTotal3 = sapply(1:nrow(salidaTotal3), function(x){
  which.max(salidaTotal3[x,])
})
plot(xz[,2], xz[,1],col=salidaTotal3,pch='*')
points(datos$x1,datos$x2,col=datos$y, pch='x')

# Arboles (C4.5)
library(RWeka)






# Ripper





# Grado de coincidencia entre los clasificadores anteriores

