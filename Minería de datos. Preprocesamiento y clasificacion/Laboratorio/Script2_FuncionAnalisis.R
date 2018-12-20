#Pasos a seguir para la confeccion del modelo
# 1. Analisis preliminar
# 1a. Grafico de pares
# 1b. Numerico: Coeficientes de correlacion
# 2. Construccion del modelo
# 2a. Definicion del modelo
# 3. Estimacion de la bondad del modelo construido
# 3a. Estimacion del sigma
# 3b. ANOVA
# 3c. Coeficiente de determinacion
# 3d. Capacidad de prediccion

library(ISLR)
library(bootstrap)
library(lmtest)
library(splines)
library(gam)




# Declaracion de funciones

# Calculo de la medida del error standar medio (MSE) y 
# el porcentaje de error sobre el rango
# La variable "y" esta en datos[,1]
MSE <- function(datos,regresion){
  yprime <- predict(regresion, datos)
  b <-sum((datos[,1]-yprime)^2)/length(yprime) ##MSE
  b <- as.vector(b)
  b[2]<- (b[1]/(range(datos[,1])[2]-range(datos[,1])[1]^2))*100
  return(b)
}



# Funciones para realizar la validacion cruzada
library(bootstrap)

theta.fit <- function(v,w,ff=model){
  a <- 0
  if (ff$call[1]=="lm()"){
    a <-lm(as.formula(ff$call$formula), data=as.data.frame(cbind(v,y=w)))
  }
  else{ if  (ff$call[1]=="gam()"){
    a <-gam(as.formula(ff$call$formula), data=as.data.frame(cbind(v,y=w)))
  }
  }  
  a
}

theta.predict <- function(fit, x) {
  if (is.null(dim(x))) {
    x <- t(x)
  }
  predict(fit, newdata=as.data.frame(x))
}


ValidacionCruzada <- function(datos, particiones, model){
  a <- crossval(datos[,-1], datos[,1], theta.fit, theta.predict, ngroup=particiones, ff=model)
  b <- (sum(abs(a$cv.fit - datos$y)^2)/length(datos$y)) ##MSE
  
  #Porcentaje de error sobre el rango
  c <- sum(abs(a$cv.fit - datos$y))/length(datos$y)
  b[2] <- (c*100)/abs(range(datos$y)[2]-range(datos$y)[1])
  b <- as.vector(b)
  names(b) <- cbind("MSE","%onRange")
  return (b)
}



# Procesamiento de propiedades del modelo
Analisis <- function (datos, model){
  resumen_model = summary(model)
  # Error Estandar Residual
  if (model$call[1]!="gam()"){
    a <-100*(resumen_model$sigma/(mean(datos$y)))
    at <- ifelse(a<10,"Si","No")
    a <- format(a,digits = 3)
  }
  else{
    a <-100*(sd(datos$y)/(mean(datos$y)))
    at <- ifelse(a<10,"Si","No")
    a <- format(a,digits = 3)
  }
  
  # ANOVA
  b <- 0
  bt <- "--"
  if (model$call[1]!="gam()"){
      #PONER AQUI EL CÁLCULO DE LA ANOVA
      }
  
  # Coeficiente de determinacion R2
  c <- 0
  ct <- "--"
  if (model$call[1]!="gam()"){
    #PONER AQUI EL CÁLCULO DEL COCIENTE DE DETERMINACIÓN
  }
  else {
    Ypred = predict(model, data = datos)
    Yreal = datos[,1]
    
    VT = sum( (Yreal-mean(Yreal))*(Yreal-mean(Yreal)) )
    VE = sum( (Ypred-mean(Yreal))*(Ypred-mean(Yreal)) )
    VR = sum(  (Yreal-Ypred) * (Yreal-mean(Yreal)))
    
    R2 = VE / VT
    n = length(Yreal)
    p = length((model))
    R2.corregido = 1 - (1-R2)*(n-1)/(n-p)
    c <- R2
    ct <- ifelse(c>0.8,"Si", "No")
  }

  c <- format(c,digits = 3)
  
  
  # 1. Test de Normalidad
  e <-shapiro.test(residuals(model))$p.value
  et <- ifelse(e>=0.05,"Si", "No")

  # 2. Homocedasticidad
  library(lmtest)
  f <-bptest(model)$p.value
  ft <- ifelse(f>=0.05,"Si", "No")
  names(ft)<-c()

  # 3. Incorrelacion
  library(lmtest)
  g<-dwtest(model,alternative = "two.sided")$p.value
  gt <- ifelse(g>=0.05,"Si","No")

  
  # 4. MSE
  h <- MSE(datos,model)
  h[1] <- format(h[1],digits = 3)
  #h[2] <- format(h[2],digits = 3)
  
  # Validacion cruzada
  library(bootstrap)
  i <- ValidacionCruzada(datos, 10, model) 
  i[1] <- format(i[1],digits = 3)
  i[2] <- format(i[2],digits = 3)
  
  data.frame(EER = at, ANOVA = bt, R2 = ct, MSE = h[1], CV = i[1], PError = i[2])
}



# Visualizacion del ajuste
visualiza_datos <- function(datos, model){
  datos_ord <-datos[sort(datos[,1], index.return=TRUE)$ix,]
  plot(1:dim(datos_ord)[1],datos_ord$y,xlab="ejemplo", ylab="y",type="p")
  pred <- predict(model, newdata = datos_ord)
  points(1:dim(datos_ord)[1],pred, col="red")
  
  plot(1:dim(datos_ord)[1],datos_ord$y,xlab="ejemplo", ylab="y",type="l")
  segments(1:dim(datos_ord)[1], datos_ord$y, 1:dim(datos_ord)[1], pred,col="red", lty = 1)
  
}


AnalisisGrafico <- function (datos, model){
  
  par(mfrow=c(2,2))
  
  # histograma Normalidad
  e <-residuals(model)
  d <- e/summary(model)$sigma
  
  hist (d, probability = T, xlab = "Errores estandar", main = "", xlim = c(-3,3))
  
  d.seq <- seq(-3,3,length = 50)
  
  lines(d.seq, dnorm(d.seq, mean(d), sd(d)), col="red")
  
  # Incorrelacion
  
  n <- length(d)
  plot(d[1:n-1],d[2:n],xlab = "Error i", ylab = "Error i-1")
  lines(lowess(d[1:n-1],d[2:n]),col="red")
  
  # Representacion del resultado
  
  visualiza_datos(datos,model)
}




# Resolucion del ejercicio 1
datos <-data.frame( y=iris$Sepal.Width,
                   x1=iris$Sepal.Length,
                   x2=iris$Petal.Length,
                   x3=iris$Petal.Width)

model.ejercicio1 <- lm(y~., data = datos)
model.ejercicio1.Resultado <- Analisis(datos,model.ejercicio1)
model.ejercicio1.Resultado






# Resolucion del ejercicio 2
datos <-data.frame( y=iris$Petal.Length,
                    x1=iris$Sepal.Length,
                    x2=iris$Sepal.Width,
                    x3=iris$Petal.Width)

model.ejercicio2 <- lm(y~., data = datos)
model.ejercicio2.Resultado <- Analisis(datos,model.ejercicio2)
model.ejercicio2.Resultado



# Comparacion entre los modelos del ejercicio 1 y el ejercicio2
df <- data.frame(rbind(ejer1=model.ejercicio1.Resultado,
                       ejer2=model.ejercicio2.Resultado),
                 stringsAsFactors = FALSE)
df





#Resolucion del ejercicio 3
datos <-data.frame(  y=trees$Girth,
                    x1=trees$Height,
                    x2=trees$Volume)





#Resolucion del ejercicio 4
datos <-data.frame( y=iris$Sepal.Width,
                    x1=iris$Sepal.Length,
                    x2=iris$Petal.Length,
                    x3=iris$Petal.Width)

model.ejercicio41 <- lm(y~poly(x1,3), data = datos)

model.ejercicio42 <- lm(y~poly(x2,3), data = datos)

model.ejercicio43 <- lm(y~poly(x3,3), data = datos)

model.ejercicio44 <- lm(y~poly(x1,3)+poly(x2,3)+poly(x3,3), data = datos)

model.ejercicio45 <- lm(y~poly(x1,3)*poly(x2,3)*poly(x3,3), data = datos)



#ilustracion de aproximaciones con splies
datos <-data.frame( y=iris$Petal.Width,
                    x1=iris$Sepal.Width,
                    x2=iris$Petal.Length,
                    x3=iris$Sepal.Length)

# lineal
plot(iris$Sepal.Length,iris$Petal.Width,col="blue")
modelAux <- lm(y~x3, data = datos)
grid <-seq(from=range(iris$Sepal.Length)[1], to = range(iris$Sepal.Length)[2], length.out = 100)
salida <- predict(modelAux, newdata = list(x3 = grid))
kk <- cbind(grid,salida)
lines(kk[,1],kk[,2], col = "red")

# polinómica
plot(iris$Sepal.Length,iris$Petal.Width,col="blue")
modelAux <- lm(y~poly(x3,3), data = datos)
grid <-seq(from=range(iris$Sepal.Length)[1], to = range(iris$Sepal.Length)[2], length.out = 100)
salida <- predict(modelAux, newdata = list(x3 = grid))
kk <- cbind(grid,salida)
lines(kk[,1],kk[,2], col = "red")


# spline cúbico sin nudos
library(splines)
plot(iris$Sepal.Length,iris$Petal.Width,col="blue")
modelAux <- lm(y~bs(x3), data = datos)
grid <-seq(from=range(iris$Sepal.Length)[1], to = range(iris$Sepal.Length)[2], length.out = 100)
salida <- predict(modelAux, newdata = list(x3 = grid))
kk <- cbind(grid,salida)
lines(kk[,1],kk[,2], col = "red")

# spline cúbico 2 nudos
plot(iris$Sepal.Length,iris$Petal.Width,col="blue")
modelAux <- lm(y~bs(x3,knots = c(5.5,7.0)), data = datos)
grid <-seq(from=range(iris$Sepal.Length)[1], to = range(iris$Sepal.Length)[2], length.out = 100)
salida <- predict(modelAux, newdata = list(x3 = grid))
kk <- cbind(grid,salida)
abline(v=c(5.5,7.0),lty=2,col="darkgreen")
lines(kk[,1],kk[,2], col = "red")

# spline cúbico 5 nudos
plot(iris$Sepal.Length,iris$Petal.Width,col="blue")
modelAux <- lm(y~bs(x3,knots = c(5.0,5.5,6.0,6.5,7.0)), data = datos)
grid <-seq(from=range(iris$Sepal.Length)[1], to = range(iris$Sepal.Length)[2], length.out = 100)
salida <- predict(modelAux, newdata = list(x3 = grid))
kk <- cbind(grid,salida)
abline(v=c(5,5.5,6,6.5,7.0),lty=2,col="darkgreen")
lines(kk[,1],kk[,2], col = "red")

# spline cúbico 6 nudos
plot(iris$Sepal.Length,iris$Petal.Width,col="blue")
modelAux <- lm(y~bs(x3,knots = c(5.0,5.5,6.0,6.5,7.0,7.5)), data = datos)
grid <-seq(from=range(iris$Sepal.Length)[1], to = range(iris$Sepal.Length)[2], length.out = 100)
salida <- predict(modelAux, newdata = list(x3 = grid))
kk <- cbind(grid,salida)
abline(v=c(5,5.5,6,6.5,7.0,7.5),lty=2,col="darkgreen")
lines(kk[,1],kk[,2], col = "red")



# spline cúbico df = 12 nudos
plot(iris$Sepal.Length,iris$Petal.Width,col="blue")
modelAux <- lm(y~bs(x3,df=12), data = datos)
grid <-seq(from=range(iris$Sepal.Length)[1], to = range(iris$Sepal.Length)[2], length.out = 100)
salida <- predict(modelAux, newdata = list(x3 = grid))
kk <- cbind(grid,salida)
lines(kk[,1],kk[,2], col = "red")




#Resolucion del ejercicio 5
datos <-data.frame( y=iris$Sepal.Width,
                    x1=iris$Sepal.Length,
                    x2=iris$Petal.Length,
                    x3=iris$Petal.Width)

model.ejercicio51 <- lm(y~ns(x1,3), data = datos)

model.ejercicio52 <- lm(y~ns(x2,3), data = datos)

model.ejercicio53 <- lm(y~ns(x3,3), data = datos)

model.ejercicio54 <- lm(y~ns(x1,3)+ns(x2,3)+ns(x3,3), data = datos)

model.ejercicio55 <- lm(y~ns(x1,3)*ns(x2,3)*ns(x3,3), data = datos)




#Resolucion del ejercicio 6
datos <-data.frame( y=iris$Sepal.Width,
                    x1=iris$Sepal.Length,
                    x2=iris$Petal.Length,
                    x3=iris$Petal.Width)
library(splines)
library(gam)
model.ejercicio61 <- lm(y~ns(x1,4)+ns(x2,4)+ns(x3,4), data = datos)

model.ejercicio62 <- gam(y~s(x1,4)+s(x2,4)+s(x3,4), data = datos)

model.ejercicio63 <- gam(y~s(x1,16)+s(x2,16)+s(x3,16), data = datos)

model.ejercicio64 <- gam(y~s(x1,16)+s(x2,16)+s(x3,16), data = datos)




#Resolucion del Ejercicio 7
datos <-data.frame(  y=trees$Volume,
                     x1=trees$Girth,
                     x2=trees$Height)

# Apartado a

# Apartado b

# Apartado c




