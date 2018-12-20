library(ISLR)
library(splines)
library(gam)

# Declaracion de funciones

# Calculo de la medida del error cuadratico medio (MSE)
MSE <- function(datos,regresion){
  yprime <- predict(regresion, datos)
  b <-sum(abs(datos$y-yprime)^2)/length(yprime) ##MSE
  b <- as.vector(b)
  c <-sum(abs(datos$y-yprime))/length(yprime) ##Absolute
  b[2]<- (c/abs(range(datos[,1])[2]-range(datos[,1])[1]))*100
  return(b)
}



library(bootstrap)

theta.fit <- function(v,w,ff=model){
  a <- 0
  if (ff$call[1]=="lm()"){
    a <-lm(as.formula(ff$call$formula), data=as.data.frame(cbind(v,y=w)))
  }
  else{ if  (ff$call[1]=="gam()"){
    a <-gam(as.formula(ff$call$formula), data=as.data.frame(cbind(v,y=w)))
  }
    else if (ff$call[1]=="glm()"){
      a <-glm(as.formula(ff$call$formula), data=as.data.frame(cbind(v,y=w)))
    }
  }  
  a
}

theta.predict <- function(fit, x) {
  if (is.null(dim(x))) {
    x <- t(x)
  }
  round(predict(fit, newdata=as.data.frame(x)))
}


ValidacionCruzada <- function(datos, particiones, model){
  a <- crossval(datos[,-1], datos[,1], theta.fit, theta.predict, ngroup=particiones, ff=model)
  b <- (sum(as.numeric(a$cv.fit == datos$y))/length(datos$y)) ##ACIERTO
  
  return (b)
}



# Procesamiento de propiedades del modelo
Analisis <- function (datos, model){
  resumen_model = summary(model)

  # 1. Test de Normalidad
  e <-shapiro.test(residuals(model))$p.value
  et <- ifelse(e>=0.05,"Si", "No")
  e <- format(e,digits = 3)
  
  # 2. Homocedasticidad
  library(lmtest)
  f <-bptest(model)$p.value
  ft <- ifelse(f>=0.05,"Si", "No")
  f <- format(f,digits = 3)
  
  # 3. Incorrelacion
  library(lmtest)
  g<-dwtest(model,alternative = "two.sided")$p.value
  gt <- ifelse(g>=0.05,"Si","No")
  g <- format(g,digits = 3)
  
  
  #h <- MSE(datos,model)
  h <- round(predict(model,newdata= datos, type="response"))
  h <- Acierto(datos[,1],h)
  h <- format(h,digits = 3)
  #h[2] <- format(h[2],digits = 3)
  
  # Validacion cruzada
  library(bootstrap)
  i <- ValidacionCruzada(datos, 10, model) 
  i[1] <- format(i[1],digits = 3)
  #i[2] <- format(i[2],digits = 3)
  
  list ( 
         "Norm"    = e, "T5" = et,
         "Homoced" = f, "T6" = ft,
         "Incorr"  = g, "T7" = gt,
         "Acierto" = h,
         "CV"      = i[1])
  
}

# Visualizacion del ajuste
visualiza_datos <- function(datos, model){
  datos_ord <-datos[sort(datos[,1], index.return=TRUE)$ix,]
  #reg = lm(formula, data = datos_ord)
  plot(1:dim(datos_ord)[1],datos_ord$y,xlab="ejemplo", ylab="y",type="p")
  pred <- round(predict(model, newdata = datos_ord, type="response"))
  points(1:dim(datos_ord)[1],pred, col="red")
  
  plot(1:dim(datos_ord)[1],datos_ord$y,xlab="ejemplo", ylab="y",type="l")
  segments(1:dim(datos_ord)[1], datos_ord$y, 1:dim(datos_ord)[1], pred,col="red", lty = 1)
  
}


AnalisisGrafico <- function (datos, model){
  
  par(mfrow=c(2,2))
  
  # histograma Normalidad
  e <-residuals(model)
  if (model$call[1]=="lm()"){
    d <- e/summary(model)$sigma
  }
  else{
    d <- e/sd(datos$y)
  }
  
  
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



# Resoluci?n del ejercicio

# 1. Conjunto de ejemplos

# Usaremos la base de datos "iris" sin la variable de clasificacion
# Variable dependiente y=iris$Sepal.Width

datos <-data.frame(y=as.numeric(I(iris$Sepal.Width<3)),
                   x1=iris$Sepal.Length,
                   x2=iris$Petal.Length,
                   x3=iris$Petal.Width)


# 2. Definicion de los modelos

model0 <-glm(y~.,data=datos)

model1 <-glm(y~ns(x1,4)+ns(x2,4)+ns(x3,4), data= datos)

model2 <-gam(y~s(x1,4)+s(x2,4)+s(x3,4), family="binomial", data= datos)

model3 <-gam(y~s(x1,16)+s(x2,16)+s(x3,16), family="binomial", data= datos)

model4 <-gam(y~s(x1,16)*s(x2,16)*s(x3,16), family="binomial", data= datos)


# Evaluacion del modelo 0
a <- round(predict(model0, newdata = datos, type="response"))
Acierto(datos[,1],a)
ValidacionCruzada(datos,10,model0)
a <- Analisis(datos,model0)
AnalisisGrafico(datos,model0)

# Evaluacion del modelo 1
b <- round(predict(model1, newdata = datos))
Acierto(datos[,1],b)
ValidacionCruzada(datos,10,model1)
b <- Analisis(datos,model1)
AnalisisGrafico(datos,model1)

# Evaluacion del modelo 2
c <- round(predict(model2, newdata = datos, type="response"))
Acierto(datos[,1],c)
ValidacionCruzada(datos,10,model2)
c <- Analisis(datos,model2)
AnalisisGrafico(datos,model2)


# Evaluacion del modelo 3
d <- round(predict(model3, newdata = datos, type="response"))
Acierto(datos[,1],d)
ValidacionCruzada(datos,10,model3)
d <- Analisis(datos,model3)
AnalisisGrafico(datos,model3)

# Evaluacion del modelo 4
e <- round(predict(model4, newdata = datos, type="response"))
Acierto(datos[,1],e)
ValidacionCruzada(datos,10,model4)
e <- Analisis(datos,model4)
AnalisisGrafico(datos,model4)


# Comparacion entre los modelos
df <- data.frame(rbind(model0=a,model1=b,model2=c,model3=d,model4=e),stringsAsFactors = FALSE)
df



# Para mas de 2 clases

datos <-data.frame(y=as.numeric(as.numeric(iris$Species)),
                   x4=iris$Sepal.Width,
                   x1=iris$Sepal.Length,
                   x2=iris$Petal.Length,
                   x3=iris$Petal.Width)


# modelo glm

model0 <- glm(y~.,data=datos)

model1 <-glm(y~ns(x1,4)+ns(x2,4)+ns(x3,4)+ns(x4,4), data= datos)

# Evaluacion del modelo 0
a <- round(predict(model0, newdata = datos, type="response"))
Acierto(datos[,1],a)
ValidacionCruzada(datos,10,model0)
a <- Analisis(datos,model0)
AnalisisGrafico(datos,model0)

# Evaluacion del modelo 1
b <- round(predict(model1, newdata = datos))
Acierto(datos[,1],b)
ValidacionCruzada(datos,10,model1)
b <- Analisis(datos,model1)
AnalisisGrafico(datos,model1)

# Ejercicio
# Creamos los clasificadores.
temp1 = datos
temp1$y = ifelse(temp1$y == 1, 1, 0)
model0 <-gam(y~ns(x1,4)+ns(x2,4)+ns(x3,4), family="binomial", data= temp1)
predModel0 = predict(model0, newdata=temp1, type="response")

temp2 = datos
temp2$y = ifelse(temp2$y == 2, 1, 0)
model1 <-gam(y~ns(x1,4)+ns(x2,4)+ns(x3,4), family="binomial", data= temp2)
predModel1 = predict(model1, newdata=temp2, type="response")

temp3 = datos
temp3$y = ifelse(temp3$y == 3, 1, 0)
model2 <-gam(y~ns(x1,4)+ns(x2,4)+ns(x3,4), family="binomial", data= temp3)
predModel2 = predict(model2, newdata=temp3, type="response")

# Para el caso OVA, tenemos que comparar la salida de cada uno de ellos.
dato_nuevo = data.frame(y=1,x4=3.9,x3=0.3,x2=1.8,x1=5.1)
p1 = predict(model0,newdata=dato_nuevo, type="response")
p2 = predict(model1,newdata=dato_nuevo, type="response")
p3 = predict(model2,newdata=dato_nuevo, type="response")
ps = c(p1,p2,p3)
x=max(ps)
which(x %in% ps)
which.max(ps)













