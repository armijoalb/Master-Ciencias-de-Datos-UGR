require(MASS)
require(ISLR)

?Boston
attach(Boston)
lstat

plot(medv~age,Boston)


temp <- Boston
plotY <- function (x,y) {
  plot(temp[,y]~temp[,x], xlab=paste(names(temp)[x]," X",x,sep=""),
       ylab=names(temp)[y])
}
par(mfrow=c(3,4))
x <- sapply(1:(dim(temp)[2]-1), plotY, dim(temp)[2])
par(mfrow=c(1,1))

par(mfrow=c(3,3))
x <- sapply(c(1, 5, 6, 7, 8, 10, 11, 12, 13), plotY, dim(temp)[2])
par(mfrow=c(1,1))

# Probamos primero con un modelo linear simple
fit1=lm(medv~lstat,data=Boston)
fit1


# Probamos con otro modelo.
fit2=lm(medv~rm,data=Boston)
fit2

# Ahora miramos la información más detallada de cada uno de los modelos.
summary(fit1)
par(mfrow=c(2,1))
plot(medv~lstat,data=Boston)
abline(fit1,col="red")
confint(fit1)

# Es bueno para ajustar modelos mirar mejor el R^2 ajustado que el R^2 normal.

# Hacemos los mismo para el modelo anterior.
summary(fit2)
plot(medv~rm,data=Boston)
abline(fit2,col="blue")
confint(fit2)
par(mfrow=c(1,1))


# Viendo que nuestro primer modelo tiene un mejor ajuste, nos centraremos en el modelo 'fit1'
# Por ello, vamos a calcular el error cuadrático medio (RMSE)
sqrt(sum(fit1$residuals^2)/length(fit1$residuals))


predict(fit1,data.frame(lstat=c(5,10,15)))

# Ahora vamos a probar a añadir más variables a nuestro modelo lineal.
fit3=lm(medv~lstat+age,data=Boston)
summary(fit3)

# Como se puede ver que la variable 'age' no nos aporta tanta información,
# así que vamos a probar con otra.