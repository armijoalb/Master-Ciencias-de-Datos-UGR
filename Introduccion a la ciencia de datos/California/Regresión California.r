
# Primero leemos los datos.
california =  read.csv("datos/california.dat", comment.char = "@", header=FALSE)
temp = scan("datos/california.dat", what=character())
ini = grep("inputs", temp, fixed=TRUE) + 1
fin = grep("outputs", temp, fixed=TRUE) - 1
y_pos = grep("outputs", temp, fixed=TRUE) + 1 
cal_attr = temp[ini:fin]
cal_attr = c(cal_attr,temp[y_pos])
cal_attr = unlist(strsplit(cal_attr,"[,]"))
colnames(california) = cal_attr
rm(temp)
rm(cal_attr)

summary(california)

# Primero miraremos las variables para ver cuales se adaptan mejor a un modelo lineal.
require(ggplot2)
colnames(california)
ggplot(california,aes(x=Longitude,y=MedianHouseValue))+geom_point(col="blue")
ggplot(california,aes(x=Latitude,y=MedianHouseValue))+geom_point(col="blue")
ggplot(california,aes(x=HousingMedianAge,y=MedianHouseValue))+geom_point(col="blue")
ggplot(california,aes(x=TotalRooms,y=MedianHouseValue))+geom_point(col="blue")
ggplot(california,aes(x=TotalBedrooms,y=MedianHouseValue))+geom_point(col="blue")
ggplot(california,aes(x=Population,y=MedianHouseValue))+geom_point(col="blue")
ggplot(california,aes(x=Households,y=MedianHouseValue))+geom_point(col="blue")
ggplot(california,aes(x=MedianIncome,y=MedianHouseValue))+geom_point(col="blue")


# También pintaremos histogramas de las variables para ver como se distribuyen
ggplot(california,aes(x=log(Population))) + geom_histogram(col="blue",bins=50)
ggplot(california,aes(x=log(Households))) + geom_histogram(col="blue",bins=50)
ggplot(california,aes(x=log(TotalRooms))) + geom_histogram(col="blue",bins=50)
ggplot(california,aes(x=log(TotalBedrooms))) + geom_histogram(col="blue",bins=50)
ggplot(california,aes(x=Latitude)) + geom_histogram(col="blue",bins=50)
ggplot(california,aes(x=Longitude)) + geom_histogram(col="blue",bins=50)
ggplot(california,aes(x=MedianIncome)) + geom_histogram(col="blue",bins=50)
ggplot(california,aes(x=log(HousingMedianAge))) + geom_histogram(col="blue",bins=50)

# Algunos datos están muy pegados a la izquierda, por lo que probaremos aplicacarle el logaritmo para que se parezcan más
# a una distribución normal.

ggplot(california,aes(x=MedianHouseValue)) + geom_histogram(bins=50, col="blue")
ggplot(california,aes(x=log(MedianHouseValue))) + geom_histogram(bins=50, col="blue")
# Al igual que con las caracterísitcas del dataset, los datos parecen distribuirse mejor con una transformación
# logaritmica, así que más tarde probaremos a crear modelos con la variable MedianHouseValue transformada.

pairs(california,pch=16,col="deepskyblue")

# Como se puede ver en la gráfica, algunos de los datos del dataset parecen estar correlacionados.
# Esto parece bastante normal, ya que ocurre con variable como Longitude y Latitude, o TotalRooms
# y TotalBedrooms. En este caso esa correlación nos puede servir para intentar mejorar el modelo 
# mediante interacciones entre dichas variables.

ggplot(california, aes(y=Longitude))+ geom_boxplot(outlier.color = "red")
ggplot(california, aes(y=Latitude))+ geom_boxplot(outlier.color = "red")
ggplot(california, aes(y=HousingMedianAge))+ geom_boxplot(outlier.color = "red")
ggplot(california, aes(y=Population))+ geom_boxplot(outlier.color = "red")
ggplot(california, aes(y=TotalBedrooms))+ geom_boxplot(outlier.color = "red")
ggplot(california, aes(y=TotalRooms))+ geom_boxplot(outlier.color = "red")
ggplot(california, aes(y=MedianIncome))+ geom_boxplot(outlier.color = "red")
ggplot(california, aes(y=Households))+ geom_boxplot(outlier.color = "red")
ggplot(california, aes(y=MedianHouseValue)) + geom_boxplot(outlier.color = "red")

# Pintando los boxplots nos damos cuenta que tenemos outliers, en este caso aparecen sobre las variables
# Population, TotalBedrooms, TotalRooms, Households, MedianIncome y MedianHouseValue. No los eliminaremos ya que 
# parecen ser casos extremos, en los que además parecen ser bastantes, por lo que no tienen porque ser realmente outliers
# y puede empeorar la predicción si los quitamos.

"La única variable que parece ser lineal es la variable MedianIncome, por ello
probaremos creando un modelo lineal con esta variable y veremos que resultados
nos da."
fit1 = lm(MedianHouseValue~MedianIncome,data=california)
summary(fit1)
plot(MedianHouseValue~MedianIncome,data=california)
abline(fit1,col="red")

"Según la información proporcionada por el modelo, se puede ver que la
variable MedianIncome sí que nos da información sobre el modelo y es 
importante para predecir. Aun así, el resultado del Adjusted R-squared es muy
pobre; por ello probaremos a añadir más variables."

"Para comenzar, optaremos por crear un modelo con todas las variables e 
iremos retirando las variables que peor puedan predecir la variable (peor
p-valor tiene, más alto)"
fit2 = lm(MedianHouseValue~., data=california)
summary(fit2)

"Como se puede ver, según el modelo todas las variables son importantes para
predecir la variable MedianHouseValue, por lo cual optaremos por retirar
algunas variables y añadir interacciones y no linealidad"

fit3 = lm(MedianHouseValue~.-Longitude-Latitude,data=california)
"Resultados tercer modelo"
summary(fit3)

fit4 = lm(MedianHouseValue~.-TotalBedrooms-TotalRooms,data=california)
"Resultados cuarto modelo"
summary(fit4)

fit5 = lm(MedianHouseValue~.-Households,data=california)
"Resultados quinto modelo"
summary(fit5)


fit6 = lm(MedianHouseValue~.-Households-TotalRooms-TotalBedrooms,data=california)
"Resultados sexto modelo"
summary(fit6)
"Parece que las variables TotalRooms o TotalBedrooms sí que aportan algo de
información por ello nos quedaremos con una de las dos ya que parecen estar relacionadas.
De estas nos quedaremos con TotalBedrooms."
ggplot(california, aes(x=TotalRooms,y=TotalBedrooms)) +  geom_point(col="blue")
cor(california$TotalBedrooms, california$TotalRooms)


fit7 = lm(MedianHouseValue~.-Households-TotalRooms,data=california)
summary(fit7)

"Mirando la forma que la gráfica de HouseMedianAge vs MedianHouseValue
parece que no hay ninguna relación lineal entre las dos, así que probaremos
a quitarla también"

fit8 = lm(MedianHouseValue~.-Households-TotalRooms-HousingMedianAge, data=california)
summary(fit8)

"Como no hay una mejora/empeoramiento significativo probaremos a hacer
interacciones entre variables y elevar al cuadrado a algunas para ver si mejora
el modelo. También consideraremos variables que habíamos eliminado antes."

fit9 = lm( MedianHouseValue~.+I(MedianIncome^2),data=california)
summary(fit9)

# Parece que el modelo mejora algo aunque minimamente, añadiremos más variables para ver si mejora más o no.

fit10 = lm(MedianHouseValue~.+I(Households^2), data=california)
summary(fit10)
# En este caso la diferencia no es significativa, así que no utilizaremos esta variable, al menos sola.

fit11 = lm(log(MedianHouseValue)~.+I(Households^2)+I(MedianIncome^2),data=california)
summary(fit11)
# En este caso si que vemos una mejora considerable conforme al modelo anterior, así que conservaremos
# nuestra variable predictora con la transformación logarítmica.

fit12 = lm(log(MedianHouseValue)~.+I(MedianIncome^2)
           +I(log(Population))+I(log(TotalBedrooms))
           +I(Longitude*Latitude) +I(Latitude**2)
           +I(Longitude**2) + I(Latitude**2 * Longitude**2) 
           +I(Longitude*Latitude**2) + I(Longitude**2*Latitude) 
           + I(log(TotalRooms)) - Households - HousingMedianAge
           , data=california)
summary(fit12)

# Este es el mejor modelo obtenido hasta ahora, se han eliminado las variables Households y HousingMedianAge
# ya que no aportaban nada realmente al modelo y las pérdidas en la variable R^2 ajustada son mínimas. Otras
# transformaciones como las logarítmicas se han probado debido a la información aportada por los histogramas
# anteriores (en los histogramas están transformados también). Las transformaciones a las variables Longitude
# y Latitude se han sido pruebas para intentar mejorar el modelo algo más.

nombre <- "datos/california" 
names(california)
run_lm_fold <- function(i, x, tt = "test") {
    file <- paste(x, "-5-", i, "tra.dat", sep="")
    x_tra <- read.csv(file, comment.char="@", header=FALSE)
    file <- paste(x, "-5-", i, "tst.dat", sep="")
    x_tst <- read.csv(file, comment.char="@", header=FALSE)
    In <- length(names(x_tra)) - 1
    names(x_tra)[1:In] <- paste ("X", 1:In, sep="")
    names(x_tra)[In+1] <- "Y"
    names(x_tst)[1:In] <- paste ("X", 1:In, sep="")
    names(x_tst)[In+1] <- "Y"
    if (tt == "train") {
        test <- x_tra
    }
    else {
        test <- x_tst
    }
    fitMulti=lm(log(Y)~.+I(X8^2)
           +I(log(X6))+I(log(X5))
           +I(X1*X2) +I(X2**2)
           +I(X1**2) + I(X2**2 * X1**2) 
           +I(X1*X2**2) + I(X1**2*X2) 
           + I(log(X4)) - X7 - X3
           , data=x_tra)
    # Transformamos los datos a su forma normal ya que la predicción está hecha sobre log(Y) y no sobre Y
    yprime=exp(predict(fitMulti,test))
    sum(abs(test$Y-yprime)^2)/length(yprime) ##MSE
}
lmMSEtrain<-mean(sapply(1:5,run_lm_fold,nombre,"train"))
lmMSEtest<-mean(sapply(1:5,run_lm_fold,nombre,"test"))
print(lmMSEtrain)
print(lmMSEtest)

"Ahora pasaremos a estudiar modelos con knn"

require(kknn)
fitknn1 = kknn(MedianHouseValue~MedianIncome,california,california)
yprime = fitknn1$fitted.values
print(sqrt(sum((california$MedianHouseValue-yprime)^2)/length(yprime))) #RMSE

# Los resultados no parecen demasiado buenos, así que probaremos con todas las variables
fitknn2 = kknn(MedianHouseValue~.,california,california)
yprime = fitknn2$fitted.values
print(sqrt(sum((california$MedianHouseValue-yprime)^2))/length(yprime))

# Por si acaso, voy a probar con las mismas variables de mi mejor modelo lineal
fitknn3 = kknn(log(MedianHouseValue)~.+I(MedianIncome^2)
           +I(log(Population))+I(log(TotalBedrooms))
           +I(Longitude*Latitude) +I(Latitude**2)
           +I(Longitude**2) + I(Latitude**2 * Longitude**2) 
           +I(Longitude*Latitude**2) + I(Longitude**2*Latitude) 
           + I(log(TotalRooms)) - Households - HousingMedianAge
           , california, california)
yprime = exp(fitknn3$fitted.values)
print(sqrt(sum((california$MedianHouseValue-yprime)^2))/length(yprime))

# En este caso, las interraciones de las variables que funcionaban bien para el modelo lineal
# siguen funcionando bien para el algoritmo de knn. Por ello, el siguiente paso que voy a dar es 
# intentar mejorar el RMSE modificando los parámetros del modelo para intentar mejorarlo.
fitknn4= kknn(log(MedianHouseValue)~.+I(MedianIncome^2)
           +I(log(Population))+I(log(TotalBedrooms))
           +I(Longitude*Latitude) +I(Latitude**2)
           +I(Longitude**2) + I(Latitude**2 * Longitude**2) 
           +I(Longitude*Latitude**2) + I(Longitude**2*Latitude) 
           + I(log(TotalRooms)) - Households - HousingMedianAge
           , california, california,k=5)
yprime = exp(fitknn4$fitted.values)
print(sqrt(sum((california$MedianHouseValue-yprime)^2))/length(yprime))

# Parece que para k=5 da mejores resultados que para k=7, que es el valor por defecto, así que dejaremos el valor de k
# como 5. Ahora, vamos a probar a cambiar el kernel del algoritmo para ver si funciona mejor.
fitknn5= kknn(log(MedianHouseValue)~.+I(MedianIncome^2)
           +I(log(Population))+I(log(TotalBedrooms))
           +I(Longitude*Latitude) +I(Latitude**2)
           +I(Longitude**2) + I(Latitude**2 * Longitude**2) 
           +I(Longitude*Latitude**2) + I(Longitude**2*Latitude) 
           + I(log(TotalRooms)) - Households - HousingMedianAge
           , california, california,k=5,kernel="gaussian")
yprime = exp(fitknn5$fitted.values)
print(sqrt(sum((california$MedianHouseValue-yprime)^2))/length(yprime))

# Este modelo empeora con respecto al anterior, así que probaremos con otro kernel.

fitknn6= kknn(log(MedianHouseValue)~.+I(MedianIncome^2)
           +I(log(Population))+I(log(TotalBedrooms))
           +I(Longitude*Latitude) +I(Latitude**2)
           +I(Longitude**2) + I(Latitude**2 * Longitude**2) 
           +I(Longitude*Latitude**2) + I(Longitude**2*Latitude) 
           + I(log(TotalRooms)) - Households - HousingMedianAge
           , california, california,k=5, kernel="biweight")
yprime = exp(fitknn6$fitted.values)
print(sqrt(sum((california$MedianHouseValue-yprime)^2))/length(yprime))

# Por ahora este es nuestro mejor modelo, ahora tenemos que comprobar si el modelo no
# tiene sobreaprendizaje.


nombre <- "datos/california"
run_knn_fold <- function(i, x, tt = "test") {
    file <- paste(x, "-5-", i, "tra.dat", sep="")
    x_tra <- read.csv(file, comment.char="@", header=FALSE)
    file <- paste(x, "-5-", i, "tst.dat", sep="")
    x_tst <- read.csv(file, comment.char="@", header=FALSE)
    In <- length(names(x_tra)) - 1
    names(x_tra)[1:In] <- paste ("X", 1:In, sep="")
    names(x_tra)[In+1] <- "Y"
    names(x_tst)[1:In] <- paste ("X", 1:In, sep="")
    names(x_tst)[In+1] <- "Y"
    if (tt == "train") {
        test <- x_tra
    }
    else {
        test <- x_tst
    }
    fitMulti=kknn(log(Y)~.+I(X8^2)
           +I(log(X6))+I(log(X5))
           +I(X1*X2) +I(X2**2)
           +I(X1**2) + I(X2**2 * X1**2) 
           +I(X1*X2**2) + I(X1**2*X2) 
           + I(log(X4)) - X7 - X3
           , x_tra,test,k=5,kernel="biweight")
    # Transformamos los datos a su forma normal ya que la predicción está hecha sobre log(Y) y no sobre Y
    yprime=exp(fitMulti$fitted.values)
    sum(abs(test$Y-yprime)^2)/length(yprime) ##MSE
}
knnMSEtrain<-mean(sapply(1:5,run_knn_fold,nombre,"train"))
knnMSEtest<-mean(sapply(1:5,run_knn_fold,nombre,"test"))
print(knnMSEtrain)
print(knnMSEtest)

# En este caso, el error en train es mayor que el error que en test; esto suele ser bastante raro, así que
# probaremos con otros modelos de los creados antes para ver si funcionan mejor.

nombre <- "datos/california"
run_knn_fold <- function(i, x, tt = "test") {
    file <- paste(x, "-5-", i, "tra.dat", sep="")
    x_tra <- read.csv(file, comment.char="@", header=FALSE)
    file <- paste(x, "-5-", i, "tst.dat", sep="")
    x_tst <- read.csv(file, comment.char="@", header=FALSE)
    In <- length(names(x_tra)) - 1
    names(x_tra)[1:In] <- paste ("X", 1:In, sep="")
    names(x_tra)[In+1] <- "Y"
    names(x_tst)[1:In] <- paste ("X", 1:In, sep="")
    names(x_tst)[In+1] <- "Y"
    if (tt == "train") {
        test <- x_tra
    }
    else {
        test <- x_tst
    }
    fitMulti=kknn(log(Y)~.+I(X8^2)
           +I(log(X6))+I(log(X5))
           +I(X1*X2) +I(X2**2)
           +I(X1**2) + I(X2**2 * X1**2) 
           +I(X1*X2**2) + I(X1**2*X2) 
           + I(log(X4)) - X7 - X3
           , x_tra,test)
    # Transformamos los datos a su forma normal ya que la predicción está hecha sobre log(Y) y no sobre Y
    yprime=exp(fitMulti$fitted.values)
    sum(abs(test$Y-yprime)^2)/length(yprime) ##MSE
}
knnMSEtrain<-mean(sapply(1:5,run_knn_fold,nombre,"train"))
knnMSEtest<-mean(sapply(1:5,run_knn_fold,nombre,"test"))
print(knnMSEtrain)
print(knnMSEtest)
# Para este algoritmo, aunque el error parece ser menor que para el modelo lineal, si que hay sobreaprendizaje.

nombre <- "datos/california"
run_knn_fold <- function(i, x, tt = "test") {
    file <- paste(x, "-5-", i, "tra.dat", sep="")
    x_tra <- read.csv(file, comment.char="@", header=FALSE)
    file <- paste(x, "-5-", i, "tst.dat", sep="")
    x_tst <- read.csv(file, comment.char="@", header=FALSE)
    In <- length(names(x_tra)) - 1
    names(x_tra)[1:In] <- paste ("X", 1:In, sep="")
    names(x_tra)[In+1] <- "Y"
    names(x_tst)[1:In] <- paste ("X", 1:In, sep="")
    names(x_tst)[In+1] <- "Y"
    if (tt == "train") {
        test <- x_tra
    }
    else {
        test <- x_tst
    }
    fitMulti=kknn(log(Y)~.+I(X8^2)
           +I(log(X6))+I(log(X5))
           +I(X1*X2) +I(X2**2)
           +I(X1**2) + I(X2**2 * X1**2) 
           +I(X1*X2**2) + I(X1**2*X2) 
           + I(log(X4)) - X7 - X3
           , x_tra,test,k=5)
    # Transformamos los datos a su forma normal ya que la predicción está hecha sobre log(Y) y no sobre Y
    yprime=exp(fitMulti$fitted.values)
    sum(abs(test$Y-yprime)^2)/length(yprime) ##MSE
}
knnMSEtrain2<-mean(sapply(1:5,run_knn_fold,nombre,"train"))
knnMSEtest2<-mean(sapply(1:5,run_knn_fold,nombre,"test"))
print(knnMSEtrain)
print(knnMSEtest)

# Al igual que para el modelo anterior, el modelo tiene sobreaprendiaje, pero los resultados parecen mejores que para
# el modelo lineal. Como el modelo anterior obtiene mejores resultados, nos quedaremos con este para comparar con el
# algoritmo de regresión lineal.

# Por último, utilizaremos el text de wilcoxon para ver cual de los dos modelos es mejor. Para ello compararemos nuestro
# mejor modelo lineal con el mejor modelo obtenido con knn.

tablatst = cbind(knnMSEtest,lmMSEtest)
difs <- (tablatst[,1] - tablatst[,2]) / tablatst[,1]
wilc_1_2 <- cbind(ifelse (difs<0, abs(difs)+0.1, 0+0.1), ifelse (difs>0, abs(difs)+0.1, 0+0.1))
colnames(wilc_1_2) <- c(colnames(tablatst)[1], colnames(tablatst)[2])
head(wilc_1_2)
LMvsKNNtst <- wilcox.test(wilc_1_2[,1], wilc_1_2[,2], alternative = "two.sided", paired=TRUE)
Rmas <- LMvsKNNtst$statistic
pvalue <- LMvsKNNtst$p.value
LMvsKNNtst <- wilcox.test(wilc_1_2[,2], wilc_1_2[,1], alternative = "two.sided", paired=TRUE)
Rmenos <- LMvsKNNtst$statistic
Rmas
Rmenos
pvalue

# Según los resultados del test de wilcoxon nos dice que sí que existe diferencia entre los datos y que el primero
# es el que es diferente. Por ello, podemos decir que el modelo con knn es mejor que el modelo de
# regresión lineal múltiple.

