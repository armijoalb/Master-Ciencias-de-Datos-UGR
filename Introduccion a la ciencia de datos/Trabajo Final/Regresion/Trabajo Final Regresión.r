# Cambiar con el path donde se encuentren los archivos
setwd("C:/Users/Alberto Armijo Ruiz/Universidad/Introduccion a la ciencia de datos/Trabajo Final/Regresion")
# Leemos el fichero del dataset. La forma de escribir el PATH puede variar dependiendo de la organización o del SO.
treasury = read.csv('treasury//treasury.dat', comment.char = '@', header = FALSE,
                   stringsAsFactors = FALSE)

# Añadimos los nombres de las caracter�sticas y de la variable que vamos a predecir.
# Se cambiará un poco los nombres de las variables para que no haya problemas con R.
y_var = 'MonthCDRate'
pred_vars = c('CMaturityRate_1Y', 'CMortgageRate_30Y', 'Rate_AuctionAverage_3M',
              'Rate_SecondaryMarket_3M', 'CMaturityRate_3Y', 'CMaturityRate_5Y',
              'bankCredit', 'currency', 'demandDeposits','federalFunds',
              'moneyStock', 'checkableDeposits', 'loansLeases',
              'savingsDeposits', 'tradeCurrencies')
colnames(treasury) = c(pred_vars,y_var)
str(treasury)

# Utilizaremos la función summary para ver la media de las variables.
summary(treasury)

# Calculamos la media y la desviación estándar de las variables.
media = sapply(treasury,mean)
desviacion = sapply(treasury,sd)

# Mostramos los valores.
med_desv = cbind(media,desviacion)
med_desv

# Ahora pasaremos a pintar las distribuciones de cada una de las variables, para ello, utilizaremos una 
# gráfica de densidad de cada una de las variables ya que son datos reales.

# Cargamos librería para mostrar los datos.
library(ggplot2)

# Pintamos los gráficos de densidad de cada una de las variables. En naranja se muestra la media de dicha variable,
# en rojo, se muestra la mediana de dicha variable. Con la función ..count.. especificamos que queremos saber el número
# de casos en vez de la densidad.
ggplot(treasury,aes(x=CMaturityRate_1Y)) +
        geom_density(aes(y=..count..),fill='lightblue') +
        geom_vline(aes(xintercept = mean(CMaturityRate_1Y),
                       color = "mean"), 
             linetype = "dashed", size = 0.6) +
        geom_vline(aes(xintercept = median(CMaturityRate_1Y),
                       color="median"),
                  linetype = "dashed", size = 0.6 ) +
        scale_color_manual(name = "statistics",
                values = c(mean = "#FC4E07", median = "blue"))

ggplot(treasury,aes(x=CMortgageRate_30Y)) +
        geom_density(aes(y=..count..),fill='lightblue') +
        geom_vline(aes(xintercept = mean(CMortgageRate_30Y),
                       color = "mean"), 
             linetype = "dashed", size = 0.6) +
        geom_vline(aes(xintercept = median(CMortgageRate_30Y),
                       color="median"),
                  linetype = "dashed", size = 0.6 ) +
        scale_color_manual(name = "statistics", 
                values = c(mean = "#FC4E07", median = "blue"))

ggplot(treasury,aes(x=Rate_AuctionAverage_3M)) +
        geom_density(aes(y=..count..),fill='lightblue') +
        geom_vline(aes(xintercept = mean(Rate_AuctionAverage_3M),
                       color = "mean"), 
             linetype = "dashed", size = 0.6) +
        geom_vline(aes(xintercept = median(Rate_AuctionAverage_3M),
                       color="median"),
                  linetype = "dashed", size = 0.6 ) +
        scale_color_manual(name = "statistics", 
                values = c(mean = "#FC4E07", median = "blue"))

ggplot(treasury,aes(x=Rate_SecondaryMarket_3M)) + geom_density(aes(y=..count..),fill='lightblue') +
        geom_vline(aes(xintercept = mean(Rate_SecondaryMarket_3M),
                       color = "mean"), 
             linetype = "dashed", size = 0.6) +
        geom_vline(aes(xintercept = median(Rate_SecondaryMarket_3M),color="median"),
                  linetype = "dashed", size = 0.6 ) +
        scale_color_manual(name = "statistics",
                values = c(mean = "#FC4E07", median = "blue"))

ggplot(treasury,aes(x=CMaturityRate_3Y)) +
        geom_density(aes(y=..count..),fill='lightblue') +
        geom_vline(aes(xintercept = mean(CMaturityRate_3Y),
                       color = "mean"), 
             linetype = "dashed", size = 0.6) +
        geom_vline(aes(xintercept = median(CMaturityRate_3Y),
                       color="median"),
                  linetype = "dashed", size = 0.6 ) +
        scale_color_manual(name = "statistics",
                values = c(mean = "#FC4E07", median = "blue"))

ggplot(treasury,aes(x=CMaturityRate_5Y)) + 
        geom_density(aes(y=..count..),fill='lightblue') +
        geom_vline(aes(xintercept = mean(CMaturityRate_5Y),
                       color = "mean"), 
             linetype = "dashed", size = 0.6) +
        geom_vline(aes(xintercept = median(CMaturityRate_5Y),
                       color="median"),
                  linetype = "dashed", size = 0.6 ) +
        scale_color_manual(name = "statistics",
                values = c(mean = "#FC4E07", median = "blue"))

ggplot(treasury,aes(x=bankCredit)) + 
        geom_density(aes(y=..count..),fill='lightblue') +
        geom_vline(aes(xintercept = mean(bankCredit),
                       color = "mean"), 
             linetype = "dashed", size = 0.6) +
        geom_vline(aes(xintercept = median(bankCredit),
                       color="median"),
                  linetype = "dashed", size = 0.6 ) +
        scale_color_manual(name = "statistics", 
                values = c(mean = "#FC4E07", median = "blue"))

ggplot(treasury,aes(x=currency)) + 
        geom_density(aes(y=..count..),fill='lightblue') +
        geom_vline(aes(xintercept = mean(currency),
                       color = "mean"), 
             linetype = "dashed", size = 0.6) +
        geom_vline(aes(xintercept = median(currency),
                       color="median"),
                  linetype = "dashed", size = 0.6 ) +
        scale_color_manual(name = "statistics",
                values = c(mean = "#FC4E07", median = "blue"))

ggplot(treasury,aes(x=demandDeposits)) +
        geom_density(aes(y=..count..),fill='lightblue') +
        geom_vline(aes(xintercept = mean(demandDeposits),
                       color = "mean"), 
             linetype = "dashed", size = 0.6) +
        geom_vline(aes(xintercept = median(demandDeposits),
                       color="median"),
                  linetype = "dashed", size = 0.6 ) +
        scale_color_manual(name = "statistics",
                values = c(mean = "#FC4E07", median = "blue"))

ggplot(treasury,aes(x=federalFunds)) + 
        geom_density(aes(y=..count..),fill='lightblue') +
        geom_vline(aes(xintercept = mean(federalFunds),
                       color = "mean"), 
             linetype = "dashed", size = 0.6) +
        geom_vline(aes(xintercept = median(federalFunds),
                       color="median"),
                  linetype = "dashed", size = 0.6 ) +
        scale_color_manual(name = "statistics", 
                values = c(mean = "#FC4E07", median = "blue"))

ggplot(treasury,aes(x=moneyStock)) + 
        geom_density(aes(y=..count..),fill='lightblue') +
        geom_vline(aes(xintercept = mean(moneyStock),
                       color = "mean"), 
             linetype = "dashed", size = 0.6) +
        geom_vline(aes(xintercept = median(moneyStock),
                       color="median"),
                  linetype = "dashed", size = 0.6 ) +
        scale_color_manual(name = "statistics", 
                values = c(mean = "#FC4E07", median = "blue"))

ggplot(treasury,aes(x=checkableDeposits)) + 
        geom_density(aes(y=..count..),fill='lightblue') +
        geom_vline(aes(xintercept = mean(checkableDeposits),
                       color = "mean"), 
             linetype = "dashed", size = 0.6) +
        geom_vline(aes(xintercept = median(checkableDeposits),
                       color="median"),
                  linetype = "dashed", size = 0.6 ) +
        scale_color_manual(name = "statistics", 
                values = c(mean = "#FC4E07", median = "blue"))

ggplot(treasury,aes(x=loansLeases)) + 
        geom_density(aes(y=..count..),fill='lightblue') +
        geom_vline(aes(xintercept = mean(loansLeases),
                       color = "mean"), 
             linetype = "dashed", size = 0.6) +
        geom_vline(aes(xintercept = median(loansLeases),
                       color="median"),
                  linetype = "dashed", size = 0.6 ) +
        scale_color_manual(name = "statistics", 
                values = c(mean = "#FC4E07", median = "blue"))

ggplot(treasury,aes(x=savingsDeposits)) + 
        geom_density(aes(y=..count..),fill='lightblue') +
        geom_vline(aes(xintercept = mean(savingsDeposits),
                       color = "mean"), 
             linetype = "dashed", size = 0.6) +
        geom_vline(aes(xintercept = median(savingsDeposits),
                       color="median"),
                  linetype = "dashed", size = 0.6 ) +
        scale_color_manual(name = "statistics", 
                values = c(mean = "#FC4E07", median = "blue"))

ggplot(treasury,aes(x=tradeCurrencies)) + 
        geom_density(aes(y=..count..),fill='lightblue') +
        geom_vline(aes(xintercept = mean(tradeCurrencies),
                       color = "mean"), 
             linetype = "dashed", size = 0.6) +
        geom_vline(aes(xintercept = median(tradeCurrencies),
                       color="median"),
                  linetype = "dashed", size = 0.6 ) +
        scale_color_manual(name = "statistics", 
                values = c(mean = "#FC4E07", median = "blue"))

ggplot(treasury,aes(x=MonthCDRate)) + 
        geom_density(aes(y=..count..),fill='lightblue') +
        geom_vline(aes(xintercept = mean(MonthCDRate),
                       color = "mean"), 
             linetype = "dashed", size = 0.6) +
        geom_vline(aes(xintercept = median(MonthCDRate),
                       color="median"),
                  linetype = "dashed", size = 0.6 ) +
        scale_color_manual(name = "statistics", 
                values = c(mean = "#FC4E07", median = "blue"))


# Pintamos los boxplots. En rojo se muestran ( si es que hay) los outliers.
ggplot(treasury, aes(y=CMaturityRate_1Y)) +
    geom_boxplot(outlier.color = "red")
ggplot(treasury, aes(y=CMortgageRate_30Y)) +
    geom_boxplot(outlier.color = "red")
ggplot(treasury, aes(y=Rate_AuctionAverage_3M)) +
    geom_boxplot(outlier.color = "red")
ggplot(treasury, aes(y=Rate_SecondaryMarket_3M)) +
    geom_boxplot(outlier.color = "red")
ggplot(treasury, aes(y=CMaturityRate_3Y)) +
    geom_boxplot(outlier.color = "red")
ggplot(treasury, aes(y=CMaturityRate_5Y)) +
    geom_boxplot(outlier.color = "red")
ggplot(treasury, aes(y=bankCredit)) +
    geom_boxplot(outlier.color = "red")
ggplot(treasury, aes(y=currency)) +
    geom_boxplot(outlier.color = "red")
ggplot(treasury, aes(y=demandDeposits)) +
    geom_boxplot(outlier.color = "red")
ggplot(treasury, aes(y=federalFunds)) +
    geom_boxplot(outlier.color = "red")
ggplot(treasury, aes(y=moneyStock)) +
    geom_boxplot(outlier.color = "red")
ggplot(treasury, aes(y=checkableDeposits)) +
    geom_boxplot(outlier.color = "red")
ggplot(treasury, aes(y=loansLeases)) +
    geom_boxplot(outlier.color = "red")
ggplot(treasury, aes(y=savingsDeposits)) +
    geom_boxplot(outlier.color = "red")
ggplot(treasury, aes(y=tradeCurrencies)) +
    geom_boxplot(outlier.color = "red")
ggplot(treasury, aes(y=MonthCDRate)) +
    geom_boxplot(outlier.color = "red")

# Dibujamos las correlaciones entre las variables.
pairs(treasury,col="deepskyblue", main="Correlaciones de los datos")

cor(treasury)

ggplot(treasury, aes(y=MonthCDRate,x=CMaturityRate_1Y)) +
    geom_point(col="deepskyblue") + geom_smooth(method="lm")
ggplot(treasury, aes(y=MonthCDRate,x=CMortgageRate_30Y)) +
    geom_point(col="deepskyblue") + geom_smooth(method="lm")
ggplot(treasury, aes(y=MonthCDRate,x=Rate_AuctionAverage_3M)) +
    geom_point(col="deepskyblue") + geom_smooth(method="lm")
ggplot(treasury, aes(y=MonthCDRate,x=Rate_SecondaryMarket_3M)) +
    geom_point(col="deepskyblue") + geom_smooth(method="lm")
ggplot(treasury, aes(y=MonthCDRate,x=CMaturityRate_3Y)) +
    geom_point(col="deepskyblue") + geom_smooth(method="lm")
ggplot(treasury, aes(y=MonthCDRate,x=CMaturityRate_5Y)) +
    geom_point(col="deepskyblue") + geom_smooth(method="lm")
ggplot(treasury, aes(y=MonthCDRate,x=bankCredit)) +
    geom_point(col="deepskyblue") + geom_smooth(method="lm")
ggplot(treasury, aes(y=MonthCDRate,x=currency)) +
    geom_point(col="deepskyblue") + geom_smooth(method="lm")
ggplot(treasury, aes(y=MonthCDRate,x=demandDeposits)) +
    geom_point(col="deepskyblue") + geom_smooth(method="lm")
ggplot(treasury, aes(y=MonthCDRate,x=federalFunds)) +
    geom_point(col="deepskyblue") + geom_smooth(method="lm")
ggplot(treasury, aes(y=MonthCDRate,x=moneyStock)) + 
    geom_point(col="deepskyblue") + geom_smooth(method="lm")
ggplot(treasury, aes(y=MonthCDRate,x=checkableDeposits)) + 
    geom_point(col="deepskyblue") + geom_smooth(method="lm")
ggplot(treasury, aes(y=MonthCDRate,x=loansLeases)) + 
    geom_point(col="deepskyblue") + geom_smooth(method="lm")
ggplot(treasury, aes(y=MonthCDRate,x=savingsDeposits)) + 
    geom_point(col="deepskyblue") + geom_smooth(method="lm")
ggplot(treasury, aes(y=MonthCDRate,x=tradeCurrencies)) + 
    geom_point(col="deepskyblue") + geom_smooth(method="lm")

# Copiamos una variable del dataset para ver como le afectar�a las transformaciones.
temp = treasury$moneyStock

log.temp = data.frame(log.data=log(temp))
log.temp$MonthCDRate = treasury$MonthCDRate
ggplot(log.temp,aes(x=log.data)) +
    geom_density(aes(y=..count..),fill='lightblue') +
        geom_vline(aes(xintercept = mean(log.data),
                       color = "mean"), 
             linetype = "dashed", size = 0.6) +
        geom_vline(aes(xintercept = median(log.data),
                       color="median"),
                  linetype = "dashed", size = 0.6 ) +
        scale_color_manual(name = "statistics", 
                values = c(mean = "#FC4E07", median = "blue"))

ggplot(log.temp, aes(y=MonthCDRate,x=log.data)) +
    geom_point(col="deepskyblue") + geom_smooth(method="lm")

print(cor(log.temp))
print(cor(treasury$moneyStock, treasury$MonthCDRate))
# Para este caso, es mejor no transformar la variable, ya que como se puede ver, pierde la relación lineal con nuestra
# variable predictora.

# Creamos el modelo.
fit1 = lm(MonthCDRate~CMortgageRate_30Y, data=treasury)
summary(fit1)

# Dibujamos el ajuste.
confint(fit1)
ggplot(treasury,aes(x=CMortgageRate_30Y,y=MonthCDRate)) + 
    geom_point(col="deepskyblue") + geom_smooth(method="lm")

# Creamos el modelo.
fit2 = lm(MonthCDRate~Rate_SecondaryMarket_3M, data=treasury)
summary(fit2)

# Dibujamos el ajuste.
confint(fit2)
ggplot(treasury,aes(x=Rate_SecondaryMarket_3M,y=MonthCDRate)) + 
    geom_point(col="deepskyblue") + geom_smooth(method="lm")

# Creamos el modelo.
fit3 = lm(MonthCDRate~CMaturityRate_3Y, data=treasury)
summary(fit3)

# Dibujamos el ajuste.
confint(fit3)
ggplot(treasury,aes(x=CMaturityRate_3Y,y=MonthCDRate)) + 
    geom_point(col="deepskyblue") + geom_smooth(method="lm")

# Creamos el modelo.
fit4 = lm(MonthCDRate~CMaturityRate_5Y, data=treasury)
summary(fit4)

# Dibujamos el ajuste.
confint(fit4)
ggplot(treasury,aes(x=CMaturityRate_5Y,y=MonthCDRate)) + 
    geom_point(col="deepskyblue") + geom_smooth(method="lm")

# Creamos el modelo.
fit5 = lm(MonthCDRate~moneyStock, data=treasury)
summary(fit5)

# Dibujamos el ajuste.
confint(fit5)
ggplot(treasury,aes(x=moneyStock,y=MonthCDRate)) + 
    geom_point(col="deepskyblue") + geom_smooth(method="lm")

nombre <- "treasury//treasury"
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
    fitMulti=lm(Y~X11,x_tra)
    yprime=predict(fitMulti,test)
    sum(abs(test$Y-yprime)^2)/length(yprime) ##MSE
}
lmMSEtrain.simple<-mean(sapply(1:5,run_lm_fold,nombre,"train"))
lmMSEtest.simple<-mean(sapply(1:5,run_lm_fold,nombre,"test"))
cat("train: ",lmMSEtrain.simple, "\n")
cat("test: ",lmMSEtest.simple, "\n")

# Creamos el modelo
fit.multiple1 = lm(MonthCDRate~., treasury)

# Comprobamos los resultados
summary(fit.multiple1)

# Creamos el modelo
fit.multiple2 = lm(MonthCDRate~CMaturityRate_1Y+
                   CMortgageRate_30Y+CMaturityRate_3Y+
                   bankCredit+currency+demandDeposits+moneyStock+
                   checkableDeposits+loansLeases+savingsDeposits+
                   tradeCurrencies, treasury)

# Comprobamos los resultados
summary(fit.multiple2)

# Creamos el modelo
fit.multiple3 = lm(MonthCDRate~CMaturityRate_1Y+
                   CMortgageRate_30Y+CMaturityRate_3Y+
                   currency+moneyStock+tradeCurrencies, treasury)

# Comprobamos los resultados
summary(fit.multiple3)

# Creamos el modelo
fit.multiple4 = lm(MonthCDRate~CMortgageRate_30Y+CMaturityRate_3Y+
                   CMaturityRate_5Y+moneyStock+Rate_SecondaryMarket_3M,
                   treasury)

# Comprobamos los resultados
summary(fit.multiple4)

# Creamos el modelo
fit.multiple5 = lm(MonthCDRate~CMortgageRate_30Y+CMaturityRate_3Y+
                   CMaturityRate_5Y+moneyStock, treasury)

# Comprobamos los resultados
summary(fit.multiple5)

# Creamos el modelo
fit.multiple6 = lm(MonthCDRate~CMaturityRate_1Y+
                   CMortgageRate_30Y+CMaturityRate_3Y+
                   currency+moneyStock+tradeCurrencies+
                   I(tradeCurrencies^2), treasury)

# Comprobamos los resultados
summary(fit.multiple6)

# Creamos el modelo
fit.multiple7 = lm(MonthCDRate~CMaturityRate_1Y+CMortgageRate_30Y+
                   CMaturityRate_3Y+
                   currency+moneyStock+
                   tradeCurrencies+
                   I(CMaturityRate_1Y*currency), treasury)

# Comprobamos los resultados
summary(fit.multiple7)

# Creamos el modelo
fit.multiple8 = lm(MonthCDRate~CMaturityRate_1Y+CMortgageRate_30Y+
                   CMaturityRate_3Y+
                   currency+moneyStock+tradeCurrencies+
                   I(CMortgageRate_30Y*moneyStock), treasury)

# Comprobamos los resultados
summary(fit.multiple8)

# Creamos el modelo
fit.multiple9 = lm(MonthCDRate~CMaturityRate_1Y+
                   CMortgageRate_30Y+CMaturityRate_3Y+
                   currency+moneyStock+tradeCurrencies+
                   I(CMaturityRate_1Y*tradeCurrencies), treasury)

# Comprobamos los resultados
summary(fit.multiple9)


# Creamos el modelo
fit.multiple10 = lm(MonthCDRate~CMaturityRate_1Y+
                    CMortgageRate_30Y+CMaturityRate_3Y+
                   currency+moneyStock+tradeCurrencies+
                   I(moneyStock*tradeCurrencies), treasury)

# Comprobamos los resultados
summary(fit.multiple10)

# Creamos el modelo
fit.multiple11 = lm(MonthCDRate~CMaturityRate_1Y+
                    CMortgageRate_30Y+CMaturityRate_3Y+
                   currency+moneyStock+tradeCurrencies+
                   I(CMaturityRate_1Y**2), treasury)

# Comprobamos los resultados
summary(fit.multiple11)

# Creamos el modelo
fit.multiple12 = lm(MonthCDRate~CMaturityRate_1Y+
                    CMortgageRate_30Y+CMaturityRate_3Y+
                   currency+moneyStock+tradeCurrencies+
                   I(CMortgageRate_30Y**2), treasury)

# Comprobamos los resultados
summary(fit.multiple12)

# Creamos el modelo
fit.multiple13 = lm(MonthCDRate~CMaturityRate_1Y+
                    CMortgageRate_30Y+CMaturityRate_3Y+
                   currency+moneyStock+tradeCurrencies+
                   I(CMaturityRate_3Y**2), treasury)

# Comprobamos los resultados
summary(fit.multiple13)

# Creamos el modelo
fit.multiple14 = lm(MonthCDRate~CMaturityRate_1Y+
                    CMortgageRate_30Y+CMaturityRate_3Y+
                   currency+moneyStock+tradeCurrencies+
                   I(currency**2), treasury)

# Comprobamos los resultados
summary(fit.multiple14)

# Creamos el modelo
fit.multiple15 = lm(MonthCDRate~CMaturityRate_1Y+
                    CMortgageRate_30Y+CMaturityRate_3Y+
                   currency+moneyStock+tradeCurrencies+
                   I(moneyStock**2), treasury)

# Comprobamos los resultados
summary(fit.multiple15)

nombre <- "treasury//treasury"

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
    fitMulti=lm(Y~X1+X2+X5+X7+X8+X9+X11+X12+X13+X14+X15,x_tra)
    yprime=predict(fitMulti,test)
    sum(abs(test$Y-yprime)^2)/length(yprime) ##MSE
}
lmMSEtrain.multiple1<-mean(sapply(1:5,run_lm_fold,nombre,"train"))
lmMSEtest.multiple1<-mean(sapply(1:5,run_lm_fold,nombre,"test"))
cat("train: ",lmMSEtrain.multiple1, "\n")
cat("test: ",lmMSEtest.multiple1, "\n")

nombre <- "treasury//treasury"
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
    fitMulti=lm(Y~X1+X2+X5+X8+X11+X15,x_tra)
    yprime=predict(fitMulti,test)
    sum(abs(test$Y-yprime)^2)/length(yprime) ##MSE
}
lmMSEtrain.multiple2<-mean(sapply(1:5,run_lm_fold,nombre,"train"))
lmMSEtest.multiple2<-mean(sapply(1:5,run_lm_fold,nombre,"test"))
cat("train: ",lmMSEtrain.multiple2, "\n")
cat("test: ",lmMSEtest.multiple2, "\n")

# cargamos la librería para utilizar knn
library(kknn)
treasury.norm = as.data.frame(lapply(treasury,
                               scale, center = TRUE, scale = TRUE))

# Creamos un modelo y calculamos su RMSE
fitknn1 = kknn(MonthCDRate~., treasury.norm, treasury.norm)
yprime = fitknn1$fitted.values
print(sqrt(sum((treasury.norm$MonthCDRate-yprime)^2)/length(yprime))) #RMSE

# Creamos un modelo y calculamos su RMSE
fitknn2 = kknn(MonthCDRate~CMaturityRate_1Y+
               CMortgageRate_30Y+CMaturityRate_3Y+
                   bankCredit+currency+demandDeposits+moneyStock+
                   checkableDeposits+loansLeases+savingsDeposits+
                   tradeCurrencies, treasury.norm, treasury.norm)
yprime = fitknn2$fitted.values
print(sqrt(sum((treasury.norm$MonthCDRate-yprime)^2)/length(yprime))) #RMSE

# Creamos un modelo y calculamos su RMSE
fitknn3 = kknn(MonthCDRate~CMaturityRate_1Y+
               CMortgageRate_30Y+CMaturityRate_3Y+
                   bankCredit+currency+demandDeposits+moneyStock+
                   checkableDeposits+loansLeases+savingsDeposits+
                   tradeCurrencies, treasury.norm, treasury.norm,k=5)
yprime = fitknn3$fitted.values
print(sqrt(sum((treasury.norm$MonthCDRate-yprime)^2)/length(yprime))) #RMSE

# Creamos un modelo y calculamos su RMSE
fitknn4 = kknn(MonthCDRate~., treasury.norm, treasury.norm,k=5)
yprime = fitknn4$fitted.values
print(sqrt(sum((treasury.norm$MonthCDRate-yprime)^2)/length(yprime))) #RMSE

# Creamos un modelo y calculamos su RMSE
fitknn5 = kknn(MonthCDRate~CMaturityRate_1Y+
               CMortgageRate_30Y+CMaturityRate_3Y+
                   bankCredit+currency+demandDeposits+moneyStock+
                   checkableDeposits+loansLeases+savingsDeposits+
                   tradeCurrencies, treasury.norm, treasury.norm,k=7)
yprime = fitknn5$fitted.values
print(sqrt(sum((treasury.norm$MonthCDRate-yprime)^2)/length(yprime))) #RMSE

# Creamos un modelo y calculamos su RMSE
fitknn6 = kknn(MonthCDRate~., treasury.norm, treasury.norm,k=7)
yprime = fitknn6$fitted.values
print(sqrt(sum((treasury.norm$MonthCDRate-yprime)^2)/length(yprime))) #RMSE

# Creamos un modelo y calculamos su RMSE
fitknn7 = kknn(MonthCDRate~CMaturityRate_1Y+
               CMortgageRate_30Y+CMaturityRate_3Y+
                   bankCredit+currency+demandDeposits+moneyStock+
                   checkableDeposits+loansLeases+savingsDeposits+
                   tradeCurrencies, treasury.norm, treasury.norm,k=9)
yprime = fitknn7$fitted.values
print(sqrt(sum((treasury.norm$MonthCDRate-yprime)^2)/length(yprime))) #RMSE

# Creamos un modelo y calculamos su RMSE
fitknn8 = kknn(MonthCDRate~., treasury.norm, treasury.norm,k=9)
yprime = fitknn8$fitted.values
print(sqrt(sum((treasury.norm$MonthCDRate-yprime)^2)/length(yprime))) #RMSE

nombre <- "treasury//treasury"
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
    x_tra = lapply(x_tra,scale, center = TRUE, scale = TRUE)
    test = lapply(test,scale, center = TRUE, scale = TRUE)
    fitMulti=kknn(Y~.,x_tra,test,k=5)
    yprime=fitMulti$fitted.values
    sum(abs(test$Y-yprime)^2)/length(yprime) ##MSE
}
knnMSEtrain.all<-mean(sapply(1:5,run_knn_fold,nombre,"train"))
knnMSEtest.all<-mean(sapply(1:5,run_knn_fold,nombre,"test"))
print(knnMSEtrain.all)
print(knnMSEtest.all)

nombre <- "treasury//treasury"
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
    x_tra = lapply(x_tra,scale, center = TRUE, scale = TRUE)
    test = lapply(test,scale, center = TRUE, scale = TRUE)
    fitMulti=kknn(Y~X1+X2+X5+X7+X8+X9+X11+X12+X13+X14+X15 ,x_tra,test,k=5)
    
    yprime=fitMulti$fitted.values
    sum(abs(test$Y-yprime)^2)/length(yprime) ##MSE
}
knnMSEtrain.simple<-mean(sapply(1:5,run_knn_fold,nombre,"train"))
knnMSEtest.simple<-mean(sapply(1:5,run_knn_fold,nombre,"test"))
print(knnMSEtrain.simple)
print(knnMSEtest.simple)

nombre <- "treasury//treasury"
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
    fitMulti=lm(Y~.,x_tra)
    yprime=predict(fitMulti,test)
    sum(abs(test$Y-yprime)^2)/length(yprime) ##MSE
}
lmMSEtrain.all<-mean(sapply(1:5,run_lm_fold,nombre,"train"))
lmMSEtest.all<-mean(sapply(1:5,run_lm_fold,nombre,"test"))
cat("train: ",lmMSEtrain.all, "\n")
cat("test: ",lmMSEtest.all, "\n")

nombre <- "treasury//treasury"
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
    fitMulti=kknn(Y~.,x_tra,test,k=5)
    yprime=fitMulti$fitted.values
    sum(abs(test$Y-yprime)^2)/length(yprime) ##MSE
}
knnMSEtrain.all<-mean(sapply(1:5,run_knn_fold,nombre,"train"))
knnMSEtest.all<-mean(sapply(1:5,run_knn_fold,nombre,"test"))
print(knnMSEtrain.all)
print(knnMSEtest.all)

# Leemos los resultados de train.
resultados.train = read.csv("regr_train_alumnos.csv")
rownames(resultados.train) = resultados.train$X
resultados.train$X = NULL
resultados.train["treasury",]$out_train_lm = lmMSEtrain.all
resultados.train["treasury",]$out_train_kknn = knnMSEtrain.all

resultados.train

# Hacemos lo mismo para test.
resultados.test = read.csv("regr_test_alumnos.csv")
rownames(resultados.test) = resultados.test$X
resultados.test$X = NULL
resultados.test["treasury",]$out_test_lm = lmMSEtest.all
resultados.test["treasury",]$out_test_kknn = knnMSEtest.all

resultados.test

# Preparamos los datos para realizar el test de Wilcoxon.
# Utilizaremos el modelo de KNN como referencia.
tablatst = resultados.test
difs = (tablatst[,1]-tablatst[,2])/tablatst[,1]
wilc_1_2 <- cbind(ifelse (difs<0, abs(difs)+0.1, 0+0.1),
                  ifelse (difs>0, abs(difs)+0.1, 0+0.1))
colnames(wilc_1_2) <- c(colnames(tablatst)[1], colnames(tablatst)[2])
head(wilc_1_2)

# Aplicamos el test e interpretamos los datos.
LMvsKNNtst <- wilcox.test(wilc_1_2[,1], wilc_1_2[,2],
                          alternative = "two.sided", paired=TRUE)
Rmas <- LMvsKNNtst$statistic
pvalue <- LMvsKNNtst$p.value
LMvsKNNtst <- wilcox.test(wilc_1_2[,2], wilc_1_2[,1],
                          alternative = "two.sided", paired=TRUE)
Rmenos <- LMvsKNNtst$statistic
Rmas
Rmenos
pvalue

# Realizamos el test de Friedman.
test_friedman = friedman.test(as.matrix(tablatst))
test_friedman

# Aplicamos el test de Holm.
tam <- dim(tablatst)
groups <- rep(1:tam[2], each=tam[1])
pairwise.wilcox.test(as.matrix(tablatst),
                     groups, p.adjust = "holm",
                     paired = TRUE)

# creamos la tabla para train y aplicacamos el test de Wilcoxon
# para los datos de regresión lineal múltiple y KNN.
tablatrain = resultados.train
difs = (tablatrain[,1]-tablatrain[,2])/tablatrain[,1]
wilc_1_2 <- cbind(ifelse (difs<0, abs(difs)+0.1, 0+0.1),
                  ifelse (difs>0, abs(difs)+0.1, 0+0.1))
colnames(wilc_1_2) <- c(colnames(tablatrain)[1], colnames(tablatrain)[2])
head(wilc_1_2)

# Aplicamos el test e interpretamos los datos.
LMvsKNNtst <- wilcox.test(wilc_1_2[,1], wilc_1_2[,2],
                          alternative = "two.sided", paired=TRUE)
Rmas <- LMvsKNNtst$statistic
pvalue <- LMvsKNNtst$p.value
LMvsKNNtst <- wilcox.test(wilc_1_2[,2], wilc_1_2[,1],
                          alternative = "two.sided", paired=TRUE)
Rmenos <- LMvsKNNtst$statistic
Rmas
Rmenos
pvalue

# Realizamos el test de Friedman.
test_friedman <- friedman.test(as.matrix(tablatrain))
test_friedman

# Realizamos el test de Holm.
tam <- dim(tablatrain)
groups <- rep(1:tam[2], each=tam[1])
pairwise.wilcox.test(as.matrix(tablatrain),
                     groups, p.adjust = "holm", 
                     paired = TRUE)
