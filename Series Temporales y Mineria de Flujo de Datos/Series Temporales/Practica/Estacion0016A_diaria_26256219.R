# Alberto Armijo Ruiz 26256219V
# armijoalb@correo.ugr.es
# Ejercicio de trabajo autónomo. Series temporales. Curso 2018-2019

## ------------------------------------------------------------------------
# cargamos las librerías
library(tseries)
library(tsoutliers)
library(ggplot2)
library(tsbox)
library(fpp2)


## ------------------------------------------------------------------------
data = read.csv2('./datos/DatosEstaciones - 2018-02/0016A.csv',header=TRUE,
                 stringsAsFactors = FALSE)
# Modificamos los datos para que estén correctos
data$Fecha = as.Date(data$Fecha)
data$Tmax = as.numeric(data$Tmax)
data$Tmin = as.numeric(data$Tmin)
data$Tmed = as.numeric(data$Tmed)
data$TPrec = as.numeric(data$TPrec)
data$Prec1 = as.numeric(data$Prec1)
data$Prec2 = as.numeric(data$Prec2)
data$Prec3 = as.numeric(data$Prec3)
data$Prec4 = as.numeric(data$Prec4)

head(data)


## ------------------------------------------------------------------------
data.aux = data[c('Fecha','Tmax')]
nrow(data.aux)
nrow(data.aux)/365
nrow(data.aux)-(4*365)
NPred = nrow(data.aux)-(4*365)
NTest = nrow(data.aux)-(4*365)


## ------------------------------------------------------------------------
data.aux[1,]
data.aux[nrow(data.aux),]


## ------------------------------------------------------------------------
# Como existen datos perdidos, utilizaremos Amelia para imputar dichos datos
serie = data[2:ncol(data)]
serie = serie[c('Fecha','Tmax')]
print(serie)


library(Amelia)
library(mice)
mice::nic(serie)
imp_serie = amelia(serie,m=1)
serie = imp_serie$imputations$imp1
serie = serie$Tmax
mice::nic(serie)


## ------------------------------------------------------------------------
serie.ts = ts(serie,frequency = 365)
plot.ts(serie.ts)


## ------------------------------------------------------------------------
k=15
filtro = rep(1/k,k)
filtrada = stats::filter(serie, filter=filtro,sides=2,method='convolution')
series = matrix(c(t(serie),t(filtrada)),ncol=2)
matplot(series,pch=1,type='l')

serie.modts = ts(filtrada,frequency = 365)
serie.mod = na.omit(filtrada)
plot.ts(serie.modts)


## ------------------------------------------------------------------------
plot(decompose(serie.modts))
serie.modts = na.omit(serie.modts)
length(serie.modts)/365


## ------------------------------------------------------------------------
NTest = length(serie.mod)-4*365
NPred = NTest
serieTr = serie.mod[1:(length(serie.mod)-NTest)]
tiempoTr = 1:length(serieTr)
serieTs = serie.mod[(length(serie.mod)-NTest+1):length(serie.mod)]
tiempoTs = (tiempoTr[length(tiempoTr)]+1):(tiempoTr[length(tiempoTr)]+NTest)

plot.ts(serieTr, xlim=c(1,tiempoTs[length(tiempoTs)]))
lines(tiempoTs,serieTs,col="red")


## ------------------------------------------------------------------------
# quitamos la seasonality
# obtenemos la componente estacional.
k = 365
length(serieTr)
est = decompose(serie.modts)$seasonal[1:k]
length(serieTr)/length(est)
aux_ts = est[1:length(serieTs)]
aux = rep(est,length(serieTr)/length(est))
serieTr.SinEst = serieTr - aux
serieTs.SinEst = serieTs - aux_ts
plot.ts(serieTr.SinEst, xlim=c(1,tiempoTs[length(tiempoTs)]))
lines(tiempoTs,serieTs.SinEst,col="red")


## ------------------------------------------------------------------------
acf(serieTr.SinEst)
pacf(serieTr.SinEst)
adftest = adf.test(serieTr.SinEst)
print(adftest)


## ------------------------------------------------------------------------
serieTr.SinEstDiff = diff(serieTr.SinEst)
serieTs.SinEstDiff = diff(serieTs.SinEst)
acf(serieTr.SinEstDiff)
adftest=adf.test(serieTr.SinEstDiff)
print(adftest)


## ------------------------------------------------------------------------
acf(serieTr.SinEstDiff)
pacf(serieTr.SinEstDiff)


## ------------------------------------------------------------------------
modelo.ar = arima(serieTr.SinEst,order=c(1,1,0))
valoresAjustados.ar = serieTr.SinEst + modelo.ar$residuals

Predicciones.ar = predict(modelo.ar, n.ahead = NPred)
valoresPredichos.ar = Predicciones.ar$pred

errorTr.ar = sum(modelo.ar$residuals^2)
errorTs.ar = sum((valoresPredichos.ar-serieTs.SinEst)^2)
print(errorTr.ar)
print(errorTs.ar)


## ------------------------------------------------------------------------
plot.ts(serieTr.SinEst,
        xlim=c(1,tiempoTs[length(tiempoTs)]))
lines(valoresAjustados.ar, col='deepskyblue')
lines(tiempoTs,serieTs.SinEst,col='red')
lines(tiempoTs,valoresPredichos.ar, col='blue')


## ------------------------------------------------------------------------
boxtest.ar = Box.test(modelo.ar$residuals)
print(boxtest.ar)

JB.ar = jarque.bera.test(modelo.ar$residuals)
print(JB.ar)

SW.ar = shapiro.test(modelo.ar$residuals)
print(SW.ar)

hist(modelo.ar$residuals, col="blue", prob=T,
     ylim=c(0,20), xlim=c(-0.2,0.2))
lines(density(modelo.ar$residuals))


## ------------------------------------------------------------------------
valoresAjustados = valoresAjustados.ar + aux
valoresPredichos = valoresPredichos.ar + aux_ts
tiempo = 1:length(serie.mod)
tiempoPred = (tiempo[length(tiempo)]+(1:NPred))
plot.ts(serie.mod,xlim=c(1,max(tiempoPred)),
        ylim=c(10,40))
lines(valoresAjustados,col="blue")
lines(valoresPredichos,col="red")


## ------------------------------------------------------------------------
modelo.ma = arima(serieTr.SinEst,order=c(0,1,4))
valoresAjustados.ma = serieTr.SinEst + modelo.ma$residuals

Predicciones.ma = predict(modelo.ma, n.ahead = NPred)
valoresPredichos.ma = Predicciones.ma$pred

errorTr.ma = sum(modelo.ma$residuals^2)
errorTs.ma = sum((valoresPredichos.ma-serieTs.SinEst)^2)
print(errorTr.ma)
print(errorTs.ma)


## ------------------------------------------------------------------------
boxtest.ma = Box.test(modelo.ma$residuals)
print(boxtest.ma)

JB.ma = jarque.bera.test(modelo.ma$residuals)
print(JB.ma)

SW.ma = shapiro.test(modelo.ma$residuals)
print(SW.ma)

hist(modelo.ma$residuals, col="blue", prob=T,
     ylim=c(0,20), xlim=c(-0.2,0.2))
lines(density(modelo.ma$residuals))


## ------------------------------------------------------------------------
plot.ts(serieTr.SinEst,
        xlim=c(1,tiempoTs[length(tiempoTs)]))
lines(valoresAjustados.ma, col='deepskyblue')
lines(tiempoTs,serieTs.SinEst,col='red')
lines(tiempoTs,valoresPredichos.ma, col='blue')


## ------------------------------------------------------------------------
valoresAjustados = valoresAjustados.ma + aux
valoresPredichos = valoresPredichos.ma + aux_ts
tiempo = 1:length(serie.mod)
tiempoPred = (tiempo[length(tiempo)]+(1:NPred))
plot.ts(serie.mod,xlim=c(1,max(tiempoPred)),
        ylim=c(10,40))
lines(valoresAjustados,col="blue")
lines(valoresPredichos,col="red")


## ------------------------------------------------------------------------

modelo.arma = arima(serieTr.SinEst,order=c(1,1,4))
valoresAjustados.arma = serieTr.SinEst + modelo.arma$residuals

Predicciones.arma = predict(modelo.arma, n.ahead = NPred)
valoresPredichos.arma = Predicciones.arma$pred

errorTr.arma = sum(modelo.arma$residuals^2)
errorTs.arma = sum((valoresPredichos.arma-serieTs.SinEst)^2)
print(errorTr.arma)
print(errorTs.arma)


## ------------------------------------------------------------------------
boxtest.arma = Box.test(modelo.arma$residuals)
print(boxtest.arma)

JB.arma = jarque.bera.test(modelo.arma$residuals)
print(JB.arma)

SW.arma = shapiro.test(modelo.arma$residuals)
print(SW.arma)

hist(modelo.arma$residuals, col="blue", prob=T,
     ylim=c(0,20), xlim=c(-0.2,0.2))
lines(density(modelo.arma$residuals))


## ------------------------------------------------------------------------
plot.ts(serieTr.SinEst,
        xlim=c(1,tiempoTs[length(tiempoTs)]))
lines(valoresAjustados.arma, col='deepskyblue')
lines(tiempoTs,serieTs.SinEst,col='red')
lines(tiempoTs,valoresPredichos.arma, col='blue')


## ------------------------------------------------------------------------
valoresAjustados = valoresAjustados.arma + aux
valoresPredichos = valoresPredichos.arma + aux_ts
tiempo = 1:length(serie.mod)
tiempoPred = (tiempo[length(tiempo)]+(1:NPred))
plot.ts(serie.mod,xlim=c(1,max(tiempoPred)),
        ylim=c(10,40))
lines(valoresAjustados,col="blue")
lines(valoresPredichos,col="red")


## ------------------------------------------------------------------------
AIC(modelo.ar,modelo.ma,modelo.arma)
cat("Error en test de los modelos:",errorTs.ar,errorTs.ma,errorTs.arma,sep='\n')


## ------------------------------------------------------------------------
serie.entera = serie.mod
tiempo = 1:length(serie.entera)

aux = ts(serie.mod,frequency = 365)
aux = decompose(aux)$seasonal
estacionalidad = as.numeric(aux[1:365])
aux = rep(estacionalidad,length(serie.mod)/length(estacionalidad))
aux = c(aux,estacionalidad[1:(length(serie.mod)-length(aux))])

serieSinEst = serie.entera-aux

modelo = arima(serieSinEst,order=c(1,1,0))
valoresAjustados = serieSinEst+modelo$residuals

predicciones = predict(modelo,n.ahead=15)
valoresPredichos = predicciones$pred


## ------------------------------------------------------------------------
valoresAjustados = valoresAjustados+aux
valoresPredichos = valoresPredichos + estacionalidad[(length(serieTs)+1):(length(serieTs)+length(valoresPredichos))]

tiempoPred = (tiempo[length(tiempo)]+(1:length(valoresPredichos)))
plot.ts(serie.entera, xlim=c(1,max(tiempoPred)), ylim=c(10,40))
lines(valoresAjustados,col="blue")
lines(valoresPredichos,col="green")

