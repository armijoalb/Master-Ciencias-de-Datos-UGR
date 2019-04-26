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
data['newFecha'] = format(data$Fecha,'%Y-%m')


## ------------------------------------------------------------------------
# Como existen datos perdidos, utilizaremos Amelia para imputar dichos datos
library(Amelia)
library(mice)
serie = data[c('Fecha','Tmax')]
imp_serie = amelia(serie,m=1)
serie = imp_serie$imputations$imp1
serie['newFecha'] = data['newFecha']


## ------------------------------------------------------------------------
library(dplyr)
newdata = serie %>%
  group_by(newFecha) %>%
  summarise(meanTmax = mean(Tmax))
head(newdata)


## ------------------------------------------------------------------------
serie = newdata$meanTmax


## ------------------------------------------------------------------------
# creamos la serie temporal.
serie.ts = ts(serie,frequency = 12)
autoplot(decompose(serie.ts))


## ------------------------------------------------------------------------
plot.ts(serie.ts)


## ------------------------------------------------------------------------
length(serie)
length(serie)/12
NPred = length(serie) - (4*12)
NTest = NPred
NTest
NTrain = length(serie)-NTest
NTrain


## ------------------------------------------------------------------------
serieTr = serie[1:NTrain]
tiempoTr = 1:length(serieTr)
serieTs = serie[(NTrain+1):length(serie)]
tiempoTs = (tiempoTr[length(tiempoTr)]+1):(tiempoTr[length(tiempoTr)]+NTest)

plot.ts(serieTr, xlim=c(1,tiempoTs[length(tiempoTs)]))
lines(tiempoTs,serieTs,col="red")


## ------------------------------------------------------------------------
k = 12
length(serieTr)
est = decompose(serie.ts)$seasonal[1:k]
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
modelo.ar = arima(serieTr.SinEst,order=c(0,0,2))
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
tiempo = 1:length(serie)
tiempoPred = (tiempo[length(tiempo)]+(1:NPred))
plot.ts(serie,xlim=c(1,max(tiempoPred)),
        ylim=c(10,40))
lines(valoresAjustados,col="blue")
lines(valoresPredichos,col="red")


## ------------------------------------------------------------------------
serie.entera = serie
tiempo = 1:length(serie.entera)
aux = ts(serie.entera,frequency = 12)
aux = decompose(aux)$seasonal
estacionalidad = as.numeric(aux[1:12])
aux = rep(estacionalidad, 4)
aux = c(aux,estacionalidad[1:10])

serieSinEst = serie.entera - aux

modelo = arima(serieSinEst,order=c(1,0,0))
valores.ajustados = serieSinEst + modelo$residuals
predicciones = predict(modelo,n.ahead=2)
valores.predichos = predicciones$pred


## ------------------------------------------------------------------------
valores.ajustados = valores.ajustados + aux
valores.predichos = valores.predichos + estacionalidad[11:12]

tiempoPred = (tiempo[length(tiempo)] + (1:2))

plot.ts(serie.entera, xlim=c(1,max(tiempoPred)),
        ylim=c(10,40))
lines(valores.ajustados,col="blue")
lines(valores.predichos,col="green")

