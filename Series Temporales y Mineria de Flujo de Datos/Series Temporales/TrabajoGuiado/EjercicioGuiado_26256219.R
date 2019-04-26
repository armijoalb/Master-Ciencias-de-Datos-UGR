# Alberto Armijo Ruiz, 26256219V
# armijoalb@correo.ugr.es
# Ejercicio guiado. Curso 2018-2019

# path de datos.
setwd('.')

"
Lo primero que se va a hacer cargar todas las librerías necesarias y leer la serie.
Una vez leida la serie crearemos un objeto del tipo ts() para crear una serie.
Como se trata de una serie temporal que contiene el número de pasajeros de avión de
forma mensual a lo largo de 11 años; cuando creemos el objeto ts debemos indicarle
que tiene una frecuencia de 12.
"

# librerías
library(tseries)
library(ggplot2)
library(fpp2)
library(tsbox)
library(stats)
library(tsoutliers)

# carga de los datos.
NPred = 12
NTest = 12
serie = scan('pasajeros_1949_1959.dat')
serie.ts = ts(serie,frequency=12)
autoplot(decompose(serie.ts))

"Por lo que se puede ver en la descomposición de la serie temporal, hay un variación bastante
grande de los residuos de la serie; por ello, deberemos utilizar una transformación sobre los datos de la serie,
por ejemplo la transformación logarítmica."

serie.ts = log(serie.ts)
serie.log = log(serie)
autoplot(decompose(serie.ts))

"Ahora podemos ver que la variación de los residuos es mucho más pequeña durante toda la serie, también
nos vienen bien dentro de la serie ya que la estacionalidad crece durante el tiempo.
Además de esto, se puede ver claramente que esta serie tiene claramente una tendencia positiva
a lo largo de toda la serie (cada año viaja más gente) y también componente estacional por cada año.
Dentro de dicha estacionalidad,se puede ver que en los meses de verano hay un pico en el número de pasajeros,
tras el cual el número de pasajeros desciende en los meses de otoño e invierno.

Dado que tenemos tanto tendencia como estacionalidad en la serie, deberemos eliminarlas ambas. Lo primero
que se hará es eliminar la tendencia de la serie; después de esto, se eliminará la estacionalidad de la serie.

Lo siguiente que vamos a hacer es separar nuestra serie temporal en dos conjuntos uno para train y otro
para test. Para test seleccionaremos los 12 últimos ejemplos.
"

# creación de conjuntos de train y test
serieTr = serie.log[1:(length(serie.log)-NTest)]
tiempoTr = 1:length(serieTr)
serieTs = serie.log[(length(serie.log)-NTest+1):length(serie)]
tiempoTs = (tiempoTr[length(tiempoTr)]+1):(tiempoTr[length(tiempoTr)]+NTest)

# pintamos la serie
plot.ts(serieTr, xlim=c(1,tiempoTs[length(tiempoTs)]))
lines(tiempoTs,serieTs,col="red")

"Lo siguiente que se va a hacer es modelar la tendencia de la serie temporal para después eliminarla 
de la serie temporal; por lo que ha podido ver en la descomposición de la serie, parece que la tendencia
es lineal por lo cuál utilizaremos una regresión lineal simple. Una vez hayamos calculado el modelo lineal, 
estimaremos la tendencia y representaremos los datos. Utilizar otro tipo de herramienta, como por ejemplo
el filtrado en este caso no tiene sentido ya que la tendencia parece lineal, pero para otros conjuntos de datos
sí puede ser interesante utilizarlos; además, si para esta serie el ajuste no fuera bueno, se debería probar
con filtrado o diferenciación y analizar los resultados."

# Estimación de la tendencia
parametros.H1 = lm(serieTr~tiempoTr)

# obtenemos los valores de la tendencia para los valores en el tiempo en train y test.
TendEstimadaTr.H1 = parametros.H1$coefficients[1] + tiempoTr*parametros.H1$coefficients[2]
TendEstimadaTs.H1 = parametros.H1$coefficients[1] + tiempoTs*parametros.H1$coefficients[2]

# Pintamos la serie con la tendencia estimada.
plot.ts(serieTr,xlim=c(1,tiempoTs[length(tiempoTs)]))
lines(tiempoTr,TendEstimadaTr.H1,col='blue')
lines(tiempoTs,serieTs,col="red")
lines(tiempoTs,TendEstimadaTs.H1,col="green")


"Ahora pasaremos a comprobar que la tendencia lineal que hemos supuesto es válida. Para ello, comprobaremos primero que los
datos son normales y después utilizaremos un test de Student. Si la salida del test de Student es menor que el p-value (0.05)
, asumiremos que nuestra hipótesis es correcta."
JB = jarque.bera.test(parametros.H1$residuals)
print(JB)
JB = jarque.bera.test((TendEstimadaTs.H1-serieTs))
print(JB)

TT =t.test(c(parametros.H1$residuals, TendEstimadaTs.H1-serieTs))
print(TT)

"Una vez hechos los test eliminaremos la tendencia de la serie temporal. Para ello le eliminaremos la tendencia estimada
que hemos calculado anteriormente a los conjuntos de train y test."
# Restamos la tendencia estimada a cada uno de los conjuntos.
serieTr.SinTend.H1 = serieTr-TendEstimadaTr.H1
serieTs.SinTend.H1 = serieTs-TendEstimadaTs.H1
# Pintamos la serie sin la tendencia.
plot.ts(serieTr.SinTend.H1, xlim=c(1,tiempoTs[length(tiempoTs)]))
lines(tiempoTs,serieTs.SinTend.H1,col="red")

"Lo siguiente será eliminar la estacionalidad de la serie. Para ello podemos hacer uso de la componente seasonal de la
descomposición de la serie; dado que se realiza una descomposición aditiva simplemente habrá que restarle esta componente
a la serie. Normalmente, deberiamos de ayudarnos de la gráfica ACF para averiguar el periodo de la serie, pero como 
para este caso sabemos que los datos son mensuales, consideraremos que el periodo es 12 directamente. Igualmente se 
mostrará la gráfica ACF para comprobar que nuestra hipótesis es cierta."
# ACF de la serie sin tendencia.
acf(serieTr.SinTend.H1)

"En la gráfica ACF se puede ver también que el periodo es 12, ya que a partir de ese lag los datos vuelven a decrecer."
# periodo de la estacionalidad
k=12
# obtenemos la estacionalidad de la serie.
estacionalidad.H1 = decompose(serie.ts)$seasonal[1:k]

# Obtenemos los valores de la estacionalidad para el conjunto de train
aux = rep(estacionalidad.H1,length(serieTr)/length(estacionalidad.H1))
# Le restamos la estacionalidad.
serieTr.SinTendEst.H1 = serieTr.SinTend.H1-aux
serieTs.SinTendEst.H1 = serieTs.SinTend.H1 - estacionalidad.H1
# pintamos la serie
plot.ts(serieTr.SinTendEst.H1, xlim=c(1,tiempoTs[length(tiempoTs)]))
lines(tiempoTs,serieTs.SinTendEst.H1,col="red")

"Una vez hemos eliminado la estacionalidad de la serie, debemos comprobar que la serie es estacionaria. Para ello, 
utilizaremos el test de Dickie-Fuller aumentado. En caso de que el resultado del test fuera negativo; habría que 
diferenciar la serie y volver a probar si pasa el test (repitiendo el proceso hasta que el test sea positivo).
También podemos utilizar la gráfica ACF para ayudarnos; si tiende rápidamente a 0, es estacionaria."
acf(serieTr.SinTendEst.H1)
# Parece que le cuesta un poco llegar a 0, por lo que igual no pasa el test.

adftest.H1 = adf.test(serieTr.SinTendEst.H1)
print(adftest.H1)
# No pasa el test, así que diferenciamos.

serieTr.SinTendEstDiff.H1 = diff(serieTr.SinTendEst.H1)
serieTs.SinTendEstDiff.H1 = diff(serieTs.SinTendEst.H1)

# repetimos el test a ver si lo pasa
adftest.H1 = adf.test(serieTr.SinTendEstDiff.H1)
print(adftest.H1)
# Ahora el p-value < 0.05, así que no tenemos que volver a diferenciar.
acf(serieTr.SinTendEstDiff.H1)
# Ahora sí que desciende rápidamente a 0, y además pasa el test.

"Ya que tenemos una serie estacionaria, utilizaremos las medidas ACF y PACF para ver que tipo de modelo 
podemos utilizar para realizar el ajuste de la serie."
par(mfrow=c(1,2))
acf(serieTr.SinTendEstDiff.H1)
pacf(serieTr.SinTendEstDiff.H1)
par(mfrow=c(1,1))

"Según lo que se puede ver en el ACF y en el PACF, podría ser un model AR(4) (en el PACF a partir del 4 no
son mayores que los indicadores) o un MA(1) (aunque el segundo valor sea distinto de 0 en el ACF y supere el indicador,
podría ser un error en el cálculo, así que quedarse con el valor 1 es más seguro). Vamos a probar con un modelo AR(4),
podemos añadir la diferenciación indicándoselo a ARIMA. Una vez hayamos definido el modelo, haremos las predicciones
con los datos de test y calcularemos el error en train y test."
# generamos el modelo
modelo.H1 = arima(serieTr.SinTendEst.H1,order=c(4,1,0))
# obtenemos los valores estimados para train
valoresAjustados.H1 = serieTr.SinTendEst.H1 + modelo.H1$residuals

# obtenemos las prediciones del modelo para test.
Predicciones.H1 = predict(modelo.H1, n.ahead = NPred)
valoresPredichos.H1 = Predicciones.H1$pred

# calculamos los errores en train y test.
errorTr.H1 = sum(modelo.H1$residuals^2)
errorTs.H1 = sum((valoresPredichos.H1-serieTs.SinTendEst.H1)^2)
print(errorTr.H1)
print(errorTs.H1)

# Mostramos los resultados del ajuste que hemos realizado.
plot.ts(serieTr.SinTendEst.H1,
        xlim=c(1,tiempoTs[length(tiempoTs)]))
lines(valoresAjustados.H1, col='deepskyblue')
lines(tiempoTs,serieTs.SinTendEst.H1,col='red')
lines(tiempoTs,valoresPredichos.H1, col='blue')

"Por último, tenemos que validar el modelo que hemos calculado para estar seguros de que es un buen modelo. Para ello,
usaremos el test de Box-Pierce para la aleatoriedad de residuos, el test de Jarque Bera y Shapiro-Wilk para normalidad
de los residuos y por último mostraremos un histograma de los residuos y su función de densidad para confirmar la salida
de los test."
# Test de Box-Pierce
boxtestM1 = Box.test(modelo.H1$residuals)
print(boxtestM1)

# Test de Jarque Bera
JB.H1 = jarque.bera.test(modelo.H1$residuals)
print(JB.H1)
# Test de Shapiro Wilk
SW.H1 = shapiro.test(modelo.H1$residuals)
print(SW.H1)

# Pintamos el histograma.
hist(modelo.H1$residuals, col="blue", prob=T,
     ylim=c(0,20), xlim=c(-0.2,0.2))
lines(density(modelo.H1$residuals))

"Como se puede ver en la gráfica, los residuos siguen una distribución normal (muy parecida), por lo que el modelo es
bueno."

"En el análisis de ACF y PACF también se ha propuesto un modelo de medias móviles de grado 1, por lo que también crearemos
este modelo y realizaremos los test para ver si este los pasa. En el caso de que ambos modelos fueran buenos, se utilizaría
el criterio de Akaike para ver cuál de los dos modelos es mejor."
# generamos el modelo
modelo.H2 = arima(serieTr.SinTendEst.H1,order=c(0,1,1))
# obtenemos los valores estimados para train
valoresAjustados.H2 = serieTr.SinTendEst.H1 + modelo.H2$residuals

# obtenemos las prediciones del modelo para test.
Predicciones.H2 = predict(modelo.H2, n.ahead = NPred)
valoresPredichos.H2 = Predicciones.H2$pred

# calculamos los errores en train y test.
errorTr.H2 = sum(modelo.H2$residuals^2)
errorTs.H2 = sum((valoresPredichos.H2-serieTs.SinTendEst.H1)^2)
print(errorTr.H2)
print(errorTs.H2)

# Mostramos los resultados del ajuste que hemos realizado.
plot.ts(serieTr.SinTendEst.H1,
        xlim=c(1,tiempoTs[length(tiempoTs)]))
lines(valoresAjustados.H2, col='deepskyblue')
lines(tiempoTs,serieTs.SinTendEst.H1,col='red')
lines(tiempoTs,valoresPredichos.H2, col='blue')

# Test de Box-Pierce
boxtestM2 = Box.test(modelo.H2$residuals)
print(boxtestM2)

# Test de Jarque Bera
JB.H2 = jarque.bera.test(modelo.H2$residuals)
print(JB.H2)
# Test de Shapiro Wilk
SW.H2 = shapiro.test(modelo.H2$residuals)
print(SW.H2)

# Pintamos el histograma.
hist(modelo.H2$residuals, col="blue", prob=T,
     ylim=c(0,20), xlim=c(-0.2,0.2))
lines(density(modelo.H2$residuals))

"Este segundo modelo parece peor que el primero, de todas formas para asegurarnos utilizaremos el criterio de Akaike
y seleccionaremos el mejor de los dos."
AIC(modelo.H1,modelo.H2)

"Para este caso, se puede ver que primer modelo obtiene un valor más bajo, por lo que nos quedaremos con este para 
realizar la predicción."


"Finalmente, podemos aplicar todo el proceso anterior a la serie entera y utilizar unos nuevos datos para test. Para ello,
leeremos la serie entera, haremos la transformación logarítmica para reducir la varianza, eliminaremos la tendencia mediante
la estimación con un modelo de regresión lineal, eliminaremos la estacionalidad, calcularemos un modelo ARIMA de autoregresión
con grado 4 y una diferenciación para que la serie sea estacionaria. Una vez obtenido este modelo, realizaremos las
predicciones y el ajuste del modelo. Tras esto desharemos los cambios hechos anteriormente sobre el ajuste y la predicción
(eliminación de la estacionalidad y tendencia y transformación logarítmica ) y calcularemos el error de las predicciones sobre
los datos nuevos. También se visualizará la serie, el ajuste, la predicción y los nuevos datos."
serieEntera = serie.log
tiempo = 1:length(serieEntera)

# Estimación de la tendencia
parametros = lm(serieEntera ~ tiempo)
TendEstimada = parametros$coefficients[1]+tiempo*parametros$coefficients[2]
# Calculamos la serie sin tendencia.
serieSinTend = serieEntera - TendEstimada
aux = ts(serieEntera,frequency = 12)
# Obtenemos la componente estacional.
aux=decompose(aux)$seasonal
estacionalidad = as.numeric(aux[1:12])
aux=rep(estacionalidad,length(serieSinTend)/length(estacionalidad))
# Eliminamos la estacionalidad
serieSinTendEst = serieSinTend-aux

# Calculamos el modelo y obtenemos las predicciones.
modelo = arima(serieSinTendEst,order=c(4,1,0))
valoresAjustados = serieSinTendEst+modelo$residuals
Predicciones = predict(modelo, n.ahead=NPred)
valoresPredichos = Predicciones$pred

# Deshacemos los cambios y representamos la serie + predicción
# Añadimos ajuste
valoresAjustados = valoresAjustados+aux
# Añadimos estacionalidad
valoresPredichos = valoresPredichos+estacionalidad
# Añadimos tendencia
valoresAjustados = valoresAjustados+TendEstimada
tiempoPred = (tiempo[length(tiempo)]+(1:NPred))
# Estimamos la tendencia para los datos predecidos.
TendEstimadaPred = parametros$coefficients[1]+tiempoPred*parametros$coefficients[2]
valoresPredichos = valoresPredichos+TendEstimadaPred

# Deshacemos la transformación logaritmica
valoresAjustados = exp(valoresAjustados)
valoresPredichos = exp(valoresPredichos)

# Pintamos la serie
plot.ts(serie,xlim=c(1,max(tiempoPred)),
        ylim=c(100,650))
lines(valoresAjustados, col="blue")
lines(valoresPredichos, col="red")

# Cargamos los datos que hemos predecido y dibujamos el resultado final.
# Pintamos la serie
plot.ts(serie,xlim=c(1,max(tiempoPred)),
        ylim=c(100,650))
lines(valoresAjustados, col="blue")
lines(valoresPredichos, col="red")

# Leemos los datos y los pintamos en la serie
predReales = scan("pasajeros_1960.predict")
lines(tiempoPred, predReales,col="green")

# Calculamos el error producido en la predicción.
ErrorMedio = sum(abs(predReales-valoresPredichos))
print(ErrorMedio)

"Como se puede en la gráfica, el modelo produce una buena predicción sobre los datos. Se puede ver que
parte de la parte estacional que teníamos en la serie cambia un poco y por ello aparece un pico en nuestra 
predicción y no en los datos reales."

