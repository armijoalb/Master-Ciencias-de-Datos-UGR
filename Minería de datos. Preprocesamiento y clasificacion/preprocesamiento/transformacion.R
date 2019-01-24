require(mice)
require(caret)

# se usa el conjunto de datos de calidad del aire, en las
# mismas condiciones que vimos con anterioridad
datos <- airquality

# se determina el numero de instancias sin datos perdidos y con datos
# perdidos. A observar la comodidad de uso de las funciones ncc e nic
completos <- ncc(datos)
incompletos <- nic(datos)
cat("Datos completos: ",completos, " e incompletos: ",incompletos,"\n")

# se imputan los datos
imputados <- mice(datos)
datos <- complete(imputados)

# se aplica el centrado y escalado sobre el conjunto de datos,
# una vez eliminados los valores perdidos
valoresPreprocesados <- caret::preProcess(datos[,1:4],method=c("center","scale"))

# el resultado consiste en el escalado y centrado de las variables
# de la 1 a la 4 (las que pueden considerarse continuas). El resultado
# anterior se usa ahora para asignar a las variables los valores
# correspondientes de acuerdo a esta transformacion
valoresTransformados <- predict(valoresPreprocesados,datos[,1:4])

# y podemos generar un nuevo conjunto de datos con el que
# seguir aplicando tecnicas con las 4 variables transformadas
# y las dos que no se tocaron
datosFinales <- cbind(valoresTransformados,datos[,5:6])

