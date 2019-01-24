require(robCompositions)
require(mice)

# se usa el conjunto de datos de calidad del aire, en las
# mismas condiciones que vimos con anterioridad
datos <- airquality

# se determina el numero de instancias sin datos perdidos y con datos
# perdidos. A observar la comodidad de uso de las funciones ncc e nic
completos <- mice::ncc(datos)
incompletos <- mice::nic(datos)
cat("Datos completos: ",completos, " e incompletos: ",incompletos,"\n")

# se hace la imputacion
imputados <- robCompositions::impKNNa(datos, primitive=TRUE)

# Ahora puede visualizarse alguna informacion sobre la forma
# en que se hizo la imputacion. El segundo argumento indica el
# tipo de grafico a obtener
plot(imputados, which=2)

# El conjunto de datos completo puede accederse de la siguiente forma
imputados$xImp


