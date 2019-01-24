# Este paquete puede usarse para imputar valores perdidos en
# variables de todo tipo
library(mice)
library(lattice)

# se usa el conjunto airquality
datos <- airquality

# se determina el numero de instancias sin datos perdidos y con datos
# perdidos. A observar la comodidad de uso de las funciones ncc e nic
completos <- mice::ncc(datos)
incompletos <- mice::nic(datos)
cat("Datos completos: ",completos, " e incompletos: ",incompletos,"\n")

# se realiza la imputacion
imputados <- mice::mice(datos, m=5, meth="pmm")

# dispone de algunos metodos que imputan siempre a un unico
# valor, como por ejemplo "mean"
imputadosMean <- mice::mice(datos, m=1, meth="mean")

# pmm es el metodo por defecto. Puedes verse todos los metodos
# disponibles de la siguiente forma
methods(mice)

# se completa el conjunto de datos con las imputaciones
datosImputados <- mice::complete(imputados)

# se determina el numero de instancias sin datos perdidos y con datos
# perdidos en la parte ya limpia
completos <- mice::ncc(datosImputados)
incompletos <- mice::nic(datosImputados)
cat("Datos completos: ",completos, " e incompletos: ",incompletos,"\n")

# se muestra la imputacion para Ozone
imputados$imp$Ozone

# Se muestra un grafico para comprobar la distribucion de Ozone en los
# datos imputados en relacion a otras variables. Los puntos en azul
# representan datos observados y datos en rojo representan imputaciones
lattice::xyplot(imputados,Ozone ~ Solar.R,pch=18,cex=1)

# Se muestran las densidades de los datos imputados respecto de los
# observados
lattice::densityplot(imputados)

# Se muestran los diagramas de caja para las imputaciones
lattice::bwplot(imputados)

