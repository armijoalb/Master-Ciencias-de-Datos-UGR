library(outliers)
library(ggplot2)

# se carga el archivo con las funcione de lectura de datos
source("lecturaDatos.R")

path <- "./data/"
file <- "datos.csv"

# lectura de los datos
datos <- lecturaDatos(path,file)

# deteccion de anomalias para las variable 1 a 3. Observad 
# que no tiene sentido considerar variables de tipo discreto
# en este analisis. La funcion devuelve el valor (o valores)
# considerados anomalos para las variable de interes. Este
# metodo solo considera las desviaciones con respecto a los
# valores de cada variable (no relaciones con otras variables)
anomalos <- outlier(datos[,1:3])
print(anomalos)

# la media de la variable separation es
mean(datos[,"separation"])

# se muestra la distribucion de separation en funcion del valor
# de la variable clase
ggplot(data = datos, aes(class, separation)) +
  geom_boxplot()

# se podria hacer igual con la variable propensity
ggplot(data = datos, aes(class, propensity)) +
  geom_boxplot()

 
