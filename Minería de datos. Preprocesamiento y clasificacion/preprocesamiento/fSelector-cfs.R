library(rpart)
library(FSelector)
data(iris)

# se realiza la seleccion de atributos:  todos los atributos
# como disponibles para la clasificacion de la variable Species
subset <- FSelector::cfs(Species~., iris)
f <- as.simple.formula(subset,"Species")

# se muestra el resultado 
print(f)

