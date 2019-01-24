library(caret)

# se hace accesible el conjunto de datos iris
data(iris)

# se obtiene la matriz de correlacion de las variables predictoras
# (debe tratarse de variables numericas, por lo que se evita el
# tratamiento de la variable 5: variable clase)
correlationMatrix <- cor(iris[,1:4])

# se encuentran aquellos pares de variables muy correladas
highlyCorrelated <- caret::findCorrelation(correlationMatrix, cutoff=0.3)
print(highlyCorrelated)

# se visualiza la correlacion entre las variables Petal.Length y
# Peta.Width (marcadas como muy correladas)
ggplot(data=iris) + 
  geom_point(mapping=aes(x=Petal.Length, y=Petal.Width))

library(corrplot)
corrplot(correlationMatrix, method="circle")
