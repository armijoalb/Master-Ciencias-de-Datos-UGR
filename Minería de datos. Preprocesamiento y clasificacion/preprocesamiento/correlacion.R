library(caret)
library(mlbench)

# se hace accesible el conjunto de datos PimaIndiansDiabetes
data(PimaIndiansDiabetes)

# se obtiene la matriz de correlacion de las variables predictoras
correlationMatrix <- cor(PimaIndiansDiabetes[,1:8])

# se encuentran aquellas variables que presentan valores de correlacion
# por encima del valor umbral
highlyCorrelated <- caret::findCorrelation(correlationMatrix, 
                                           cutoff=0.3)
print(highlyCorrelated)
