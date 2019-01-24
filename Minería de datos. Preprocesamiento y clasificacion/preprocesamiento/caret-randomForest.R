library(caret)
library(mlbench)

# se asigna la semilla para asegura la reproducibilidad de los 
# resultados
set.seed(7)

# carga el conjunto de datos
data(PimaIndiansDiabetes)

# define el control usando la funcion de seleccion mediante 
# random forest
control <- caret::rfeControl(functions=rfFuncs, method="cv", 
                             number=10)

# ejecuta el metodo
results <- caret::rfe(PimaIndiansDiabetes[,1:8], 
                      PimaIndiansDiabetes[,9], sizes=c(1:8), 
                      rfeControl=control)

# muestra los resultados
print(results)

# muestra las caracteristicas elegidas
predictors(results)

# realiza un grafico de los resultados. El grafico muestra que con
# 4 atributos se obtienen resultados simulares a usar los 8 atributos
# iniciales
plot(results, type=c("g", "o"), lw=2)

