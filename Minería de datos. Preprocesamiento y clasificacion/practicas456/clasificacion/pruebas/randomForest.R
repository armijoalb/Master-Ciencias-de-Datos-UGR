library(caret)
library(randomForest)

# se carga el conjunto de datos
data("GermanCredit")

# se aprende el modelo: random forest. Podemos especificar el
# numero de arboles a incluir en el bosque
modelo <- randomForest::randomForest(Class ~ ., data=GermanCredit, ntree=10)

# se muestra informacion del modelo
print(modelo)
 
# muestra la importancia de los atributos, teniendo en cuenta
# el modelo construido
randomForest::importance(modelo)

# se muestra informacion sobre los errores para cada una de las
# clases: la linea en negro indica el error medio 
plot(modelo)

# se aprende otrol modelo con mas arboles
modelo2 <- randomForest(Class ~ ., data=GermanCredit, ntree=100)

# se muestran los errores para cada etiqueta de la variable clase
plot(modelo2)

