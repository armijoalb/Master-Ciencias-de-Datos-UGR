library(caret)
library(pROC)

# se fija la semilla para asegurar la reproducibilidad de los 
# resultados
set.seed(7)

# carga el conjunto de datos
data(PimaIndiansDiabetes)

# prepara el esquema de entrenamiento
control <- caret::trainControl(method="repeatedcv", number=10, 
                               repeats=3)

# aprende el modelo
modelo <- caret::train(diabetes~., data=PimaIndiansDiabetes, 
                       method="lvq", preProcess="scale", 
                       trControl=control)

# estima la importancia de las variables a partir del modelo
importance <- caret::varImp(modelo, scale=FALSE)

# muestra los datos del analisis
print(importance)

# representa graficamente los resultados
plot(importance,lw=3)

