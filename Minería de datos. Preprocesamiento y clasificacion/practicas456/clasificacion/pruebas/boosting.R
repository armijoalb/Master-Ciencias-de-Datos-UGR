library(adabag)
library(caret)

# se carga el conjunto de datos
data(Vehicle)

# se hace un particionado sencillo del conjunto de datos
indices <- seq(1,nrow(Vehicle),by=1)
indicesEntrenamiento <- caret::createDataPartition(indices, p=0.8, list=FALSE)
datosEntrenamiento <- Vehicle[indicesEntrenamiento, ]
datosTest <- Vehicle[-indicesEntrenamiento, ]

# se realiza el aprendizaje. El argumento mfinal indica el numero
# de iteraciones del proceso, o lo que es lo mismo, el numeor de
# modelos que se construyen. El argumento maxdepth se usa por parte
# del paquete rpart, que es el usado para construir los arboles
# del ensamblado
modelo <- adabag::boosting(Class ~ ., data = datosEntrenamiento, 
                           mfinal = 10, 
                           control = rpart::rpart.control(maxdepth = 2))

# se muestra un grafico indicando la importancia relativa
# de las variables
barplot(modelo$imp[order(modelo$imp, decreasing = TRUE)],
           ylim = c(0, 100), main = "Variables Relative Importance",
           col = "lightblue")

# la prediccion se realizaria de la siguiente forma
prediccion <- predict.boosting(modelo, newdata = datosTest)

# se muestra el resultado. Se observa que la prediccion es de tipo probabilistico.
# Para cada una de las 36 instancias del conjunto de test se muestra la probabilidad
# de pertenencia a cada clase
print(modelo)

# se calcula el error sobre el conjunto de test
evol.test <- errorevol(modelo,newdata=datosTest)

# se calcula el error sobre el conjunto de entrenamiento
evol.train <- errorevol(modelo,newdata=datosEntrenamiento)

# se muestra la evolucion del error a medida que se construyen
# mas arboles
plot(evol.test$error, type="l", main="AdaBoost error Vs numero arboles",
     xlab="Arboles", ylab="Error", col = "red")

# igual con el error en el conjunto de entrenamiento
plot(evol.train$error, col="blue",type="l",
     main="AdaBoost error Vs numero arboles",
     xlab="Arboles", ylab="Error")
