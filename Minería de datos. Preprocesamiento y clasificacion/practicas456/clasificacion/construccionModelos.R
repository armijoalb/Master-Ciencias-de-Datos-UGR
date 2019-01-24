library(caret)
library(e1071)
data(GermanCredit)

# *********************** PARTE 1 *********************************

# se crean las particiones del conjunto de datos. En este caso
# se usa con conjunto de datos con 1000 instancias y 62 variables.
# Se generan las particiones del conjunto de datos mediante
# la funcion createFolds, que genera 10 particiones
indices <- seq(1,nrow(GermanCredit),by=1)
particionesEntrenamiento <- createFolds(indices, k = 10, 
                                        returnTrain = TRUE)

# genero de la misma forma las particiones de test
particionesTest <- list()
for(i in 1:10){
  particionesTest[[i]] <- indices[-particionesEntrenamiento[[i]]]
}

# usaremos estas particiones para hacer validacion cruzada
# Bucle de generacion de modelos
errores <- c()
aciertos <- c()
for(i in 1:10){
  # hay varias formas de especificar el objetivo del modelo a 
  # construir. Una de ellas es la formula: se compone de dos
  # terminos separados por el simbolo ~: a la izquierda va
  # la variable a predecir (clasificacion o regresion) y a la
  # derecha las variables a usar como predictoras. En el ejemplo
  # siguiente el punto a la derecha indica que se usan todos los
  # atributos como atributos. Como ejemplo veremos la aplicacion
  # del metodo de aprendizaje a una de las particiones
  modelo <- train(Class ~ ., 
                   data = GermanCredit[particionesEntrenamiento[[i]], ], 
                   method = "svmRadial")
  cat("Aprendido modelo ",i,"\n")
  
  # se realizan las predicciones sobre el conjunto de test
  # asociado
  # por supuesto es posible obtener las predicciones para las 
  # instancias del conjunto de test
  predicciones <- predict(modelo, GermanCredit[particionesTest[[i]], ])

  # se determinan las diferencias entre prediccion y valor real de 
  # la clase
  diferencias = (GermanCredit[particionesTest[[i]],"Class"] != predicciones)
  errores[i] <- length(which(diferencias == TRUE))
  aciertos[i] <- length(which(diferencias == FALSE))
}

# se calcula la tasa de aciertos
tasaAciertos <- mean(aciertos/(errores+aciertos))

# el modelo general propuesto se construiria ahora con todos los
# datos
modeloFinal <- train(Class ~ ., 
                     data = GermanCredit, method = "svmRadial")

# *********************** PARTE 2 *********************************

# tambien es posible aplicar algunas operaciones de preprocesamiento
# sobre los datos: centrado y escalado, por ejemplo, al emplear una
# tecnica de aprendizaje sensible a las dimensiones y escala de los
# datos
modelo2 <- train(Class ~ ., 
                 data = GermanCredit[particionesEntrenamiento[[1]], ], 
                 method = "svmRadial", preProc = c("center", "scale"))

# por supuesto es posible obtener las predicciones para las 
# instancias del conjunto de test
predicciones <- predict(modelo2, GermanCredit[particionesTest[[1]], ])
str(predicciones)

# se determinan las diferencias entre prediccion y valor real de 
# la clase
diferencias= (GermanCredit[particionesTest[[1]],"Class"] != predicciones)
errores <- length(which(diferencias == TRUE))
aciertos <- length(which(diferencias == FALSE))

# *********************** PARTE 3 *********************************

# este metodo tiene un parametro de coste que regula el coste asociado 
# a los errores de prediccion: las diferencias entre el valor predicho 
# y el real. Es posible evaluar diferentes valores de coste directamente, 
# haciendo uso del ultimo argumento (costes 2^-2, 2^-1, 
# 2^0, .... 2^7)
modelo3 <- train(Class ~ ., 
                 data = GermanCredit[particionesEntrenamiento[[1]], ], 
                 method = "svmRadial", preProc = c("center", "scale"), 
                 tuneLength = 10)

# y volvemos a hacer las predicciones
# por supuesto es posible obtener las predicciones para las 
# instancias del conjunto de test
predicciones <- predict(modelo3, GermanCredit[particionesTest[[1]], ])
str(predicciones)

# se determinan las diferencias entre prediccion y valor real de 
# la clase
diferencias= (GermanCredit[particionesTest[[1]],"Class"] != predicciones)
errores <- length(which(diferencias == TRUE))
aciertos <- length(which(diferencias == FALSE))

# puede imprimirse la informacion de los modelos considerados
plot(modelo3, scales = list(x = list(log = 2)))

# *********************** PARTE 4 *********************************

# tambien se puede modificar esta llamada para que se utilicen 
# diferentes particionados (que se generan de forma automatica 
# por parte de la funcion de aprendizaje). En este ejemplo se 
# realizan 5 repeticiones de validacion cruzada con k=10
# se inicializa la semilla (si interesa). De esta forma se
# automatiza de forma completo el proceso de aprendizaje y
# de estimacion. Se observa informacion sobre el particionado
# al analizar la salida del modelo
set.seed(1)
modelo4 <- train(Class ~ ., data = GermanCredit,  
                 method = "svmRadial", preProc = c("center", "scale"),
                 tuneLength=10,
                 trControl = trainControl(method = "repeatedcv", 
                                          repeats=5))

# se muestra la relacion entre el valor de C usado y la fiabilidad
# predictiva
plot(modelo4)

# *********************** PARTE 5 *********************************

# tambien es posible hacer comparacion con otros modelos. Imaginemos 
# un modelo de regresion logistica (clasificacion) para estos datos. 
# Seproduce la misma inicializacion de semilla
set.seed(1)
modeloRegLog <- train(Class ~ ., data=GermanCredit, method="glm", 
                      trControl= trainControl(method="repeatedcv", 
                                              repeats=5))

# la funcion resamples nos sirve para comparar estos modelos
resamp <- resamples(list(SVM = modelo4, Logistic = modeloRegLog))
summary(resamp)

# se obtienen las diferencias entre ambos modelos: supone realizar
# un test estadistico, siendo la hipotesis nula la igualdad (lo que
# indicaria que los modelos se comportan de forma similar)
diferencias <- diff(resamp)
summary(diferencias)

# Si los p-valores son muy bajos se acepta la hipotesis
# nula: los modelos son similares
