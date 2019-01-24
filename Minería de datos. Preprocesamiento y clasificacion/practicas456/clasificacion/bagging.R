# la libreria rpart se usa para disponer de metodos de
# construccion de arboles de clasificacion
library(rpart)

# aqui hay datos considerados como de referencia
library(mlbench)

# libreria necesaria para funcionalidad de construccion
# de ensembles mediante bagging
library(adabag)

# se definen los datos a usar
data(Vehicle)

# se crean las particiones del conjunto de datos. En este caso
# se usa con conjunto de datos con 1000 instancias y 62 
# variables. Se generan las particiones del conjunto de datos 
# mediante la funcion createFolds, que genera 10 particiones
indices <- seq(1,nrow(Vehicle),by=1)
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
  modelo <- adabag::bagging(Class ~ ., 
                   data = Vehicle[particionesEntrenamiento[[i]], ], 
                   control=rpart::rpart.control(maxdepth=5, minsplit=15))
  cat("Aprendido modelo ",i,"\n")
  
  # se realizan las predicciones sobre el conjunto de test
  # asociado. Por supuesto es posible obtener las predicciones 
  # para las instancias del conjunto de test
  predicciones <- adabag::predict.bagging(modelo, 
                                          newdata=Vehicle[particionesTest[[i]], ])

  # se determinan las diferencias entre prediccion y valor real de 
  # la clase
  errores[i] <- predicciones$error
  aciertos[i] <- 1-predicciones$error
  cat("  Aciertos:", aciertos[i], " Errores: ", errores[i], "\n")
}

# se calcula la tasa de aciertos
tasaAciertos <- mean(aciertos/(errores+aciertos))

# usaremos estas particiones para hacer validacion cruzada
# Bucle de generacion de modelos
errores <- c()
aciertos <- c()
for(i in 1:10){
  # se cambian los parametros de control para la construccion de
  # los arboles. Aqui el modelo final contendra unicamente 5 arboles
  # y se limita su tam. a profuncidad maxima de tres
  modelo <- adabag::bagging(Class ~ ., 
                   data = Vehicle[particionesEntrenamiento[[i]], ],
                   mfinal=20,
                   control=rpart::rpart.control(maxdepth=3, minsplit=5))
  cat("Aprendido modelo ",i,"\n")
  
  # se realizan las predicciones sobre el conjunto de test
  # asociado. Por supuesto es posible obtener las predicciones 
  # para las instancias del conjunto de test
  predicciones <- adabag::predict.bagging(modelo, 
                                          newdata=Vehicle[particionesTest[[i]], ])

  # se determinan las diferencias entre prediccion y valor real de 
  # la clase
  errores[i] <- predicciones$error
  aciertos[i] <- 1-predicciones$error
  cat("  Aciertos:", aciertos[i], " Errores: ", errores[i], "\n")
}

# se calcula la tasa de aciertos
tasaAciertos <- mean(aciertos/(errores+aciertos))
