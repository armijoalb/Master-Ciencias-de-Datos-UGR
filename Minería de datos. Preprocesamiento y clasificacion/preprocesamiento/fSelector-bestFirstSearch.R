library(rpart)
library(FSelector)
data(iris)

# Se define una funcion de evaluacion: recibe como argumento un 
# vector de atributos a evaluar
evaluator <- function(subset){
  # se indica el numero de particiones a realizar en el proceso
  # de validacion cruzada
  k <- 10
  
  # genera valores aleatorios (uniforme) para cada muestra del
  # conjunto de datos
  splits <- runif(nrow(iris))
  
  # tratamiento de cada una de las particiones. Para cada valor de
  # particion se aplica la funcion que se define a continuacion
  results <- sapply(1:k, function(i) {
    # se determina el indice de las muestras para test (aproximadamente
    # una fraccion 1/k de las muestras del conjunto de datos)
    test.idx <- (splits >= ((i-1)/k) & (splits < (i/k)))
    
    # todas las demas muestras seran para training
    train.idx <- !test.idx
    
    # se seleccionan las muestras en si
    test <- iris[test.idx, ,drop=FALSE]
    train <- iris[train.idx, , drop=FALSE]
    
    # aprende el modelo sobre el conjunto de entrenamiento
    tree <- rpart::rpart(as.simple.formula(subset,"Species"),train)
    
    # calcula la tasa de error
    error.rate <- sum(test$Species != predict(tree, test, type="class"))/nrow(test)
    
    # devuelve la tasa de aciertos
    return(1-error.rate)
  })
  
  # se muestra el subconjunto y la media de resultados y se devuelve
  # la media de los resultados (un resultado por particion)
  print(subset)
  print(mean(results))
  return(mean(results))
}

# con esta funcion de evaluacion la seleccion se haria de la forma siguiente
subset <- FSelector::best.first.search(names(iris)[-5], evaluator)
f <- as.simple.formula(subset,"Species")
print(f)
