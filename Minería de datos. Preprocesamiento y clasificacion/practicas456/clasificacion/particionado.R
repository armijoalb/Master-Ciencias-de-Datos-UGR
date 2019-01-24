# libreria que incluye algunos conjuntos de datos
# y asociada al libro de igual titulo
library(AppliedPredictiveModeling)
library(caret)

# se selecciona el conjunto de datos de interes: mtcars. Contiene
# 32 instancias y 11 variables: todas son numericas
data(mtcars)

# se obtiene resumen de los datos
summary(mtcars)

# se fija la semilla para el generador de numeros aleatorios,
# con lo que el experimento es reproducible. Para comportamiento
# normal deberia desactivarse esta opcion
set.seed(1)

# se genera un vector con tantos indices como instancias haya en
# el conjunto de datos
indices <- seq(1,nrow(mtcars),by=1)

# se pasa como argumento el vector de indices y de el se van
# seleccionando valores, con la probabilidad indicada. De esta
# forma, el vector resultante deberia tener aproximadamente
# el 80% de valores del pasado como primer argumento
indicesEntrenamiento <- caret::createDataPartition(indices, p=0.8, 
                                            list = FALSE)

# ahora se seleccionan los indices de los datos del conjunto de test
indicesTest <-indices[-indicesEntrenamiento]

# con esto es facil ahora seleccionar los conjuntos de datos
datosEntrenamiento <- mtcars[indicesEntrenamiento, ]
datosTest <- mtcars[indicesTest, ]

# si necesitamos obtener varias particiones, por ejemplo, para hacer
# validacion cruzada mediante metodo Montecarlo, podemos repetir
# el proceso el numero de veces que deseemos. Ahora el resultado
# sera una matriz con tantas filas como muestras y tantas columnas
# como repeticiones se hayan indicado
particionadosEntrenamiento <- caret::createDataPartition(indices, 
                                                         p = .80, 
                                                         list= FALSE, 
                                                         times = 10)

# De esta forma, cada columna contiene los indices de las instancias 
# seleccionada de una de las particiones
particion1 <- particionadosEntrenamiento[,1]
particion10 <- particionadosEntrenamiento[,10]

# ahora podemos obtener los indices de las instancias de test para
# cada caso. En primer lugar se calcula el numero de instancias en 
# el conjunto test. El objetivo es crear una matriz con tantas
# filas como tenga el conjunto de test y tantas columnas como 
# variantes del particionado se hayan generado (en este caso 10).
# De esta forma, cada columna servira para almacenar un conjunto
# de test. En primer lugar se determina el tam. del conjunto
# de test
tamConjuntoTest <- nrow(mtcars) - nrow(particionadosEntrenamiento)

# se crea una matriz para almacenar las sentencias de test de
# todas las particiones
particionadosTest <- matrix(nrow=tamConjuntoTest, ncol = 10)

# se obtienen de forma iterativa considerando las particiones de 
# entrenamiento
for(i in 1:10){
  particionadosTest[,i] <- indices[-particionadosEntrenamiento[,i]]
}

# tambien es posible usar createResamples (para bootstrap), 
# createFolds (para k-fold cross-validation) y createMultiFolds 
# (para repeticiones de cross-validation). Por ejemplo, imaginemos 
# deseamos crear 10 particiones para validacion cruzada: el siguiente 
# metodo me ofrece las particiones de forma que pueda usarlas 
# posteriormente
set.seed(8)
particionesEntrenamiento <- caret::createFolds(indices, k = 10, 
                                               list = TRUE,
                                        returnTrain = TRUE)

# el objeto devuelto ahora por R es una lista con tantas entradas
# como indique k. Cada una de las entradas es un vector con los
# indices de las instancias generadas
particion1 <- particionesEntrenamiento[[1]]
particion10 <- particionesEntrenamiento[[10]]

# de nuevo podemos usar la misma estrategia de antes para generar los 
# indices de las correspondientes particiones de test
particionesTest <- list()
for(i in 1:10){
  particionesTest[[i]] <- indices[-particionesEntrenamiento[[i]]]
}

# para que sea mas visible, vamos a considerar que tenemos 10
# instancias
indices <- seq(1,10)
particionesEntrenamiento <- caret::createFolds(indices, k = 10, 
                                        returnTrain = TRUE)

# e igual para los indices de test
particionesTest <- list()
for(i in 1:10){
  particionesTest[[i]] <- indices[-particionesEntrenamiento[[i]]]
}

# probamos el uso de la tecnica de bootstrap: ahora habra
# muestras repetidas y ademas todas las particiones tienen
# el mismo numero de muestras que el conjunto de datos original
particionBootstrap <- caret::createResample(indices, times=10)

# tambien podemos probar la creacion de multiplesParticiones: en este
# caso 3 particionados completos, de 10 particiones cada una de ellas
multiplesParticiones <- caret::createMultiFolds(indices, k=10, 
                                                times=3)

