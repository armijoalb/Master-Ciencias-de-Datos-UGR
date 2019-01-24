library(caret)
library(mlbench)
data(Sonar)
set.seed(107)

# se crea la particion: esto obtiene de forma aleatoria un porcentaje
# de instancias dado por p. El metodo mantiene la proporcion de instancias
# para cada valor de la variable clase
inTrain <- caret::createDataPartition(y = Sonar$Class, p = .75, list = FALSE)

# ahora se obtienen los conjuntos de test y de entrenamiento
training <- Sonar[ inTrain,]
testing  <- Sonar[-inTrain,]

# se muestra la proporcion de instancias para cada valor de la
# variable clase
summary(Sonar$Class)
summary(training$Class)
