library(e1071)
library(caret)

# se cargan los datos
data(iris)

# se crea la particion: esto obtiene de forma aleatoria un porcentaje
# de instancias dado por p
inTrain <- caret::createDataPartition(y = iris$Species, p = .75, list = FALSE)

# ahora se obtienen los conjuntos de test y de entrenamiento
training <- iris[ inTrain,]
testing  <- iris[-inTrain,]

# se construye el modelo
model <- e1071::svm(Species~., data=training, method="C-classification", 
             kernel="radial", cost=10, gamma=0.1)

# se muestra informacion sobre el modelo
summary(model)

# se hace prediccion
pred <- predict(model, testing, decision.values = TRUE)

# para interpretarlo bien se trata como una tabla
tab <- table(pred = pred, true = testing[,5])

# se muestra la tabla
print(tab)
