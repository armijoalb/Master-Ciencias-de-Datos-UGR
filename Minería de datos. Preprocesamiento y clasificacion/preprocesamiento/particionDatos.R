library(caret)
data(Sonar)
set.seed(107)

# se crea la particion: esto obtiene de forma aleatoria un 
# porcentaje de instancias dado por p. El metodo mantiene 
# la proporcion deinstancias para cada valor de la variable 
# clase
inTrain <- caret::createDataPartition(y = Sonar$Class, p = .75, 
                                      list = FALSE)

# ahora se obtienen los conjuntos de test y de entrenamiento
training <- Sonar[ inTrain,]
testing  <- Sonar[-inTrain,]

# se muestra la proporcion de instancias para cada valor de la
# variable clase en el conjunto de datos original
summary(Sonar$Class)
ggplot(data=Sonar) + 
  geom_bar(mapping=aes(x=Class, y=..prop.., group=1))
            
# tambien en el de entrenamiento
summary(training$Class)
ggplot(data=training) + 
  geom_bar(mapping=aes(x=Class, y=..prop.., group=1))

# y lo mismo con el de test
summary(testing$Class)
ggplot(data=testing) + 
  geom_bar(mapping=aes(x=Class, y=..prop.., group=1))
