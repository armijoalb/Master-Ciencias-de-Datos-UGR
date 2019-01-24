library(Boruta)
library(randomForest)

# tambien puede usarse en combinacion con random forest como
# forma de validar la seleccion
data(Ozone)

# se eliminan datos perdidos
ozo <- na.omit(Ozone)

# aplicacion del metodo de seleccion
Bor.ozo <- Boruta(V4~.,data=ozo,doTrace=2)

cat("Random forest sobre todos los atributos\n")
model1 <- randomForest(V4~.,data=ozo)
print(model1)

cat("Random forest unicamente sobre atributos confirmados\n")
model2 <- randomForest(ozo[,getSelectedAttributes(Bor.ozo)],ozo$V4)
print(model2)

# se muestra un grafico con los resultados de la seleccion
plot(Bor.ozo)
