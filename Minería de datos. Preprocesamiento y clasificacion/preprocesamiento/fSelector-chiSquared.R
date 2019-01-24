library(FSelector)
library(mlbench)
data(HouseVotes84)

# se calculan los pesos de los atributos: la medida devuelta
# indica el nivel de dependencia de cada atributo frente a la
# variable clase
weights <- FSelector::chi.squared(Class~.,HouseVotes84)
print(weights)

# se seleccionan los 5 mejores
subset <- FSelector::cutoff.k(weights,5)

# se muestran los seleccionados
f <- as.simple.formula(subset,"Class")
print(f)

