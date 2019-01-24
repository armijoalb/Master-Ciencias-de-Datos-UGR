library(FSelector)
library(mlbench)
data(HouseVotes84)

# se usa el metodo consistency para seleccionar el subconjunto
# de atributos. Este metodo usa a su vez la funcion best.first.search
# para determinar el subconjunto mas prometedor
subset <- consistency(Class~.,HouseVotes84)

# se muestra el resultado
f <- as.simple.formula(subset,"Class")
print(f)
