library(mlbench)
library(FSelector)
data(HouseVotes84)

# se calculan los pesos
weights <- FSelector::random.forest.importance(Class~.,HouseVotes84, importance.type=1)

# se muestran los resultados
print(weights)
subset <- cutoff.k(weights,5)
f <- as.simple.formula(subset,"Class")
print(f)
