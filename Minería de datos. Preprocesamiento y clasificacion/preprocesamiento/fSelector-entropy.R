library(FSelector)
data(iris)

# se obtienen las medidas mediante ganancia de informacion
weights <- FSelector::information.gain(Species~., iris)

# se muestran los pesos y se seleccionan los mejores
print(weights)
subset <- FSelector::cutoff.k(weights,2)
f <- as.simple.formula(subset,"Species")
print(f)

# igual, pero con ganancia de informacion
weights <- FSelector::gain.ratio(Species~., iris)
print(weights)

# e igual con symmetrical.uncertainty
weights <- FSelector::symmetrical.uncertainty(Species~., iris)
print(weights)

