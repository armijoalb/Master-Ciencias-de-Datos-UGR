library(NoiseFiltersR)

# se selecciona el conjunto de datos iris
data(iris)

# se inicializa la semilla aleatoria para reproducir los resultados
set.seed(1)

# se aplica el algoritmo 
out <- IPF(Species~., data = iris, s = 2)

# se muestran los indices de las instancias con ruido
summary(out, explicit = TRUE)

# el conjunto de datos sin ruido se obtiene de la siguiente forma
out$cleanData

# tambien podriamos obtenerlo de forma directa eliminando los
# indices de las instancias consideradas como ruidosas
datosSinRuido <- iris[setdiff(1:nrow(iris),out$remIdx),]

