library(Boruta)
library(mlbench)

# carga el conjunto de datos
data(Sonar)

# aprende el modelo
Bor.son <- Boruta(Class~.,data=Sonar,doTrace=2)

# muestra los resultados
print(Bor.son)

# se ven los resultados de decision de cada variable
print(Bor.son$finalDecision)

# imprime las estadisticas
stats <- attStats(Bor.son)
print(stats)

# se muestran los resultados en forma grafica
plot(Bor.son)

# muestra un grafico de los resultado: los valores en
# rojo estan relacionados con las variables confirmadas
# mientras que los verdes con variables descartadas
plot(normHits~meanImp,col=stats$decision,data=stats)
