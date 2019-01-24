require(discretization)

# se usa el conjunto de datos de calidad del aire, en las
# mismas condiciones que vimos con anterioridad
datos <- iris

# discretizacion mediante metodo CAIM
cm <- discretization::disc.Topdown(iris, method=1)

# se muestran los puntos de corte
cat("Puntos de corte metodo CAIM: \n")
print(cm$cutp)

# los datos discretizados se mostrarian de la
# forma siguiente
cat("Datos discretizados: \n")
print(cm$Disc.data)

# discretizacion mediante CACC
cmCacc <- disc.Topdown(datos, method=2)

# se muestran los puntos de corte
cat("Puntos de corte metodo CACC: \n")
print(cm$cutp)

# discretizacion mediante AMEVA
cmAmeva <- disc.Topdown(datos, method=3)

# se muestran los puntos de corte
cat("Puntos de corte metodo AMEVA: \n")
print(cm$cutp)

