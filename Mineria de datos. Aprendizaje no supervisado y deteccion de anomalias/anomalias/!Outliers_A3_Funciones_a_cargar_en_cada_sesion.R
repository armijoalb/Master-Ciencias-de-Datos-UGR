# Máster -> Detección de anomalías
# Juan Carlos Cubero. Universidad de Granada

###########################################################################
# Funciones utilizadas a lo largo del curso
###########################################################################


rm(list=ls()) 


#######################################################################
# Muestra un plot básico con los outliers en rojo
# Necesita como parámetros de entrada:
# el dataset "datos", un vector de T/F indicando si el registro i-ésimo 
# de "datos" es o no un outlier (según el método aplicado) 
# y el título a aparecer en el gráfico

MiPlot_Univariate_Outliers = function (datos, indices_de_Outliers, titulo){
  numero.de.datos = nrow(as.matrix(datos))
  vectorTFoutliers =  rep(FALSE, numero.de.datos)
  vectorTFoutliers[indices_de_Outliers] = TRUE
  vector.colores.outlier = rep("black", numero.de.datos)
  vector.colores.outlier [vectorTFoutliers] = "red"
  
  cat("\nNúmero de datos: ")
  cat(numero.de.datos)
  cat("\n¿Quién es outlier?: ")
  cat(vectorTFoutliers)
  cat('\n')
  
  X11()
  plot(datos, col=vector.colores.outlier, main = titulo)
}



###########################################################################
# Calcula los outliers IQR 
# Devuelve un vector TRUE/FALSE indicando si el registro i-ésimo 
# de "datos" es o no un outlier IQR con respecto a la columna de índice "indice"
# coef es 1.5 para los outliers normales y hay que pasarle 3 para los outliers extremos

vector_es_outlier_IQR = function (datos, indice.de.columna, coef = 1.5){
  columna.datos = datos[,indice.de.columna]
  cuartil.primero = quantile(columna.datos)[2]  #quantile[1] es el mínimo y quantile[5] el máximo.
  cuartil.tercero = quantile(columna.datos)[4] 
  iqr = cuartil.tercero - cuartil.primero
  extremo.superior.outlier = (iqr * coef) + cuartil.tercero
  extremo.inferior.outlier = cuartil.primero - (iqr * coef)
  es.outlier  = columna.datos > extremo.superior.outlier |
    columna.datos < extremo.inferior.outlier
  return (es.outlier)
}

vector_claves_outliers_IQR = function(datos, indice, coef = 1.5){
  columna.datos = datos[,indice]
  vector.de.outliers = vector_es_outlier_IQR(datos, indice, coef)
  return (which(vector.de.outliers  == TRUE))
}



#######################################################################
# Devuelve los nombres de aquellas filas de datos especificadas 
# en el segundo parámetro (vector de T/F)

Nombres_de_Filas = function (datos, vector_TF_datos_a_incluir) {
  numero.de.filas = nrow(datos)
  
  if (is.null(row.names(datos)))
    row.names(datos) = rep(1:numero.de.filas)
  
  nombres.de.filas = rep("", numero.de.filas)
  nombres.de.filas[vector_TF_datos_a_incluir==TRUE] = 
        row.names(datos)[vector_TF_datos_a_incluir==TRUE]
  return (nombres.de.filas)
}



#######################################################################
# Calcula los outliers IQR y muestra sus etiquetas en un BoxPlot

MiBoxPlot_IQR_Univariate_Outliers = function (datos, indice.de.columna, coef = 1.5){
  # Importante: Para que aes busque los parámetros en el ámbito local, 
  # debe incluirse  environment = environment()
  
  datos = as.data.frame(datos)
  vector.TF.outliers.IQR = vector_es_outlier_IQR(datos, indice.de.columna, coef)
  nombres.de.filas = Nombres_de_Filas(datos, vector.TF.outliers.IQR)
  nombre.de.columna = colnames(datos, indice.de.columna)
  
  ggboxplot = ggplot(data = datos, 
                     aes(x=factor(""), 
                     y=datos[,indice.de.columna]) , 
                     environment = environment()) + 
    xlab(nombre.de.columna) + ylab("") +
    geom_boxplot(outlier.colour = "red") + 
    geom_text(aes(label = nombres.de.filas)) #, position = position_jitter(width = 0.1))   
  
  X11()
  ggboxplot
}



#######################################################################
# Muestra de forma conjunta todos los BoxPlots de las columnas de datos
# Para ello, normaliza los datos.
# También muestra con un punto en rojo los outliers de cada columna
# Requiere reshape


MiBoxPlot_juntos  = function (datos, vector_TF_datos_a_incluir = c()){  
  # Importante: Para que aes busque los parámetros en el ámbito local, 
  # debe incluirse  environment = environment()
  
  # Para hacerlo con ggplot, lamentablemente hay que construir antes una tabla 
  # que contenga en cada fila el valor que a cada tupla le da cada variable 
  # -> paquete reshape->melt
  
  # Por ejemplo, si tenemos el siguiente data frame
  
  # datos = data.frame(
  #   A  = c(1, 2),
  #   B = c(3, 4)
  # )
  # datos =
  #     A  B
  #     1  3
  #     2  4
  
  # melt(datos) construye esta tabla:
  
  #      variable value
  # 1        A     1
  # 2        A     2
  # 3        B     3
  # 4        B     4

  
  nombres.de.filas = Nombres_de_Filas(datos, vector_TF_datos_a_incluir)
  
  datos = scale(datos)
  datos.melted = melt(datos)
  colnames(datos.melted)[2]="Variables"
  colnames(datos.melted)[3]="zscore"
  factor.melted = colnames(datos.melted)[1]
  columna.factor = as.factor(datos.melted[,factor.melted])
  levels(columna.factor)[!levels(columna.factor) %in% nombres.de.filas] = ""  
  
  ggplot(data = datos.melted, aes(x=Variables, y=zscore), environment = environment()) + 
    geom_boxplot(outlier.colour = "red") + 
    geom_text(aes(label = columna.factor), size = 3) 
}



#######################################################################
# Muestra de forma conjunta todos los BoxPlots de las columnas de datos
# Para ello, normaliza los datos
# También muestra las etiquetas de los outliers de cada columna


MiBoxPlot_juntos_con_etiquetas = function (datos, coef = 1.5){
  # Aplicamos outlier IQR a cada columna
  
  matriz.datos.TF.outliers = 
    sapply(1:ncol(datos), function(x) vector_es_outlier_IQR(datos, x, coef))  
  
  vector.datos.TF.outliers = apply(matriz.datos.TF.outliers, 1, sum)   
  vector.datos.TF.outliers[vector.datos.TF.outliers > 1] = 1 # Si un registro es outlier en alguna columna lo incluimos
  
  MiBoxPlot_juntos(datos, vector.datos.TF.outliers)
}



#######################################################################
# Aplica el test de Grubbs e imprime los resultados

MiPlot_resultados_TestGrubbs = function(datos){
  alpha = 0.05
  
  test.de.Grubbs = grubbs.test(datos, two.sided = TRUE)
  cat('p.value: ')
  cat(test.de.Grubbs$p.value)
  cat('\n')
  
  if (test.de.Grubbs$p.value < alpha){
    indice.de.outlier.Grubbs = order(abs(datos - mean(datos)), decreasing = T)[1]
    indice.de.outlier.Grubbs
    cat('Índice de outlier: ')
    cat(indice.de.outlier.Grubbs)
    cat('\n')
    valor.de.outlier.Grubbs  = datos[indice.de.outlier.Grubbs]
    cat('Valor del outlier: ')
    cat(valor.de.outlier.Grubbs)
    MiPlot_Univariate_Outliers (datos, indice.de.outlier.Grubbs, "Test de Grubbs")
  }
  else
    cat('No hay outliers')
}


#######################################################################
# Aplica el test de Rosner e imprime los resultados

MiPlot_resultados_TestRosner = function(datos){  
  test.de.rosner = rosnerTest(datos, k=4)
  is.outlier.rosner = test.de.rosner$all.stats$Outlier
  k.mayores.desviaciones.de.la.media = test.de.rosner$all.stats$Obs.Num
  indices.de.outliers.rosner = k.mayores.desviaciones.de.la.media[is.outlier.rosner]
  valores.de.outliers.rosner = datos[indices.de.outliers.rosner]
  
  cat("\nTest de Rosner")
  cat("\nÍndices de las k-mayores desviaciones de la media: ")
  cat(k.mayores.desviaciones.de.la.media)
  cat("\nDe las k mayores desviaciones, ¿Quién es outlier? ")
  cat(is.outlier.rosner)
  cat("\nLos índices de los outliers son: ")
  cat(indices.de.outliers.rosner)
  cat("\nLos valores de los outliers son: ")
  cat(valores.de.outliers.rosner)
  
  MiPlot_Univariate_Outliers (datos, indices.de.outliers.rosner, "Test de Rosner")
}


MiBiplot = function(datos){
  PCA.model = princomp(scale(datos))
  biplot = ggbiplot(PCA.model, obs.scale = 1, var.scale=1 , varname.size = 5,alpha = 1/2) 
  X11()
  print(biplot)
}

MiBiPlot_Multivariate_Outliers = function (datos, vectorTFoutliers, titulo){
   identificadores_de_datos = rownames(datos)
   identificadores_de_datos[!vectorTFoutliers] = ''
   cat(identificadores_de_datos)
 
   PCA.model = princomp(scale(datos))
   outlier.shapes = c(".","x") #c(21,8)
   biplot = ggbiplot(PCA.model, 
                     obs.scale = 1, 
                     var.scale=1 , 
                     varname.size = 5,
                     groups =  vectorTFoutliers, 
                     alpha = 1/2) #alpha = 1/10
   biplot = biplot + labs(color = "Outliers")
   biplot = biplot + scale_color_manual(values = c("black","red"))
   biplot = biplot + geom_text(label = identificadores_de_datos, 
                               stat = "identity", 
                               size = 3, 
                               hjust=0, 
                               vjust=0)
   biplot = biplot + ggtitle(titulo)
  
   X11()
   print(biplot)
}


# Dentro de MiBiPlot_Clustering_Outliers se llama a la función ggbiplot, la cual está basada
# en la función ggplot que tiene un bug de diseño ya que dentro del parámetro aes
# sólo se pueden llamar a variables del entorno global y no del entorno local.
# Por tanto, desgraciadamente, debemos establecer variables globales que 
# son usadas dentro de nuestra función MiBiPlot_Clustering_Outliers:
# BIPLOT.isOutlier
# BIPLOT.asignaciones.clusters
# BIPLOT.cluster.colors

MiBiPlot_Clustering_Outliers = function (datos, titulo){
  PCA.model = princomp(scale(datos))
  outlier.shapes = c("o","x") #c(21,8)
  
  identificadores_de_datos = rownames(datos)
  identificadores_de_datos[!BIPLOT.isOutlier] = ''
  #cat(identificadores_de_datos)
  
  BIPLOT.asignaciones.clusters = factor(BIPLOT.asignaciones.clusters)
  
  biplot = ggbiplot(PCA.model, obs.scale = 1, var.scale=1 , varname.size = 3, alpha = 0) +              
    geom_point(aes(shape = BIPLOT.isOutlier, colour = factor(BIPLOT.asignaciones.clusters)))  +
    scale_color_manual(values = BIPLOT.cluster.colors) +
    scale_shape_manual(values = outlier.shapes) +
    ggtitle(titulo) +
    geom_text(label = identificadores_de_datos, stat = "identity", size = 3, hjust=0, vjust=0)      
  
  X11()
  print(biplot)
}



