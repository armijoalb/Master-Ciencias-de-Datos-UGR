# Máster -> Detección de anomalías
# Juan Carlos Cubero. Universidad de Granada

###########################################################################
# MULTIVARIATE STATISTICAL OUTLIERS. CLUSTERING OUTLIERS 
###########################################################################

# Los outliers son respecto a un conjunto de variables.


#####################################################################
# Lectura de valores y Preprocesamiento
#####################################################################

# Trabajamos sobre las columnas numéricas de iris [1:4]
# Este conjunto de datos está disponible en R
# Tanto LOF como clustering usan distancias entre registros, por lo que habrá
# que trabajar sobre los datos previamente normalizados

# Construimos los siguiente conjuntos:

# mis.datos.numericos -> con las columnas 1:4 de iris
# mis.datos.numericos.normalizados -> con los valores normalizados
# a Los rownames de mis.datos.numericos.normalizados les asignamos los rownames de mis.datos.numericos

# Establecemos la variable numero.de.outliers a 5 y numero.de.clusters a 3


mis.datos.numericos   = iris[,1:4]
#mis.datos.numericos   = mis.datos.originales[,sapply(mis.datos.originales, is.numeric)]
mis.datos.numericos.normalizados           = scale(mis.datos.numericos)
rownames(mis.datos.numericos.normalizados) = rownames(mis.datos.numericos)

numero.de.outliers   = 5
numero.de.clusters   = 3

set.seed(2)  # Para establecer la semilla para la primera iteración de kmeans


###########################################################################
# Cómputo de los outliers según la distancia euclídea de cada dato 
# al centroide de su cluster
# El centroide podrá ser cualquiera (podrá provenir de un k-means 
# o ser un medoide, por ejemplo) Empezamos con k-means
###########################################################################



###########################################################################
# k-Means

# Construimos el modelo kmeans (modelo.kmeans) con los datos normalizados. 
# Para ello, usamos la función de R llamada "kmeans"

# A partir del resultado de kmeans, accedemos a:

# a) $cluster para obtener 
#   los índices de asignación de cada dato al cluster correspondiente 
#   El resultado lo guardamos en la variable indices.clustering.iris
#   Por ejemplo, si el dato con índice 69 está asignado al tercer cluster,
#   en el vector indices.clustering.iris habrá un 3 en la componente número 69

# b) $centers para obtener los datos de los centroides.
#   Los datos están normalizados por lo que los centroides también lo están.
#   El resultado lo guardamos en la variable centroides.normalizados.iris


# indices.clustering.iris
# 1   2   3   4   ... 69  70  71 ...
# 1   1   1   1   ... 3   3   2  ...

# centroides.normalizados.iris
#    Sepal.Length Sepal.Width Petal.Length Petal.Width
# 1  -1.01119138  0.85041372   -1.3006301  -1.2507035
# 2   1.13217737  0.08812645    0.9928284   1.0141287
# 3  -0.05005221 -0.88042696    0.3465767   0.2805873



# COMPLETAR
modelo.kmeans = kmeans(mis.datos.numericos.normalizados, centers=numero.de.clusters)

indices.clusterings.iris = modelo.kmeans$cluster
indices.clusterings.iris
centroides.normalizados.iris = modelo.kmeans$centers
centroides.normalizados.iris

# -------------------------------------------------------------------------

# Calculamos la distancia euclídea de cada dato a su centroide (con los valores normalizados)
# Para ello, usad la siguiente función (intente entender cómo está implementada)

distancias_a_centroides = function (datos.normalizados, 
                                    indices.asignacion.clustering, 
                                    datos.centroides.normalizados){
  
  sqrt(rowSums(   (datos.normalizados - datos.centroides.normalizados[indices.asignacion.clustering,])^2   ))
}

# dist.centroides.iris
# 1          2          3             ......
# 0.21224719 0.99271979 0.64980753    ......

# Ordenamos dichas distancias a través de la función order y obtenemos
# los índices correspondientes. Nos quedamos con los primeros
# (tantos como diga la variable numero.de.outliers)

# top.outliers.iris
# [1]  42  16 132 118  61



# COMPLETAR
top.outliers.iris = distancias_a_centroides(mis.datos.numericos.normalizados,
                                            indices.clusterings.iris,
                                            centroides.normalizados.iris)
top.outliers.iris = order(top.outliers.iris,decreasing = TRUE)[1:numero.de.outliers]
top.outliers.iris



###########################################################################
# Creamos la función top_clustering_outliers para realizar las tareas anteriores
# top_clustering_outliers = function(datos.normalizados, 
#                                   indices.asignacion.clustering, 
#                                   datos.centroides.normalizados, 
#                                   numero.de.outliers)
# La función devolverá una lista con dos miembros:
# indices    -> Contiene los índices de los top outliers
# distancias -> Contiene las distancias a los centroides de los anteriores outliers


# Devuelve los índices de los top-k clustering outliers y sus distancias a los centroides



# COMPLETAR

top_clusterings_outliers = function(datos.normalizados=mis.datos.numericos.normalizados,
                                    indices.asignacion.clustering=indices.asignacion.clustering,
                                    datos.centroides.normalizados=centroides.normalizados.iris,
                                    numero.de.outliers=numero.de.outliers){
  
  top.outliers.iris = distancias_a_centroides(datos.normalizados,
                                              indices.asignacion.clustering,
                                              centroides.normalizados.iris)
  
  indices.top.outliers.iris = order(top.outliers.iris,decreasing = TRUE)[1:numero.de.outliers]
  
  mi.lista = list(indices=indices.top.outliers.iris,
                  distancias=top.outliers.iris[indices.top.outliers.iris]
                  )
  mi.lista
}



# Llamamos a la función top_clustering_outliers e imprimimos los índices y las distancias a sus 
# centroides de los outliers
# Tiene que salir lo siguiente:
# > top.outliers.kmeans$indices
# [1]  42  16 132 118  61
# > top.outliers.kmeans$distancias
# 42       16      132      118       61 
# 2.661639 2.390978 2.166200 2.094256 1.965365 



# COMPLETAR


top_clustering = top_clusterings_outliers(mis.datos.numericos.normalizados,
                                          indices.clusterings.iris,
                                          centroides.normalizados.iris,
                                          numero.de.outliers)

top_clustering$indices
top_clustering$distancias

###########################################################################
# Biplot de los outliers

# Creamos un vector is.kmeans.outlier de TRUE/FALSE que nos diga si cada
# registro es o no un outlier.

# is.kmeans.outlier
# [1] FALSE FALSE FALSE FALSE ....

# Para crear el Biplot llamamos a la función MiBiPlot_Clustering_Outliers
# Dentro de esta función se llama a la función ggbiplot, la cual está basada
# en la función ggplot que tiene un bug de diseño ya que dentro del parámetro aes
# sólo se pueden llamar a variables del entorno global y no del entorno local.
# Por tanto, desgraciadamente, debemos establecer variables globales que 
# son usadas dentro de nuestra función MiBiPlot_Clustering_Outliers.
# Dichas variables son BIPLOT.isOutlier, BIPLOT.cluster.colors y BIPLOT.asignaciones.clusters


numero.de.datos   = nrow(mis.datos.numericos)
is.kmeans.outlier = rep(FALSE, numero.de.datos) 
is.kmeans.outlier[top.outliers.kmeans$indices] = TRUE
# is.kmeans.outlier[top.outliers.kmeans.distancia.relativa] = TRUE


BIPLOT.isOutlier             = is.kmeans.outlier
BIPLOT.cluster.colors        = c("blue","red","green")     # Tantos colores como diga numero.de.clusters
BIPLOT.asignaciones.clusters = indices.clusterings.iris
MiBiPlot_Clustering_Outliers(mis.datos.numericos, "K-Means Clustering Outliers")




###########################################################################
###########################################################################
# AMPLIACIÓN
###########################################################################
###########################################################################



###########################################################################
# TODO LO QUE HAY A PARTIR DE AHORA ES DE AMPLIACIÓN
# RESUÉLVALO SÓLO CUANDO TERMINE EL RESTO DE FICHEROS
# POR LO TANTO, PASE AHORA A RESOLVER EL SIGUIENTE FICHERO

# ESTA PARTE DE AMPLIACIÓN ES OBLIGATORIO RESOLVERLA EN EL CASO DE QUE HAGA 
# EL TRABAJO DEL CURSO SOBRE LA PARTE DE ANOMALÍAS
###########################################################################




###########################################################################
#
# Los datos de los centroides construidos por el modelo están normalizados:
# centroides.normalizados.iris
#   Sepal.Length Sepal.Width Petal.Length Petal.Width
# 1  -1.01119138  0.85041372   -1.3006301  -1.2507035
# 2   1.13217737  0.08812645    0.9928284   1.0141287
# 3  -0.05005221 -0.88042696    0.3465767   0.2805873

# Queremos revertir la operación z-score para que nos quede así:
# centroides.valores
#   Sepal.Length Sepal.Width Petal.Length Petal.Width
# 1     5.006000    3.428000     1.462000    0.246000
# 2     6.780851    3.095745     5.510638    1.972340
# 3     5.801887    2.673585     4.369811    1.413208


# Para revertir la operación de normalización, simplemente tenemos que despejar
# en la fórmula:
# z-score = (dato - media.columna) / sd.columna
# dato = z-score * sd.columna + media.columna 

# Para aplicar la anterior fórmula, seguimos los siguientes pasos:

# Construimos un vector mis.datos.medias con las medias de cada columna (usad la función colMeans)
# mis.datos.medias
# Sepal.Length  Sepal.Width Petal.Length  Petal.Width 
# 5.843333     3.057333     3.758000      1.199333

# Construimos un vector mis.datos.desviaciones con las desviaciones típicas de cada columna.
# Para ello usamos apply con la función sd (standard deviation) 
# mis.datos.desviaciones
# Sepal.Length  Sepal.Width Petal.Length  Petal.Width 
# 0.8280661    0.4358663    1.7652982     0.7622377

# Ahora hay que multiplicar cada dato del centroide por la desviación de la correspondiente columna.
# es decir, tenemos que multiplicar centroides.normalizados.iris[i]  por mis.datos.desviaciones[i] 
# Para ello, usamos la función sweep con el operador producto "*", aplicándolo sobre las columnas (MARGIN = 2)

# Sepal.Length Sepal.Width Petal.Length Petal.Width
# 1  -0.83733333  0.37066667   -2.2960000  -0.9533333
# 2   0.93751773  0.03841135    1.7526383   0.7730071
# 3  -0.04144654 -0.38374843    0.6118113   0.2138742


# Finalmente, tenemos que sumar a dichos valores la media de la columna correspondiente
# para lo que volvemos a usar sweep con el anterior resultado y mis.datos.medias

# centroides.valores
# Sepal.Length Sepal.Width Petal.Length Petal.Width
# 1     5.006000    3.428000     1.462000    0.246000
# 2     6.780851    3.095745     5.510638    1.972340
# 3     5.801887    2.673585     4.369811    1.413208




# COMPLETAR





###########################################################################


# SI LE HA SOBRADO TIEMPO INTENTE HACER LO QUE VIENE A CONTINUACIÓN SIN MIRAR LA SOLUCIÓN
# EN CUALQUIER CASO, AL SER ESTA PARTE ALGO MÁS COMPLEJA, SE PROPORCIONA LA SOLUCIÓN
# PARA QUE PUEDA EJECUTARLO DIRECTAMENTE EN EL CASO DE QUE OPTE POR REALIZAR EL TRABAJO
# CORRESPONDIENTE A LA PARTE DE ANOMALÍAS.



###########################################################################
# AMPLIACIÓN

# Transparencia 128

# El objetivo es calcular la distancia de cada punto a su centroide usando la distancia de Mahalanobis.

# Vamos a construir la siguiente función:
# top_clustering_outliers_distancia_mahalanobis = function(datos, 
#                                                          indices.asignacion.clustering, 
#                                                          numero.de.outliers)

# Para hacerlo, tenemos que aislar en un mismo data frame aquellos registros que
# pertenezcan al mismo cluster, obteniendo así k data frames.
# El data frame -i- tendrá los valores (en todas las variables) de los registros
# que están en el cluster -i-
# A cada data frame, le calcularemos la matriz de covarianzas, necesaria
# para calcular las distancias de Mahalanobis de todos los registros
# de ese cluster al centro de su distribución.

# Así pues, realizamos el siguiente proceso:

# Construimos el data frame "seleccion", de forma que 
# seleccion[, i] será un vector de T/F indicando qué registros pertenecen al cluster i.
# seleccion
#     cluster 1   cluster 2   cluster 3
# 1   TRUE        FALSE        FALSE     -> El registro 1 está en el cluster 1
# 2   FALSE       FALSE        TRUE      -> El registro 2 está en el cluster 3
# 3   ....

# En el ejemplo del iris, nos quedaría:
#    [,1]  [,2]  [,3]
# 1   TRUE FALSE FALSE
# 2   TRUE FALSE FALSE
# ......
# 57  FALSE TRUE FALSE
# ......

# Así pues, datos.numericos[seleccion[, i] , ]  son los datos numéricos de los registros que pertenecen al cluster i
# Basta usar la función cov sobre las anteriores selecciones, para obtener las k matrices de covarianzas
# Guardamos las matrices de covarianzas en una lista lista.matriz.de.covarianzas (usad para ello la función lapply)
# Construimos también una lista lista.vector.de.medias, de forma que la lista i-ésima contendrá
# las medias de las variables de aquellos registros que están en el cluster -i-

# De forma alternativa, podemos usar la función cov.rob del paquete MASS
# Esta función realiza una estimación robusta de la matriz de covarianzas y de la media (Transparencia 94)
# Cunado apliquemos dicha función, accederemos a $cov para obtener la estimación robusta
# de la matriz de covarianzas y a $center para obtener la estimación robusta de la media.

# Ahora, basta obtener las distancias de Mahalanobis. Para ello, usamos la función mahalanobis
# a la que se le pasa como parámetros:
# - El data frame de datos. En nuestro caso serán cada uno de los data frames obtenidos a partir de "seleccion"
# - El vector que contiene la medias de las variables. En nuestro caso, será la componente correspondiente de lista.vector.de.medias
#   Nota: Para extraer una componente x de una lista L, debe usar L[[x]]
# - La matriz de covarianzas. En nuestro caso, será la componente correspondiente de lista.matriz.de.covarianzas
# Construimos la variable mah.distances aplicando lo anterior a los k data frames, usando la función lapply
# mah.distances es una lista de k listas. La lista -i- contiene las distancias de Mahalanobis del cluster -i-

# mah.distances
# [[1]]
# 1          2          3          ......
# 0.8032616  4.2108010  1.2007133  ......
# ......
# ......
# [[3]]
# 54         55         ......
# 2.7151536  4.6704382  ......

# Una vez obtenido mah.distances, ya sólo nos queda:
# - Unir todas las distancias en una única lista
# - Ordenar las distancias
# - Quedarnos con los top n

# La función devolverá una lista con:
# - Los índices de los top outliers
# - Las distancias de Mahalanobis de dichos outliers

# Llamamos a la función así construida con los datos de iris y mostramos el Biplot correspondiente.


top_clustering_outliers_distancia_mahalanobis = function(datos, 
                                                         indices.asignacion.clustering, 
                                                         numero.de.outliers){
  
  cluster.ids = unique(indices.asignacion.clustering)
  k           = length(cluster.ids)
  seleccion   = sapply(1:k, function(x) indices.asignacion.clustering == x)
  
  
  # Usando medias y covarianzas:
  # lista.matriz.de.covarianzas   = lapply(1:k, function(x) cov(mis.datos.numericos[seleccion[,x],]))
  # lista.vector.de.medias        = lapply(1:k, function(x) colMeans(mis.datos.numericos[seleccion[,x],]))
  
  
  # Usando la estimación robusta de la media y covarianza: (cov.rob del paquete MASS:
  lista.matriz.de.covarianzas   = lapply(1:k, function(x) cov.rob(mis.datos.numericos[seleccion[,x],])$cov)
  lista.vector.de.medias        = lapply(1:k, function(x) cov.rob(mis.datos.numericos[seleccion[,x],])$center)
  
  
  mah.distances   = lapply(1:k, 
                           function(x) mahalanobis(mis.datos.numericos[seleccion[,x],], 
                                                   lista.vector.de.medias[[x]], 
                                                   lista.matriz.de.covarianzas[[x]]))  
  
  todos.juntos = unlist(mah.distances)
  todos.juntos.ordenados = names(todos.juntos[order(todos.juntos, decreasing=TRUE)])
  indices.top.mah.outliers = as.numeric(todos.juntos.ordenados[1:numero.de.outliers])
  
  
  list(distancias = mah.distances[indices.top.mah.outliers]  , indices = indices.top.mah.outliers)
}

top.clustering.outliers.mah = top_clustering_outliers_distancia_mahalanobis(mis.datos.numericos, 
                                                                            indices.clustering.iris, 
                                                                            numero.de.outliers)

numero.de.datos = nrow(mis.datos.numericos)
is.kmeans.outlier.mah = rep(FALSE, numero.de.datos) 
is.kmeans.outlier.mah[top.clustering.outliers.mah$indices] = TRUE

BIPLOT.isOutlier             = is.kmeans.outlier.mah
BIPLOT.cluster.colors        = c("blue","red","brown")     # Tantos colores como diga numero.de.clusters
BIPLOT.asignaciones.clusters = indices.asignacion.clustering.kmeans
MiBiPlot_Clustering_Outliers(mis.datos.numericos, "K-Means Clustering Outliers")




###########################################################################
# AMPLIACIÓN: 

# Definir la función top_clustering_outliers_distancia_relativa
# Esta función hará lo mismo que la función top_clustering_outliers
# pero usando como criterio la distancia relativa 
# (pag. 127 de las transparencias)

top_clustering_outliers_distancia_relativa = function(datos.normalizados, 
                                                      indices.asignacion.clustering, 
                                                      datos.centroides.normalizados, 
                                                      numero.de.outliers){
  
  dist_centroides = distancias_a_centroides (datos.normalizados, 
                                             indices.asignacion.clustering, 
                                             datos.centroides.normalizados)
  
  cluster.ids = unique(indices.asignacion.clustering)
  k           = length(cluster.ids)
  
  distancias.a.centroides.por.cluster    = sapply(1:k , 
                                                  function(x) dist_centroides [indices.asignacion.clustering  == cluster.ids[x]])
  
  distancias.medianas.de.cada.cluster    = sapply(1:k , 
                                                  function(x) median(dist_centroides[[x]]))
  
  todas.las.distancias.medianas.de.cada.cluster  =  distancias.medianas.de.cada.cluster[indices.asignacion.clustering]
  ratios = dist_centroides   /  todas.las.distancias.medianas.de.cada.cluster
  
  indices.top.outliers           = order(ratios, decreasing=T)[1:numero.de.outliers]
  
  list(distancias = ratios[indices.top.outliers]  , indices = indices.top.outliers)
}



top.outliers.kmeans.distancia.relativa = top_clustering_outliers_distancia_relativa(mis.datos.numericos.normalizados, 
                                                                                    indices.clustering.iris, 
                                                                                    centroides.normalizados.iris, 
                                                                                    numero.de.outliers)


cat("Índices de los top k clustering outliers (k-means, usando distancia relativa)")
top.outliers.kmeans.distancia.relativa$indices 
cat("Distancias a sus centroides de los top k clustering outliers (k-means, usando distancia relativa)")
top.outliers.kmeans.distancia.relativa$distancias
