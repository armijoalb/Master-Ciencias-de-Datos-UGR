# Cargamos el dataset y las librerías correspondientes.
library(foreign)
library(RWeka)
library(e1071)
datos.ordinal = read.arff(file = 'Material/esl.arff')
datos.era = read.arff('Material/era.arff')
datos.swd = read.arff('Material/swd.arff')
datos.lev = read.arff('Material/lev.arff')

# función para generar datasets binarios a partir de un dataset normal para programación ordinal.
generateBinaryDataset = function(data,class_label,class_position){
  indices = which(data[,class_position] <= class_label)
  new_dataset = data[,1:class_position-1]
  y = data[,class_position]
  y[indices] = 0
  y[-indices] = 1
  
  new_dataset = cbind(new_dataset,class=as.factor(y) )
}

# Función para generar un modelo con J48
generateModel =  function(posicion_clase,datos){
  nombre_clase = names(datos)[posicion_clase]
  formula_modelo = as.formula(paste(nombre_clase,"~.",sep=""))
  model = J48(formula_modelo,data=datos)
}

# función para devolver solamente la probabilidad de que sea 1.
probBeingOne = function(data){
  data[,2]
}

# función para calcular las probabilidades dado una fila del dataset.
calculateProbs = function(data){
  p1 = 1-data[1]
  pfinal = data[length(data)-1]
  
  ps = sapply(2:(length(data)-1),function(pos){
    p = data[pos-1] * (1-data[pos])
  })
  
  ps = unlist(ps)
  
  myProbs = c(p1,ps,pfinal)
  myProbs = unlist(myProbs)
  
}
# función general para ejecutar el algoritmo con J48
generalOrdinalJ48 = function(datos, posicion_clase){
  # Obtenemos el número de clases y ordenamos el resultado.
  clases_dataset = sort(unique(datos[,posicion_clase]))
  
  # Eliminamos la última clase ya que no es necesario para calcular las probabilidades
  clases_dataset = clases_dataset[-length(clases_dataset)]
  
  # Generamos todos los modelos necesarios.
  datasets = lapply(clases_dataset,generateBinaryDataset,data=datos,class_position = posicion_clase)
  cat("creados datasets\n")
  
  # Generamos los modelos.
  modelos = lapply(datasets,generateModel,posicion_clase=posicion_clase)
  cat("modelos obtenidos\n")
  
  # Calculamos las probabilidades.
  probs = lapply(modelos, predict, newdata=datos[,-posicion_clase],type="probability")
  cat("haciendo predicciones")
  # Nos quedamos solamente con las que son 1 y las metemos en un dataset.
  prob_v = lapply(probs,probBeingOne)
  data_prob = data.frame(matrix(unlist(prob_v), nrow=nrow(datos), byrow = FALSE))
  
  cat("calculando probabilidades\n")
  real_probs = t(apply(data_prob,1,calculateProbs))
  prediction = apply(real_probs,1,which.max)
  prediction
}

# Obtenemos resultados para el algoritmo que utiliza J48
prediction_j48 = generalOrdinalJ48(datos.ordinal,ncol(datos.ordinal))
acc_j48 = sum(prediction_j48 == datos.ordinal$out1) / length(prediction_j48)
acc_j48

prediction_j48 = generalOrdinalJ48(datos.era,ncol(datos.era))
acc_era_j48 = sum(prediction_j48 == datos.era$out1) / length(prediction_j48)
acc_era_j48

prediction_j48 = generalOrdinalJ48(datos.lev,ncol(datos.lev))
acc_lev_j48 = sum(prediction_j48 == datos.lev$Out1) / length(prediction_j48)
acc_lev_j48

prediction_j48 = generalOrdinalJ48(datos.swd,ncol(datos.swd))
acc_swd_j48 = sum(prediction_j48 == datos.swd$Out1) / length(prediction_j48)


# función para generar un modelo con SVM, haciendo que tenga como salida probabilidades en vez de una clase.
generateSVMModel = function(posicion_clase, datos){
  nombre_clase = names(datos)[posicion_clase]
  formula_modelo = as.formula(paste(nombre_clase,"~.",sep=""))
  model = svm(formula_modelo,data=datos, probability = TRUE)
}

# Función para obtener las probabilidades de unos datos predecidos con el modelo SVM
probBeingOneSVM = function(data){
  attr(data,"prob")[,'1']
}

# Función para predecir la salida dados los modelos.
predictOrdinalSVM = function(modelos, datos_test){
  # Calculamos las probabilidades.
  probs = lapply(modelos, predict, newdata=datos_test,probability=TRUE)
  cat("haciendo predicciones")
  # Nos quedamos solamente con las que son 1 y las metemos en un dataset.
  prob_v = lapply(probs,probBeingOneSVM)
  data_prob = data.frame(matrix(unlist(prob_v), nrow=nrow(datos_test), byrow = FALSE))
  
  cat("calculando probabilidades\n")
  real_probs = t(apply(data_prob,1,calculateProbs))
  prediction = apply(real_probs,1,which.max)
  prediction
}

# Función para obtener los modelos necesarios para el algoritmo.
generalOrdinalSVM = function(datos, posicion_clase){
  # Obtenemos el número de clases y ordenamos el resultado.
  clases_dataset = sort(unique(datos[,posicion_clase]))
  
  # Eliminamos la última clase ya que no es necesario para calcular las probabilidades
  clases_dataset = clases_dataset[-length(clases_dataset)]
  
  # Generamos todos los modelos necesarios.
  datasets = lapply(clases_dataset,generateBinaryDataset,data=datos,class_position = posicion_clase)
  cat("creados datasets\n")
  
  # Generamos los modelos.
  modelos = lapply(datasets,generateSVMModel,posicion_clase=posicion_clase)
  cat("modelos obtenidos\n")
  
  modelos
}
# Probamos a ver los resultados con el algo
modelos_svm = generalOrdinalSVM(datos.ordinal,ncol(datos.ordinal))
prediction_svm = predictOrdinalSVM(modelos_svm,datos.ordinal[,-ncol(datos.ordinal)])
acc_svm = sum(prediction_svm == datos.ordinal$out1) / length(prediction_svm)
acc_svm

modelos_svm_era = generalOrdinalSVM(datos.era,ncol(datos.era))
prediction_svm_era = predictOrdinalSVM(modelos_svm_era,datos.era[,-ncol(datos.era)])
acc_svm_era = sum(prediction_svm_era == datos.era$out1) / length(prediction_svm_era)
acc_svm_era

modelos_svm_lev = generalOrdinalSVM(datos.lev,ncol(datos.lev))
prediction_svm_lev = predictOrdinalSVM(modelos_svm_lev,datos.lev[,-ncol(datos.lev)])
acc_svm_lev = sum(prediction_svm_lev == datos.lev$Out1) / length(prediction_svm_lev)
acc_svm_lev

modelos_svm_swd = generalOrdinalSVM(datos.swd,ncol(datos.swd))
prediction_svm_swd = predictOrdinalSVM(modelos_svm_swd,datos.swd[,-ncol(datos.swd)])
acc_svm_swd = sum(prediction_svm_swd == datos.swd$Out1) / length(prediction_svm_swd)
acc_svm_swd

"Para los datos de esl ambos modelos (SVM y J48) obtienen buenos resultados, en cambio el resto de dataset no; esto puede ser porque en el dataset esl
exista orden entre las clases y en el resto de datasets no."