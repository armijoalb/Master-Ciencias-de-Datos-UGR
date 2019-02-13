# Cargamos las librerías necesarias y cargamos los ficheros.
library(xgboost)
library(foreign)
datos.ordinal = read.arff(file = 'Material/esl.arff')
datos.era = read.arff('Material/era.arff')
datos.swd = read.arff('Material/swd.arff')
datos.lev = read.arff('Material/lev.arff')

# Función para crear datasets binarios que se utilizarán con xgboost
generateBinaryDatasetXGBoost = function(data,class_label,class_position){
  indices = which(data[,class_position] >= class_label)
  new_dataset = data[,1:class_position-1]
  y = data[,class_position]
  y[indices] = 1
  y[-indices] = 0
  
  new_dataset = cbind(new_dataset,class=as.factor(y) )
}

# Función para generar un modelo monotónico con xgboost
generateXGBoostModel = function(data,pos_clase){
  real_data = as.matrix(data[,-pos_clase])
  labels = as.numeric(as.vector.factor(data[,pos_clase]))
  model = xgboost(real_data,labels,nrounds=50, verbose = 0, objective = "binary:logistic",
                  monotone_constraints=1)
}

# Función para generar los modelos binarios para datasets multicalse.
generalMonotonicXGBoost = function(datos, posicion_clase){
  clases_dataset = sort(unique(datos[,posicion_clase]))
  
  # La primera clase no la necesitamos ya que para ese caso todos los valores serán 1.
  clases_dataset = clases_dataset[-1]
  
  # Generamos los dataset correspondientes
  datasets = lapply(clases_dataset, generateBinaryDataset,data=datos,class_position=posicion_clase)
  cat("datasets binarios creados\n")
  
  # Generamos los modelos con xgboost
  modelos = lapply(datasets,generateXGBoostModel,pos_clase = posicion_clase)
  cat("modelos obtenidos\n")
  
  modelos
}

# Función para hacer predicción con xgboost.
predictModelXboost = function(models,data,pos_clase){
  prob_l = lapply(models,predict,as.matrix(data[,-pos_clase]))
  predictions_binary = lapply(prob_l,function(x) ifelse(x>0.5,1,0))
  predictions_binary = data.frame(matrix(unlist(predictions_binary), nrow=nrow(data),byrow = FALSE))
  predictions = apply(predictions_binary,1,function(x){
    prediction = 1 + sum(x)
  })
}


# Probamos a hacer predicción con todos los archivos.
modelos_xgboost = generalMonotonicXGBoost(datos.ordinal,ncol(datos.ordinal))
predictions_xgboost = predictModelXboost(modelos_xgboost,datos.ordinal,ncol(datos.ordinal))
acc = sum(predictions_xgboost == datos.ordinal$out1) / length(datos.ordinal$out1)
acc

modelos_xgboost_era = generalMonotonicXGBoost(datos.era,ncol(datos.era))
predictions_xgboost_era = predictModelXboost(modelos_xgboost_era,datos.era,ncol(datos.era))
acc_era = sum(predictions_xgboost_era == datos.era$out1) / length(datos.era$out1)
acc_era

modelos_xgboost_lev = generalMonotonicXGBoost(datos.lev,ncol(datos.lev))
predictions_xgboost_lev = predictModelXboost(modelos_xgboost_lev,datos.lev,ncol(datos.lev))
acc_lev = sum(predictions_xgboost_lev == datos.lev$Out1) / length(datos.lev$Out1)
acc_lev

modelos_xgboost_swd = generalMonotonicXGBoost(datos.swd,ncol(datos.swd))
predictions_xgboost_swd = predictModelXboost(modelos_xgboost_swd,datos.swd,ncol(datos.swd))
acc_swd = sum(predictions_xgboost_swd == datos.swd$Out1) / length(datos.swd$Out1)
acc_swd