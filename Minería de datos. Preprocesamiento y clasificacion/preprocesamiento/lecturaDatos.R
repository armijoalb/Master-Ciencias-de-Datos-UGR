# funcion para lectura de archivo. En este script tambien se 
# consideran opciones de analisis exploratorio preliminar de 
# los datos. Argumentos:
# @param path ruta hasta el archivo de datos a leer
# @param file archivo a leer
lecturaDatos <- function(path,file){
  # se compone el path completo
  pathCompleto <- paste(path,file,sep="/")
  
  # se leen los datos
  dataset <- read.csv(pathCompleto,na.strings=c(".","NA","","?"))
  
  # se devuelve el conjunto de datos
  return(dataset)
}

# funcion para almacenar un conjunto de datos. Argumentos:
# @param path ruta donde se quiere almacenar
# @param file nombre del archivo donde almacenar los datos
# @param cdataset conjunto de datos a almacenar
escrituraDatos <- function(path, file, dataset){
  # se compone el path completo
  pathCompleto <- paste(path,file,sep="")
  
  # se escribe el archivo
  write.csv(dataset, pathCompleto, row.names = FALSE)
}

# se incluyen aqui algunas sentencia para ver la forma en 
# que se ejecutan las funciones anteriores

# se comienza leyendo un conjunto de datos en forma csv
datos <- lecturaDatos("./data/","datos.csv")

# se visualiza el tipo de objeto devuelto por la operacion
# de lectura de datos
class(datos)

