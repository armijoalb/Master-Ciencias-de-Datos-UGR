import numpy as np
import cv2 as cv
import os
from lbp import LBP
from uniform_lbp import ULBP
from functions import loadImages




newTotalData, newTotalClases = loadImages(cv.HOGDescriptor())
#np.save("lbp_data",newTotalData)
#np.save("lbp_clases",newTotalClases)

#newTotalDataU, newTotalClasesU = loadImages(ULBP)
#np.save("lbp_uniform_data",newTotalDataU)
#np.save("lbp_clases_uniform",newTotalClasesU)

#totalData = np.load('lbp_data.npy')
#totalClases = np.load('lbp_clases.npy')

#resultados_lineal = crossValidation(totalData,totalClases)

#print("Resultados LBP con modelo lineal:")
#print("La media de acierto de los modelos es:"+str(resultados_lineal['mean_accuracy']))
#print("El mejor modelo obtuvo un accuracy de:"+str(resultados_lineal['best_accuracy']))
#print("Métricas obtenidas en cada validación:\n"+str(resultados_lineal['metrics_cv']))

#resultados_rbf = crossValidation(totalData,totalClases,kernelType=cv.ml.SVM_RBF)

#print("Resultados LBP con modelo radial:")
#print("La media de acierto de los modelos es:"+str(resultados_rbf['mean_accuracy']))
#print("El mejor modelo obtuvo un accuracy de:"+str(resultados_rbf['best_accuracy']))
#print("Métricas obtenidas en cada validación:\n"+str(resultados_rbf['metrics_cv']))

#resultados_poli = crossValidation(totalData,totalClases,kernelType=cv.ml.SVM_POLY, degree_= 3)
#print("Resultados LBP con modelo polinomial:")
#print("La media de acierto de los modelos es:"+str(resultados_poli['mean_accuracy']))
#print("El mejor modelo obtuvo un accuracy de:"+str(resultados_poli['best_accuracy']))
#print("Métricas obtenidas en cada validación:\n"+str(resultados_poli['metrics_cv']))
