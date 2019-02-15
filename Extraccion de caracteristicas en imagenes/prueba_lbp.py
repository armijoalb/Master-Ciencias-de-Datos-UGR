import numpy as np
import cv2 as cv
from sklearn.model_selection import train_test_split
import pandas as pd
import os
from lbp import LBP

PATH_POSITIVE_TRAIN = "ECI.Practica/data/train/pedestrians/"
PATH_NEGATIVE_TRAIN = "ECI.Practica/data/train/background/"
PATH_POSITIVE_TEST = "ECI.Practica/data/test/pedestrians/"
PATH_NEGATIVE_TEST = "ECI.Practica/data/test/background/"
EXAMPLE_POSITIVE = PATH_POSITIVE_TEST + "AnnotationsPos_0.000000_crop_000011b_0.png"
EXAMPLE_NEGATIVE = PATH_NEGATIVE_TEST+"AnnotationsNeg_0.000000_00000002a_0.png"


def train(trainingData,classes,kernel=cv.ml.SVM_LINEAR, degree = 2):

    params = dict(kernel_type = kernel,
            svm_type=cv.ml.SVM_C_SVC,
            degree=1)
    
    if(kernel == cv.ml.SVM_POLY):
        params['degree'] = degree
    
    svm = cv.ml.SVM_create()
    svm.setKernel(params['kernel_type'])
    svm.setType(params['svm_type'])
    svm.setDegree(params['degree'])
    svm.train(trainingData,cv.ml.ROW_SAMPLE,classes)
    
    return svm

def calculateMetrics(predictedData,realData):
    metrics = dict()
    
    true_positive = sum(np.logical_and(predictedData == 1,realData == 1) == True)
    false_positive = sum(np.logical_and(predictedData == 1,realData == 0) == True)
    false_negative = sum(np.logical_and(predictedData == 0, realData == 1) == True)
    true_negative = sum(np.logical_and(predictedData == 0, realData == 0) == True)
    
    # Calculamos accuracy
    metrics['accuracy'] = (true_positive + true_negative) / realData.size
    
    # Calculamos true positive rate (positivos predichos entre todos los positivos que hay)
    metrics['truepositiverate'] = (true_positive) / (true_positive + false_negative)
    
    # Calculamos true negative rate (negativos predichos entre todos los negativos que hay)
    metrics['truenegativerate'] = (true_negative) / (true_negative + false_positive)
    
    # Calculamos precision, es decir cuantos de los positivos son buenas predicciones.
    metrics['precision'] = (true_positive) / (true_positive + false_positive)
    
    # Calculamos F1-Score
    metrics['F1'] = 2*((metrics['precision']*metrics['truepositiverate'])/
                       (metrics['precision']+metrics['truepositiverate']))
    
    return metrics

def crossValidation(data,labels,kfolds = 5, kernelType=cv.ml.SVM_LINEAR, degree_=2):
    # variable para almacenar el mejor modelo.
    bestModel = 0
    # variable para almacenar los resultados de los modelos con los datos.
    metrics = []
    # variable para almacenar el mejor accuracy hasta el momento.
    bestAcc =  0
    # variable para calcular la media de accuracy
    meanAcc = []
    
    for i in range(kfolds):
        print("Computing validation "+str(i+1))
        # Creamos conjunto de train y test
        x_train, x_test, y_train, y_test = train_test_split(data, labels,test_size=0.2)
        
        # Entrenamos el modelo.
        print('Training model')
        model = train(x_train,y_train,kernelType,degree=degree_)
        # Hacemos la predicción de los modelos.
        print('Predicting results')
        pred_label = model.predict(x_test)[1].flatten()
        # Obtenemos los resultados.
        met = calculateMetrics(pred_label,y_test)
        metrics.append(met)
        
        meanAcc.append(met['accuracy'])
        
        # Actualizamos los resultados si el modelo es mejor.
        if(met['accuracy'] > bestAcc):
            bestAcc = met['accuracy']
            bestModel = model
    
    return dict(best_model=bestModel,best_accuracy=bestAcc,
                metrics_cv=pd.DataFrame(metrics),mean_accuracy=np.mean(meanAcc))

def loadImages():
    totalClases = []
    totalData = []

    totalData.extend([LBP(cv.imread(PATH_POSITIVE_TRAIN+file,cv.IMREAD_COLOR)).compute() for file in os.listdir(PATH_POSITIVE_TRAIN)])
    totalClases.extend(1 for file in os.listdir(PATH_POSITIVE_TRAIN))
        
    print("Leidas " + str(len(
        [name for name in os.listdir(PATH_POSITIVE_TRAIN) if os.path.isfile(os.path.join(PATH_POSITIVE_TRAIN, name)) ]))
         + " imágenes de entrenamiento -> positivas")

    totalData.extend([LBP(cv.imread(PATH_NEGATIVE_TRAIN+file,cv.IMREAD_COLOR)).compute() for file in os.listdir(PATH_NEGATIVE_TRAIN)])
    totalClases.extend(0 for file in os.listdir(PATH_NEGATIVE_TRAIN))
        
    print("Leidas " + str(len(
        [name for name in os.listdir(PATH_NEGATIVE_TRAIN) if os.path.isfile(os.path.join(PATH_NEGATIVE_TRAIN, name)) ]))
         + " imágenes de entrenamiento -> negativas")
    
    totalData.extend([LBP(cv.imread(PATH_POSITIVE_TEST+file,cv.IMREAD_COLOR)).compute() for file in os.listdir(PATH_POSITIVE_TEST)])
    totalClases.extend(1 for file in os.listdir(PATH_POSITIVE_TEST))
    
        
    print("Leidas " + str(len(
        [name for name in os.listdir(PATH_POSITIVE_TEST) if os.path.isfile(os.path.join(PATH_POSITIVE_TEST, name)) ]))
         + " imágenes de entrenamiento -> positivas")

    totalData.extend([LBP(cv.imread(PATH_NEGATIVE_TEST+file,cv.IMREAD_COLOR)).compute() for file in os.listdir(PATH_NEGATIVE_TEST)])
    totalClases.extend(1 for file in os.listdir(PATH_NEGATIVE_TEST))
       
    print("Leidas " + str(len(
        [name for name in os.listdir(PATH_NEGATIVE_TEST) if os.path.isfile(os.path.join(PATH_NEGATIVE_TEST, name)) ]))
         + " imágenes de entrenamiento -> negativas")
    
    totalData = np.array(totalData, dtype=np.float)
    totalClases = np.array(totalClases,dtype=np.int32)
    
    
    return totalData, totalClases


#newTotalData, newTotalClases = loadImages()
#np.save("lbp_data",newTotalData)
#np.save("lbp_clases",newTotalClases)

totalData = np.load('lbp_data.npy')
totalClases = np.load('lbp_clases.npy')
totalData = np.array(totalData,dtype=np.float32)

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

resultados_poli = crossValidation(totalData,totalClases,kernelType=cv.ml.SVM_POLY, degree_= 3)
print("Resultados LBP con modelo polinomial:")
print("La media de acierto de los modelos es:"+str(resultados_poli['mean_accuracy']))
print("El mejor modelo obtuvo un accuracy de:"+str(resultados_poli['best_accuracy']))
print("Métricas obtenidas en cada validación:\n"+str(resultados_poli['metrics_cv']))
