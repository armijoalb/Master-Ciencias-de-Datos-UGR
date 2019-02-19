"""
Fichero con funciones implementadas en python, por ejemplo la función necesaria para cargar las imagenes.
Alberto Armijo Ruiz.
"""

import cv2 as cv
import numpy as np
import os
from sklearn.model_selection import train_test_split
import pandas as pd


PATH_POSITIVE_TRAIN = "ECI.Practica/data/train/pedestrians/"
PATH_NEGATIVE_TRAIN = "ECI.Practica/data/train/background/"
PATH_POSITIVE_TEST = "ECI.Practica/data/test/pedestrians/"
PATH_NEGATIVE_TEST = "ECI.Practica/data/test/background/"
EXAMPLE_POSITIVE = PATH_POSITIVE_TEST + "AnnotationsPos_0.000000_crop_000011b_0.png"
EXAMPLE_NEGATIVE = PATH_NEGATIVE_TEST+"AnnotationsNeg_0.000000_00000002a_0.png"

def loadImages(descriptor_class):
    totalClases = []
    totalData = []
    

    totalData.extend([descriptor_class.compute(cv.imread(PATH_POSITIVE_TRAIN+file,cv.IMREAD_COLOR)).flatten() for file in os.listdir(PATH_POSITIVE_TRAIN)])
    totalClases.extend(1 for file in os.listdir(PATH_POSITIVE_TRAIN))
        
    print("Leidas " + str(len(
        [name for name in os.listdir(PATH_POSITIVE_TRAIN) if os.path.isfile(os.path.join(PATH_POSITIVE_TRAIN, name)) ]))
         + " imágenes de entrenamiento -> positivas")

    totalData.extend([descriptor_class.compute(cv.imread(PATH_NEGATIVE_TRAIN+file,cv.IMREAD_COLOR)).flatten() for file in os.listdir(PATH_NEGATIVE_TRAIN)])
    totalClases.extend(0 for file in os.listdir(PATH_NEGATIVE_TRAIN))
        
    print("Leidas " + str(len(
        [name for name in os.listdir(PATH_NEGATIVE_TRAIN) if os.path.isfile(os.path.join(PATH_NEGATIVE_TRAIN, name)) ]))
         + " imágenes de entrenamiento -> negativas")
    
    totalData.extend([descriptor_class.compute(cv.imread(PATH_POSITIVE_TEST+file,cv.IMREAD_COLOR)).flatten() for file in os.listdir(PATH_POSITIVE_TEST)])
    totalClases.extend(1 for file in os.listdir(PATH_POSITIVE_TEST))
    
        
    print("Leidas " + str(len(
        [name for name in os.listdir(PATH_POSITIVE_TEST) if os.path.isfile(os.path.join(PATH_POSITIVE_TEST, name)) ]))
         + " imágenes de entrenamiento -> positivas")

    totalData.extend([descriptor_class.compute(cv.imread(PATH_NEGATIVE_TEST+file,cv.IMREAD_COLOR)).flatten() for file in os.listdir(PATH_NEGATIVE_TEST)])
    totalClases.extend(0 for file in os.listdir(PATH_NEGATIVE_TEST))
       
    print("Leidas " + str(len(
        [name for name in os.listdir(PATH_NEGATIVE_TEST) if os.path.isfile(os.path.join(PATH_NEGATIVE_TEST, name)) ]))
         + " imágenes de entrenamiento -> negativas")
    
    totalData = np.array(totalData, dtype=np.float32)
    totalClases = np.array(totalClases,dtype=np.int32)
    
    
    return totalData, totalClases

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

def loadCompresedData(file_name):
    arr = np.load(file_name)
    arr = arr.f.arr_0
    return arr

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

def loadAllImages():
    totalClases = []
    totalData = []
    
    for file in os.listdir(PATH_POSITIVE_TRAIN):
        img = cv.imread(PATH_POSITIVE_TRAIN+file,cv.IMREAD_COLOR)
        
        hog = cv.HOGDescriptor()
        descriptor = hog.compute(img)
        totalData.append(descriptor.flatten())
        totalClases.append(1)
        
    print("Leidas " + str(len(
        [name for name in os.listdir(PATH_POSITIVE_TRAIN) if os.path.isfile(os.path.join(PATH_POSITIVE_TRAIN, name)) ]))
         + " imágenes de entrenamiento -> positivas")
        
    for file in os.listdir(PATH_NEGATIVE_TRAIN):
        img = cv.imread(PATH_NEGATIVE_TRAIN+file,cv.IMREAD_COLOR)
        
        hog = cv.HOGDescriptor()
        descriptor = hog.compute(img)
        totalData.append(descriptor.flatten())
        totalClases.append(0)
       
        
    print("Leidas " + str(len(
        [name for name in os.listdir(PATH_NEGATIVE_TRAIN) if os.path.isfile(os.path.join(PATH_NEGATIVE_TRAIN, name)) ]))
         + " imágenes de entrenamiento -> negativas")
    
    for file in os.listdir(PATH_POSITIVE_TEST):
        img = cv.imread(PATH_POSITIVE_TEST+file,cv.IMREAD_COLOR)
        
        hog = cv.HOGDescriptor()
        descriptor = hog.compute(img)
        totalData.append(descriptor.flatten())
        totalClases.append(1)
        
    print("Leidas " + str(len(
        [name for name in os.listdir(PATH_POSITIVE_TEST) if os.path.isfile(os.path.join(PATH_POSITIVE_TEST, name)) ]))
         + " imágenes de entrenamiento -> positivas")
        
    for file in os.listdir(PATH_NEGATIVE_TEST):
        img = cv.imread(PATH_NEGATIVE_TEST+file,cv.IMREAD_COLOR)
        
        hog = cv.HOGDescriptor()
        descriptor = hog.compute(img)
        totalData.append(descriptor.flatten())
        totalClases.append(0)
       
    print("Leidas " + str(len(
        [name for name in os.listdir(PATH_NEGATIVE_TEST) if os.path.isfile(os.path.join(PATH_NEGATIVE_TEST, name)) ]))
         + " imágenes de entrenamiento -> negativas")
    
    
    
    totalData = np.array(totalData)
    totalClases = np.array(totalClases,dtype=np.int32)
    
    
    return totalData, totalClases

def addTwoDescriptors(hogdesc,lbpdesc):
    united = [np.append(hog,lbp) for hog,lbp in zip(hogdesc,lbpdesc)]
    united = np.array(united,dtype=np.float32)
    return united
