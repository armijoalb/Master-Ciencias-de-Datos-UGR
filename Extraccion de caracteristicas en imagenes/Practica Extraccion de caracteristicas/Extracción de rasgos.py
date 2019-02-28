#!/usr/bin/env python
# coding: utf-8

# In[1]:


#get_ipython().run_line_magic('matplotlib', 'inline')
import cv2  as cv
from matplotlib import pyplot as plt
import numpy as np
import os
from sklearn.model_selection import train_test_split
import pandas as pd


# In[2]:


strImgPrueba = "ECI.Practica/Fry.jpg"
image = cv.imread(strImgPrueba,cv.IMREAD_COLOR)

# Linux funciona con BGR en vez de con RBG
image = cv.cvtColor(image,cv.COLOR_RGB2BGR)
print("Tamaño de la imagen: " + str(image.shape))

plt.imshow(image)
plt.show()


# In[3]:


gris = cv.cvtColor(image,cv.COLOR_BGR2GRAY)
blur = cv.blur(gris,(3,3))

canny = cv.Canny(gris,10,100)

plt.imshow(canny,cmap="gray")
plt.show()

harris = cv.cornerHarris(gris,2,3,0.04)

plt.imshow(harris,cmap="gray")
plt.show()


# In[4]:


# Cómputo de descriptores HOG
cv.normalize(harris, harris, 0, 255, cv.NORM_MINMAX)        
cv.convertScaleAbs(harris, harris)

hog = cv.HOGDescriptor()
descriptors = hog.compute(image)

d_size = descriptors.shape
blockSize = hog.blockSize
winSize = hog.winSize
blockStride = hog.blockStride
cellsize = hog.cellSize
n_bins = hog.nbins
descriptor_size = hog.getDescriptorSize()

print("HOG ("+str(descriptors.shape)+"): "+
    "block size: "+str(blockSize)+
    ", windows size: "+str(winSize)+
    ", stride size: "+str(blockStride)+
    ", cell size: "+str(cellsize) +
    ", number of bins " + str(n_bins)+
    ", descriptor size: "+str(descriptor_size))


# In[5]:


# Otro descriptor
fd = cv.FastFeatureDetector.create()
sift = fd.detect(gris)
siftImage = cv.drawKeypoints(gris,sift,np.array([]))

plt.imshow(siftImage,cmap="gray")
plt.show()


# In[6]:


# Direcciones de las imágenes
PATH_POSITIVE_TRAIN = "ECI.Practica/data/train/pedestrians/"
PATH_NEGATIVE_TRAIN = "ECI.Practica/data/train/background/"
PATH_POSITIVE_TEST = "ECI.Practica/data/test/pedestrians/"
PATH_NEGATIVE_TEST = "ECI.Practica/data/test/background/"
EXAMPLE_POSITIVE = PATH_POSITIVE_TEST + "AnnotationsPos_0.000000_crop_000011b_0.png"
EXAMPLE_NEGATIVE = PATH_NEGATIVE_TEST+"AnnotationsNeg_0.000000_00000002a_0.png"


# In[7]:


# Función para cargar las imágenes.
def loadTrainingData():
    trainingData = []
    classes = []
    
    for file in os.listdir(PATH_POSITIVE_TRAIN):
        img = cv.imread(PATH_POSITIVE_TRAIN+file,cv.IMREAD_COLOR)
        
        hog = cv.HOGDescriptor()
        descriptor = hog.compute(img)
        trainingData.append(descriptor.flatten())
        classes.append(np.ones((1,1),dtype=np.int32))
        
    print("Leidas " + str(len(
        [name for name in os.listdir(PATH_POSITIVE_TRAIN) if os.path.isfile(os.path.join(PATH_POSITIVE_TRAIN, name)) ]))
         + " imágenes de entrenamiento -> positivas")
        
    for file in os.listdir(PATH_NEGATIVE_TRAIN):
        img = cv.imread(PATH_NEGATIVE_TRAIN+file,cv.IMREAD_COLOR)
        
        hog = cv.HOGDescriptor()
        descriptor = hog.compute(img)
        trainingData.append(descriptor.flatten())
        classes.append(np.zeros((1,1),dtype=np.int32))
       
        
    print("Leidas " + str(len(
        [name for name in os.listdir(PATH_NEGATIVE_TRAIN) if os.path.isfile(os.path.join(PATH_NEGATIVE_TRAIN, name)) ]))
         + " imágenes de entrenamiento -> negativas")
    
    trainingData = np.array(trainingData)
    classes = np.array(classes,dtype=np.int32)
        
    return trainingData,classes


# In[8]:


trainingData,classes = loadTrainingData()


# In[9]:


# Función para crear un SVM y entrarnalo con las imágenes.
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


# In[10]:


svm = train(trainingData,classes)


# In[11]:


# Función para realizar una predicción.
def test(image,clasificador):
    # Computamos HOG.
    hog = cv.HOGDescriptor()
    descriptor = hog.compute(image)
    return clasificador.predict(np.mat(descriptor.flatten(),dtype=np.float32))


# In[12]:


# Función de ejemplo de clasificador.
def ejemploClasificadorImagenes():
    trainingData,classes = loadTrainingData()
    svm = train(trainingData,classes)
    print("Clasificador entrenado")
    
    image = cv.imread(EXAMPLE_POSITIVE,cv.IMREAD_COLOR)
    if(image.size == 0):
        print("Cannot load image")
        return

    plt.imshow(cv.cvtColor(image,cv.COLOR_BGR2RGB))
    plt.show()
    
    prediction = test(image,svm)
    print("Predicción: " + str(prediction))
    
# Ejemplo con la imagen positiva
ejemploClasificadorImagenes()


# In[13]:


# Función para leer las imágenes de test
def loadTestData():
    testData = []
    classes = []
    
    for file in os.listdir(PATH_POSITIVE_TEST):
        img = cv.imread(PATH_POSITIVE_TEST+file,cv.IMREAD_COLOR)
        
        testData.append(img)
        classes.append(np.ones((1,1),dtype=np.int32))
        
    print("Leidas " + str(len(
        [name for name in os.listdir(PATH_POSITIVE_TEST) if os.path.isfile(os.path.join(PATH_POSITIVE_TEST, name)) ]))
         + " imágenes de entrenamiento -> positivas")
        
    for file in os.listdir(PATH_NEGATIVE_TEST):
        img = cv.imread(PATH_NEGATIVE_TEST+file,cv.IMREAD_COLOR)
        
        testData.append(img)
        classes.append(np.zeros((1,1),dtype=np.int32))
       
        
    print("Leidas " + str(len(
        [name for name in os.listdir(PATH_NEGATIVE_TEST) if os.path.isfile(os.path.join(PATH_NEGATIVE_TEST, name)) ]))
         + " imágenes de entrenamiento -> negativas")
    
    testData = np.array(testData)
    classes = np.array(classes,dtype=np.int32)
        
    return testData,classes

testData,classesTest = loadTestData()


# In[14]:


def predictTest(clasif,testData):
    return [test(image, clasif) for image in testData]

predict_test = predictTest(svm, testData)
clases_imagenes = [dato[1] for dato in predict_test]
    
clases_imagenes = np.array(clases_imagenes, dtype=np.int32)

# Calculamos el accuracy.

iguales =  np.equal(clases_imagenes,classesTest, out=None)
accuracy = sum(iguales==True)/iguales.size
accuracy


# In[15]:


# Creamos una función para hacer cálculos de diferentes medidas de interés.
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

metricas = calculateMetrics(clases_imagenes,classesTest)
print(metricas)


# In[16]:


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


# In[17]:


totalData, totalClases = loadAllImages()


# In[18]:


# Probamos si funciona el split en train y test.
x_train,x_test,y_train,y_test = train_test_split(totalData,totalClases,test_size=0.2)
svm_prueba = train(x_train,y_train)
prediction = svm_prueba.predict(np.mat(x_test))[1].flatten()
print(prediction.shape)
print(y_test.shape)
calculateMetrics(prediction,y_test)


# In[19]:


# función para obtener solamente la predicción hecha por el modelo.
def obtainClassPredicted(data):
    class_predicted = [dato[1] for dato in data]
    return class_predicted


# función para hacer validación cruzada con el modelo de openCV
def crossValidation(data,labels,kfolds = 5, kernelType=cv.ml.SVM_LINEAR, degree_ = 2):
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
        model = train(x_train,y_train,kernelType,degree=degree_)
        # Hacemos la predicción de los modelos.
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

resultsCV = crossValidation(totalData,totalClases)


# In[20]:


print("La media de acierto de los modelos es:"+str(resultsCV['mean_accuracy']))
print("El mejor modelo obtuvo un accuracy de:"+str(resultsCV['best_accuracy']))
print("Métricas obtenidas en cada validación:\n"+str(resultsCV['metrics_cv']))


# In[21]:


# Probamos la función de train con un kernel diferente, y volvemos a hacer validación cruzada.
resultsCV_radialkernel = crossValidation(totalData,totalClases,kernelType=cv.ml.SVM_RBF)


# In[22]:


print("La media de acierto de los modelos es:"+str(resultsCV_radialkernel['mean_accuracy']))
print("El mejor modelo obtuvo un accuracy de:"+str(resultsCV_radialkernel['best_accuracy']))
print("Métricas obtenidas en cada validación:\n"+str(resultsCV_radialkernel['metrics_cv']))


resultsCV_poli = crossValidation(totalData,totalClases,kernelType=cv.ml.SVM_POLY)
print("La media de acierto de los modelos es:"+str(resultsCV_poli['mean_accuracy']))
print("El mejor modelo obtuvo un accuracy de:"+str(resultsCV_poli['best_accuracy']))
print("Métricas obtenidas en cada validación:\n"+str(resultsCV_poli['metrics_cv']))

resultsCV_poli = crossValidation(totalData,totalClases,kernelType=cv.ml.SVM_POLY,degree_=3)
print("La media de acierto de los modelos es:"+str(resultsCV_poli['mean_accuracy']))
print("El mejor modelo obtuvo un accuracy de:"+str(resultsCV_poli['best_accuracy']))
print("Métricas obtenidas en cada validación:\n"+str(resultsCV_poli['metrics_cv']))
