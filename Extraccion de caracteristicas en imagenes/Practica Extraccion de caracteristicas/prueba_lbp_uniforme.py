import numpy as np
import cv2 as cv
import os
from functions import loadImages,crossValidation,loadCompresedData


if __name__ == "__main__":
    totalData = loadCompresedData('data/uniform_lbp_data_good.npz')
    totalClases = loadCompresedData('data/uniform_lbp_clases_good.npz')

    print("PRUEBAS CON LBP-Uniforme\n")

    cv_lineal = crossValidation(totalData,totalClases)
    print("Pruebas con SVM lineal:\n")
    print(str(cv_lineal['metrics_cv']))

    cv_radial = crossValidation(totalData,totalClases,kernelType=cv.ml.SVM_RBF)
    print("Pruebas con SVM radial:\n")
    print(str(cv_radial['metrics_cv']))

    cv_poli2 = crossValidation(totalData,totalClases,kernelType=cv.ml.SVM_POLY,degree_=2)
    print("Pruebas con SVM polinomial grado 2:\n")
    print(str(cv_poli2['metrics_cv']))

    cv_poli3 = crossValidation(totalData,totalClases,kernelType=cv.ml.SVM_POLY,degree_=3)
    print("Pruebas con SVM polinomial grado 3:\n")
    print(str(cv_poli3['metrics_cv']))

    cv_poli4 = crossValidation(totalData,totalClases,kernelType=cv.ml.SVM_POLY,degree_=4)
    print("Pruebas con SVM polinomial grado 4:\n")
    print(str(cv_poli4['metrics_cv']))

    cv_poli5 = crossValidation(totalData,totalClases,kernelType=cv.ml.SVM_POLY,degree_=5)
    print("Pruebas con SVM polinomial grado 5:\n")
    print(str(cv_poli5['metrics_cv']))

    cv_sigmoid = crossValidation(totalData,totalClases,kernelType=cv.ml.SVM_SIGMOID)
    print("Pruebas con SVM sigmoidal:\n")
    print(str(cv_sigmoid['metrics_cv']))

    cv_chi = crossValidation(totalData,totalClases,kernelType=cv.ml.SVM_CHI2)
    print("Pruebas con SVM Chi cuadrado:\n")
    print(str(cv_chi['metrics_cv']))

    cv_inter = crossValidation(totalData,totalClases,kernelType=cv.ml.SVM_INTER)
    print("Pruebas con SVM kernel Inter:\n")
    print(str(cv_inter['metrics_cv']))