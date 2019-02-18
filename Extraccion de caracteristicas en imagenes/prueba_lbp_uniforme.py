import numpy as np
import cv2 as cv
import os
from lbp import LBP
from uniform_lbp import ULBP
from functions import loadImages,crossValidation


if __name__ == "__main__":
    totalData = np.load('lbp_uniform_data.npy')
    totalClases = np.load('lbp_clases_uniform.npy')

    print("PRUEBAS CON LBP-Uniforme\n")

    """ cv_lineal = crossValidation(totalData,totalClases)
    print("Pruebas con SVM lineal:\n")
    print(str(cv_lineal['metrics_cv']))

    cv_radial = crossValidation(totalData,totalClases,kernelType=cv.ml.SVM_RBF)
    print("Pruebas con SVM radial:\n")
    print(str(cv_radial['metrics_cv'])) """

    cv_poli2 = crossValidation(totalData,totalClases,kernelType=cv.ml.SVM_POLY,degree_=2)
    print("Pruebas con SVM polinomial grado 2:\n")
    print(str(cv_poli2['metrics_cv']))

    cv_poli3 = crossValidation(totalData,totalClases,kernelType=cv.ml.SVM_POLY,degree_=3)
    print("Pruebas con SVM polinomial grado 3:\n")
    print(str(cv_poli3['metrics_cv']))