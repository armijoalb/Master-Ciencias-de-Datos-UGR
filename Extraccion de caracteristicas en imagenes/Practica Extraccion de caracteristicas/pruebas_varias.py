from functions import *
from lbp import LBP
import otro_lbp
import cv2 as cv
import numpy as np
import matplotlib.pyplot as plt
from uniform_lbp import ULBP

img = cv.imread('ECI.Practica/Fry.jpg',cv.IMREAD_COLOR)
img2 = cv.imread('ECI.Practica/data/train/pedestrians/AnnotationsPos_0.000000_crop_000010a_0.png',cv.IMREAD_COLOR)
d1 = otro_lbp.LBP().compute(img2)
d2 = LBP().compute(img2)
d3 = ULBP().compute(img2)
gray = cv.cvtColor(img2,cv.COLOR_RGB2GRAY)

print(d1)
print(d2)
print(d3)

print(gray)
bigger_image = np.pad(gray,1,'symmetric')
print(bigger_image)

print(d1.shape)
print(d3.shape)