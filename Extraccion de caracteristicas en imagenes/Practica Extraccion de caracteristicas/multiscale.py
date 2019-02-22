import cv2 as cv
import numpy as np
from matplotlib import pyplot as plt
from functions import crossValidation,loadCompresedData

class PedestrianDetector:
    def __init__(self,model):
        self.model = model
        self.desp = 32
        self.window_x = 64
        self.window_y = 128
        self.descriptor = cv.HOGDescriptor()
    
    def computeImage(self,image):
        # TODO: Extraer una pirámide de la imagen a diferentes escalas (ej. original,1/2*original,
        # 1/4*original). Después pasar la ventana del descriptor por la imagen, utilizando desplazamiento de por
        # ejemplo 32 píxeles, tanto en x como en y. Guardar el resultado de la imagen, tras esto, obtener los resultados
        # y en aquellas ventas que den positivo (pedestrian), dibujar sobre las cordenadas iniciales de la imagen un rectángulo.

        return
    
    def computeWindows(self,image,desp=32):
        fin_y,fin_x = image.shape[:2]
        positions = [[y,x] for y in range(0,fin_y,desp)
                    for x in range(0,fin_x,desp) if (y+self.window_y) <= fin_y
                    if (x+self.window_x) <= fin_x ]

        windows = np.array([self.descriptor.compute(image,locations=[position]).flatten()
                     for position in positions])

        return windows

    def createPyramid(self,original_image):
        images = []
        images.append(original_image)
        escaled = cv.pyrDown(original_image)
        images.append(escaled)
        images.append(cv.pyrDown(escaled))
        return images


image = cv.imread('ECI.Practica/data/train/pedestrians/AnnotationsPos_0.000000_crop_000010a_0.png',cv.IMREAD_COLOR)
img = cv.imread('ECI.Practica/Fry.jpg',cv.IMREAD_COLOR)
pedestrian = cv.imread('pedestrian.jpg',cv.IMREAD_COLOR)
plt.imshow(pedestrian)
plt.show()

# print(image.shape)
# print(img.shape)

hog = cv.HOGDescriptor()
locations = [(20,20)]
hist = hog.compute(img,locations=locations).flatten()
print(hist.shape)
hist = hog.compute(img,locations=[[0,0],[32,32]]).flatten()
print(hist.shape)
print(PedestrianDetector(model=0).computeWindows(img).shape)

other_img = cv.pyrDown(img)
print(img.shape)
print(other_img.shape)


pyramid = PedestrianDetector(model=0).createPyramid(pedestrian)
print(type(pyramid))
for image in pyramid:
    plt.imshow(cv.cvtColor(image,cv.COLOR_BGR2RGB))
    plt.show()

hogData = loadCompresedData('hog_data.npz')
hogClases = np.load('hog_clases.npy')
cv_hog = crossValidation(hogData,hogClases)



