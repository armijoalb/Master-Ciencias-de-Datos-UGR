import cv2 as cv
import numpy as np
from matplotlib import pyplot as plt
from functions import crossValidation,loadCompresedData

class PedestrianDetector:
    def __init__(self):
        self.model = cv.ml.SVM_load('./linear_svm.xml')
        self.desp = 32
        self.window_x = 64
        self.window_y = 128
        self.descriptor = cv.HOGDescriptor()

    
    def computeImage(self,image,desplazamiento=32):
        # TODO: Extraer una pirámide de la imagen a diferentes escalas (ej. original,1/2*original,
        # 1/4*original). Después pasar la ventana del descriptor por la imagen, utilizando desplazamiento de por
        # ejemplo 32 píxeles, tanto en x como en y. Guardar el resultado de la imagen, tras esto, obtener los resultados
        # y en aquellas ventas que den positivo (pedestrian), dibujar sobre las cordenadas iniciales de la imagen un rectángulo.
        pyramid,scales = self.createPyramid(image)

        real_data = [self.computeWindows(img,desp=desplazamiento) for img in pyramid]
        dict_data = dict(data=real_data,scale=scales)

        for use_data,scale in zip(dict_data['data'],dict_data['scale']):
            if use_data['win'].shape[0] > 0:
                pred = self.model.predict(use_data['win'])[1]
                pred_c = self.model.predict(use_data['win'],flags=cv.ml.STAT_MODEL_RAW_OUTPUT)[1]
                pos = np.where(pred == 1)[0]
                confidence = [self.computeConfidence(pred_c[i]) for i in pos]
                print(confidence)
                print("encontrados ",len(pos), "escala: ", scale)

                for i in range(len(pos)):
                    if(confidence[i] > 0.6):
                        pos_i = pos[i]
                        position = use_data['pos'][pos_i]
                        print(position[::-1])
                        positions = [position[1],position[0],position[1]+64,position[0]+128]

                        if(scale > 1):
                            positions = [int(i/scale) for i in positions[:2]]
                            print(positions)
                            positions.append(positions[0]+64)
                            positions.append(positions[1]+128)
                        else:
                            positions = [int(i/scale) for i in positions]

                        print(positions)
                        cv.rectangle(image,(positions[0],positions[1]),(positions[2],positions[3]),(0,0,255),2)

        plt.imshow(cv.cvtColor(image,cv.COLOR_BGR2RGB))
        plt.show()

        return None
    
    def computeWindows(self,image,desp=32):
        fin_y,fin_x = image.shape[:2]
        positions = [[y,x] for y in range(0,fin_y,desp)
                    for x in range(0,fin_x,desp) if (y+self.window_y) <= fin_y
                    if (x+self.window_x) <= fin_x ]

        windows = np.array([self.descriptor.compute(image[position[0]:position[0]+self.window_y
                            ,position[1]:position[1]+self.window_x]).flatten()
                     for position in positions])
        print(windows.shape)

        data = dict(win=windows,pos=positions)

        return data

    def computeConfidence(self,predicted_data):
        return 1.0 - (1.0 / (1.0 + np.exp(-predicted_data)))

    def createPyramid(self,original_image):
        images = []
        images.append(original_image)
        escaled = cv.pyrDown(original_image)
        images.append(escaled)
        images.append(cv.pyrDown(escaled))
        images.append(cv.resize(pedestrian,dsize=(0,0),fx=1.5,fy=1.5))
        return images,[1,0.5,0.25,1.5]



if __name__ == "__main__":
    detector = PedestrianDetector()
    pedestrian = cv.imread('pedestrian.jpg',cv.IMREAD_COLOR)
    detector.computeImage(pedestrian)
    pedestrian = cv.imread('pedestrian_2.jpeg',cv.IMREAD_COLOR)
    detector.computeImage(pedestrian,28)
    pedestrian = cv.imread('pedestrian_3.jpeg',cv.IMREAD_COLOR)
    detector.computeImage(pedestrian,27)
