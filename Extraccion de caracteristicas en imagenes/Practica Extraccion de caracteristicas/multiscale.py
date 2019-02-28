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

    def inRange(self,pos_1,pos_2,range=8):
        inRange = False
        ini_y = pos_1[0]-8
        fin_y = pos_1[0]+8
        ini_x = pos_1[0]-8
        fin_x = pos_1[0]+8

        if(pos_2[0] >= ini_y and pos_2[0] <= fin_y
            or pos_2[1] >= ini_x and pos_2[1] <= fin_x):
            inRange = True

        return inRange
    
    def checkIfInRange(self,rects,rect):
        in_range = False


        for r in rects:
            in_range = self.inRange(pos_1=r[:2],pos_2=rect[:2])
            if(in_range):
                break

        return in_range

    def compute2(self,image,desplazamiento=32):
        pyramid,scales =  self.createPyramid(image)
        real_data = []
        real_scales = []
        confidence = []
        total_positions = []
        rects = []
        
        # Computamos las imagenes que pueden contener la ventana de HOG.
        for pos in range(len(pyramid)):
            img = pyramid[pos]
            if(img.shape[0] >= self.window_y and img.shape[1] >= self.window_x):
                real_data.append(self.computeWindows(img,desplazamiento))
                real_scales.append(scales[pos])
        
        # Calculamos predicciones y confianza.
        for data,sc in zip(real_data,real_scales):
            pred = self.model.predict(data['win'])[1]
            pred_c = self.model.predict(data['win'],flags=cv.ml.STAT_MODEL_RAW_OUTPUT)[1]
            pos = np.where(pred==1)[0]
            print("Encontrados: ",len(pos)," escala:", sc)
            confidence.extend(self.computeConfidence(pred_c[i])[0] for i in pos)
            for p in pos:
                position = data['pos'][p]
                positions = [position[1],position[0],position[1]+64,position[0]+128]
                positions = [int(i/sc) for i in positions]
                total_positions.append(positions)
        
        print(total_positions,confidence)
        total_positions = np.array(total_positions)
        confidence = np.array(confidence)
        arg = np.argsort(confidence)[::-1]
        print(arg)
        dic_pos = dict(posi=total_positions,conf=confidence)
        print(dic_pos)
    
        confidence = confidence[arg]
        total_positions = total_positions[arg]
        rects.append(total_positions[0])
        for p in range(1,len(total_positions)):
            if(not self.checkIfInRange(rects,total_positions[p]) and confidence[p] > 0.6):
                rects.append(total_positions[p])

        print("Rects: ", rects)

        copy = image.copy()
        for rect in rects:
            cv.rectangle(copy,(rect[0],rect[1]),(rect[2],rect[3]),(0,0,255),2)

        plt.imshow(cv.cvtColor(copy,cv.COLOR_BGR2RGB))
        plt.title("Resultados detección")
        plt.show()

        return None

    
    def computeImage(self,image,desplazamiento=32):
        # TODO: Extraer una pirámide de la imagen a diferentes escalas (ej. original,1/2*original,
        # 1/4*original). Después pasar la ventana del descriptor por la imagen, utilizando desplazamiento de por
        # ejemplo 32 píxeles, tanto en x como en y. Guardar el resultado de la imagen, tras esto, obtener los resultados
        # y en aquellas ventas que den positivo (pedestrian), dibujar sobre las cordenadas iniciales de la imagen un rectángulo.
        pyramid,scales = self.createPyramid(image)

        copy = image.copy()

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
                        positions = [position[1],position[0],position[1]+64,position[0]+128]

                        if(scale > 1):
                            positions = [int(i/scale) for i in positions[:2]]
                            positions.append(positions[0]+64)
                            positions.append(positions[1]+128)
                        else:
                            positions = [int(i/scale) for i in positions]

                        cv.rectangle(copy,(positions[0],positions[1]),(positions[2],positions[3]),(0,0,255),2)

        plt.imshow(cv.cvtColor(copy,cv.COLOR_BGR2RGB))
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
    pedestrian = cv.imread('data/pedestrian.jpg',cv.IMREAD_COLOR)
    plt.imshow(cv.cvtColor(pedestrian,cv.COLOR_BGR2RGB))
    plt.title("Original")
    plt.show()
    detector.compute2(pedestrian)

    pedestrian = cv.imread('data/pedestrian_2.jpeg',cv.IMREAD_COLOR)
    
    plt.imshow(cv.cvtColor(pedestrian,cv.COLOR_BGR2RGB))
    plt.title("Original")
    plt.show()
    detector.compute2(pedestrian,28)

    pedestrian = cv.imread('data/pedestrian_3.jpeg',cv.IMREAD_COLOR)
    plt.imshow(cv.cvtColor(pedestrian,cv.COLOR_BGR2RGB))
    plt.title("Original")
    plt.show()
    detector.compute2(pedestrian,12)


