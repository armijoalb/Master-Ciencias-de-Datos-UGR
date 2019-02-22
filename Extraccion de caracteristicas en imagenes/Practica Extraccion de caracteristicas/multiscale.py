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

        windows = np.array([self.descriptor.compute(image[position[0]:position[0]+self.window_y
                            ,position[1]:position[1]+self.window_x]).flatten()
                     for position in positions])
        print(windows.shape)

        data = dict(win=windows,pos=positions)

        return data

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
# plt.imshow(pedestrian)
# plt.show()

# print(image.shape)
# print(img.shape)

hog = cv.HOGDescriptor()
locations = [(20,20)]
hist = hog.compute(img,locations=locations).flatten()
print(hist.shape)
hist = hog.compute(img,locations=[[0,0],[32,32]]).flatten()
print(hist.shape)
detector = PedestrianDetector(0)
data = detector.computeWindows(pedestrian)
pedestrian2 = cv.pyrDown(pedestrian)
data2 = detector.computeWindows(pedestrian2)

pos = data['pos']

# for pos_y,pos_x in pos:
#     plt.imshow(pedestrian[pos_y:pos_y+128,pos_x:pos_x+64])
#     plt.show()
#     print(pos_y,pos_x)


hogData = loadCompresedData('hog_data.npz')
hogClases = np.load('hog_clases.npy')
#cv_hog = crossValidation(hogData,hogClases)
# model = cv_hog['best_model']
# hog = cv.HOGDescriptor()
hist = hog.compute(image).flatten()
hist2 = hog.compute(pedestrian[64:64+128,192:192+64]).flatten()
hist3 = data['win'][28]

model = cv.ml.SVM_load('./linear_svm.xml')
print(model)
print(model.predict(np.mat(hist3)))
print(model.predict(data2['win']))
print(data2['pos'][3])

position = data2['pos'][3]

positions = [position[1],position[0],position[1]+64,position[0]+128]

results = cv.rectangle(cv.cvtColor(pedestrian2,cv.COLOR_BGR2RGB),(positions[0],positions[1]),(positions[2],positions[3]),(255,0,0),1)
plt.imshow(results)
plt.show()

to_scale = np.multiply(positions,[2])
print(to_scale)
results_scale = cv.rectangle(cv.cvtColor(pedestrian,cv.COLOR_BGR2RGB),(to_scale[0],to_scale[1]),(to_scale[2],to_scale[3]),(255,0,0),1)
plt.imshow(results_scale)
plt.show()

pedestrian = cv.imread('pedestrian_2.jpeg',cv.IMREAD_COLOR)
data = detector.computeWindows(pedestrian)
data2 = detector.computeWindows(cv.pyrDown(pedestrian))

print(model.predict(data['win']))
pred = model.predict(data['win'])[1]
position = np.where(pred == 1)[0][0]
position = data['pos'][position]
positions = [position[1],position[0],position[1]+64,position[0]+128]
print(position)
results = cv.rectangle(cv.cvtColor(pedestrian,cv.COLOR_BGR2RGB),(positions[0],positions[1]),(positions[2],positions[3]),(255,0,0),1)
plt.imshow(results)

plt.show()

pedestrian = cv.imread('pedestrian_3.jpeg',cv.IMREAD_COLOR)
data = detector.computeWindows(pedestrian)
data2 = detector.computeWindows(cv.pyrUp(pedestrian))


pedestrian = cv.cvtColor(pedestrian,cv.COLOR_BGR2RGB)

print(model.predict(data['win']))
pred = model.predict(data['win'])[1]
pos = np.where(pred == 1)[0]
for pos_i in pos:
    position = data['pos'][pos_i]
    print(position)
    positions = [position[1],position[0],position[1]+64,position[0]+128]
    print(position)
    cv.rectangle(pedestrian,(positions[0],positions[1]),(positions[2],positions[3]),(255,0,0),1)

plt.imshow(pedestrian)
plt.show()

pred = model.predict(data2['win'])[1]
pos = np.where(pred == 1)[0]
for pos_i in pos:
    position = data2['pos'][pos_i]
    print(position)
    positions = [position[1],position[0],position[1]+64,position[0]+128]
    positions = np.multiply(np.array(positions),0.5)
    print(positions)
    print(position)
    cv.rectangle(pedestrian,(positions[0],positions[1]),(positions[2],positions[3]),(255,0,0),1)

plt.imshow(pedestrian)
plt.show()

# print(model.predict(np.mat(hist)))
# print(model.predict(np.mat(hist2)))
# print(model.predict(np.mat(hist3)))
