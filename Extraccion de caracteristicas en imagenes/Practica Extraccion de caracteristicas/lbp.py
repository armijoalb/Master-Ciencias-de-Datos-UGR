import cv2 as cv
import numpy as np
from functions import loadImages
import time

class LBP:
    def __init__(self):
        self.window_width = 64
        self.window_heigth = 128
        self.block_width = 16
        self.block_heigth = 16
        self.desp_x = 8
        self.desp_y = 8
        self.potencias_2 = np.array([2**i for i in range(7,-1,-1)])
        
    def checkPixel(self,pixel_value,image,x,y):
        value = 0

        try:
            if(image[y][x] >= pixel_value):
                value=1
        except:
            pass

        return value

    def computeLBPpixel(self,center_x,center_y,block):
        positions = [ [center_y-1,i] for i in range(center_x-1,center_x+2)]
        positions.append([center_y,center_x+1])
        positions.extend([[center_y+1,i] for i in range(center_x+1,center_x-2,-1)])
        positions.append([center_y,center_x-1])

        pixel_value = block[center_y][center_x]

        positions = np.array(positions)

        values = [block[y,x] for y,x in positions]
        code = np.where(values>=pixel_value,1,0)
        # print(code)
        # print(self.potencias_2)

        lbp_value = np.dot(code,self.potencias_2)

        return lbp_value

    def computeLBPblock(self,ini_x,ini_y,image):
        return np.array([self.computeLBPpixel(x,y,image)
            for y in range(ini_y,ini_y+self.block_heigth)
            for x in range(ini_x,ini_x+self.block_width)],dtype=np.float)

    def computeLBPWindow(self,image,ini_x=1,ini_y=1):
        size_y, size_x = [self.window_heigth,self.window_width]
        # TODO cambiar pos iniciales solamente para valores que y,x + 16 < size_y,size_x
        pos_iniciales = [[y,x] for y in range(ini_y,size_y,self.desp_y) for x in range(ini_x,size_x,self.desp_x)
                        if (x+self.block_width) <= (size_x+ini_x)  and (y+self.block_heigth) <= (size_y+ini_y)]

        lbp_hist = [self.computeLBPblock(x,y,image) for y,x in pos_iniciales]

        lbp_hist = np.array([np.array(block_hist) for block_hist in lbp_hist]).flatten()

        return lbp_hist
    
    def compute(self,image):
        gray_image = cv.cvtColor(image,cv.COLOR_RGB2GRAY)
        bigger_image = np.pad(gray_image,1,'constant',constant_values = 255)
        return np.array(self.computeLBPWindow(bigger_image),dtype=np.float)

# image = cv.imread('ECI.Practica/data/train/pedestrians/AnnotationsPos_0.000000_crop_000010a_0.png',cv.IMREAD_COLOR)
# gray = cv.cvtColor(image,cv.COLOR_RGB2GRAY)
# bigger = np.pad(gray,1,'constant',constant_values=0)

# tiempo en cargar los datos 3314.53182387352 segundos.

# start = time.time()
# d,c = loadImages(LBP())
# stop = time.time()

# print(stop-start)
# np.savez_compressed('lbp_data_good',d)
# np.savez_compressed('lbp_clases_good',c)