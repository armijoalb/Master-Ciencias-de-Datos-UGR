import cv2 as cv
import numpy as np

class LBP:
    def __init__(self,image):
        self.image = cv.cvtColor(image,cv.COLOR_RGB2GRAY)
        
    def checkPixel(self,pixel_value,image,x,y):
        value = 0

        try:
            if(image[y][x] >= pixel_value):
                value=1
        except:
            pass

        return value

    def computeLBPpixel(self,center_x,center_y,block):
        values = list()
        potencias_2 = np.array([2**i for i in range(8)])

        positions = [ [center_y-1,i] for i in range(center_x-1,center_x+2)]
        positions.append([center_y,center_x+1])
        positions.extend([[center_y+1,i] for i in range(center_x+1,center_x-2,-1)])
        positions.append([center_y,center_x-1])

        for pos_y, pos_x in positions:
            values.append(self.checkPixel(block[center_y][center_x],block,pos_x,pos_y))

        values = np.array(values)
        lbp_value = np.dot(values,potencias_2)

        return lbp_value

    def computeLBPblock(self,ini_x,ini_y,image):
        return [self.computeLBPpixel(x,y,image) for y in range(ini_y,ini_y+16) for x in range(ini_x,ini_x+16)]

    def computeLBPWindow(self,image,ini_x=0,ini_y=0):
        size_y, size_x = image.shape[:2]
        # TODO cambiar pos iniciales solamente para valores que y,x + 16 < size_y,size_x
        pos_iniciales = [[y,x] for y in range(ini_y,size_y,8) for x in range(ini_x,size_x,8)
                        if (x+16) <= size_x  and (y+16) <= size_y]

        lbp_hist = [self.computeLBPblock(x,y,image) for y,x in pos_iniciales]

        lbp_hist = np.array([np.array(block_hist) for block_hist in lbp_hist]).flatten()

        return lbp_hist
    
    def compute(self):
        return self.computeLBPWindow(self.image)
