import cv2  as cv
from matplotlib import pyplot as plt

strImgPrueba = "ECI.Practica/Fry.jpg"
image = cv.imread(strImgPrueba,cv.IMREAD_COLOR)
print("Tamaño de la imagen: " + str(image.shape))

plt.imshow(image)
plt.show()

gris = cv.cvtColor(image,cv.COLOR_RGB2GRAY)
blur = cv.blur(gris,(3,3))

canny = cv.Canny(gris,10,100)

plt.imshow(canny)
plt.show()

harris = cv.cornerHarris(gris,2,3,0.04)

plt.imshow(harris)
plt.show()


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