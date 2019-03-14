import numpy as np
import GPy
import scipy.io

if __name__ == "__main__":
    mat = scipy.io.loadmat('Datos.mat')
    healthy = mat['Healthy_folds'][0]
    malign = mat['Malign_folds'][0]
    print(malign[4][0].shape)