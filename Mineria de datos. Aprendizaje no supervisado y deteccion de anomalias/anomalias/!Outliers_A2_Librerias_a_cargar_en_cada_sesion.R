# Máster -> Detección de anomalías
# Juan Carlos Cubero. Universidad de Granada

###########################################################################
# Paquetes usados a lo largo del curso
###########################################################################



#Gráficos
#install.packages("ggplot2")

library(ggplot2)

install.packages("devtools")
library(devtools)

#install_github("ggbiplot", "vqv")
library(reshape2)   # melt
library(ggbiplot)

install.packages("rgl")     #plot3D
library(rgl)
               
install.packages("GGally")  #ggpairs
library(GGally)


################################################################################
# 1-variate

install.packages("outliers")  # Grubb
library(outliers)

install.packages("EnvStats")  # Rosner
library(EnvStats)

################################################################################

# Multi-variate -Mahalanobis-

install.packages("mvoutlier")  #MCD ChiC
library(mvoutlier)       

install.packages("CerioliOutlierDetection")  #MCD Hardin Rocke
library(CerioliOutlierDetection)

install.packages("robustbase")
library(robustbase)

install.packages("mvnormtest")   # Test Normalidad multivariante
library(mvnormtest)


install.packages("MASS")
library(MASS) 

################################################################################

# Multivariate Unsupervised

install.packages("DMwR")  #lof
library(DMwR)

install.packages("cluster")
library(cluster)    # PAM

