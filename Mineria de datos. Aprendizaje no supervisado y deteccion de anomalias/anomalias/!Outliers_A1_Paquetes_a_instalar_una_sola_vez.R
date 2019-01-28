# Máster -> Detección de anomalías
# Juan Carlos Cubero. Universidad de Granada

# Gráficos
install.packages("ggplot2")
install.packages("devtools")
library(devtools)

# ggbiplot:
install_github("vqv/ggbiplot")

# Si no funciona lo anterior, probar alguna de las siguientes:
#devtools::install_github("richardjtelford/ggbiplot", ref = "experimental")
#install_github("ggbiplot", "vqv")
#install_github("vqv/ggbiplot", ref = "experimental")


#install.packages("rgl")     #plot3D
#install.packages("GGally")  #ggpairs

# 1-variate
install.packages("outliers")  # Grubb
install.packages("EnvStats")  # Rosner

# Multi-variate -Mahalanobis-
install.packages("mvoutlier")  #MCD ChiC 
#install.packages("CerioliOutlierDetection")  #MCD Hardin Rocke estimación robusta de la matriz de covarianzas
#install.packages("robustbase")
#install.packages("mvnormtest")   # Test Normalidad multivariante
#install.packages("MASS")         # Para cov.rob estimación robusta de la matriz de covarianzas

# Multivariate Unsupervised
install.packages("DMwR")  #lof
install.packages("cluster")
