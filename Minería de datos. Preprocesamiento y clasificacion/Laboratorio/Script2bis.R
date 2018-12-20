################## Visualizacion Aproximaciones 3D ##########################
library(splines)
library(rgl)
library(gam)
#' @param new.device a logical value. If TRUE, creates a new device
#' @param bg the background color of the device
#' @param width the width of the device
rgl_init <- function(new.device = FALSE, bg = "white", width = 640) { 
  if( new.device | rgl.cur() == 0 ) {
    rgl.open()
    par3d(windowRect = 50 + c( 0, 0, width, width ) )
    rgl.bg(color = bg )
  }
  rgl.clear(type = c("shapes", "bboxdeco"))
  rgl.viewpoint(theta = 15, phi = 20, zoom = 0.7)
}


# Poned aqui el path donde tengais el archivo "MedicionRuido.csv"
setwd("/home/usuario/tmp/master2016_17/data")

datosE <- read.csv(file="MedicionRuido.csv",header = T, sep=",", stringsAsFactors = F)

datos <-data.frame(  y=datosE$Noise,
                     x1=datosE$Long,
                     x2=datosE$Lat)


# Descomenta solo uno de los siguientes modelos
 model.ruido <- lm (y~x1+x2, data = datos)
# model.ruido <- lm (y~poly(x1,3)+poly(x2,3), data = datos)
# model.ruido <- gam (y~s(x1,20)+s(x2,20), data = datos)


x <- datos$x1
y <- datos$y
z <- datos$x2

rgl_init()
rgl.spheres(x, y, z, r = 0.1, color = "red") 
#rgl_add_axes(x, y, z, show.bbox = FALSE)
aspect3d(1,1,1)
# predice los valores en la rejilla xz
grid.lines = 52
x.pred <- seq(min(x), max(x), length.out = grid.lines)
z.pred <- seq(min(z), max(z), length.out = grid.lines)
xz <- expand.grid( x1 = x.pred, x2 = z.pred)
y.pred <- matrix(predict(model.ruido, newdata = xz), 
                 nrow = grid.lines, ncol = grid.lines)
# Incorpora la superficie de regresión
rgl.surface(x.pred, z.pred, y.pred, color = "steelblue", 
            alpha = 0.5, lit = FALSE)  
# Incorpora la líneas
rgl.surface(x.pred, z.pred, y.pred, color = "black",
            alpha = 0.5, lit = FALSE, front = "lines", back = "lines")

