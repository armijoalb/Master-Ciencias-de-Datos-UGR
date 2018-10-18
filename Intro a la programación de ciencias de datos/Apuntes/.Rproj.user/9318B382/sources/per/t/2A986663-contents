#Matrices.

a=matrix(1:12,nrow=3,byrow=FALSE)
rownames(a)<-c("A","B","C")
colnames(a)=c('1','2','x','y')
dimnames(a)

#Indexing
a[1,1] # Único elemento.
a[1,] # Fila
a[,1] # Columna

#Unir vectores en una matriz.
v1=1:3
v2=4:6

cbind(v1,v2) # Unir por columas
rbind(v1,v2) # Unir por filas

# R hace las operaciones con matrices elemento a elemento, si se quiere operar en espacio vectorial o matricial
# hay que utilizar la función solve().


# Ejercicio 4.

x=month.name[1:4]
y=month.name[3:7]
setdiff(x,y)
intersect(x,y)


# Ejercicio 5
z=c(5,2,-3,8)
z2=z**2
z2[z2>8]

x=c(6,1:3,NA,12)
subset(x,x>5)

which(z2>8)

y=as.integer(runif(100,1,100))
ifelse(y%%2==0,5,12)

x <- c(5,2,9,12)
ifelse(x>6,x*2,x*3)
