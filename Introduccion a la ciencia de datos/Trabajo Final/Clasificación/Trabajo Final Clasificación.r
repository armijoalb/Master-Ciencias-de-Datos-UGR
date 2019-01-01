# Cambiar el path con la direcci�n de los archivos.
setwd("C:/Users/Alberto Armijo Ruiz/Universidad/Introduccion a la ciencia de datos/Trabajo Final/Clasificaci�n")
# Leemos el dataset
hayesroth = read.csv('hayes-roth//hayes-roth.dat', comment.char = '@', header = FALSE)
nombres = c('Hobby', 'Age', 'EducationalLevel', 'MaritalStatus','Class')
colnames(hayesroth) = nombres

head(hayesroth)
str(hayesroth)

# Calculamos la media y la desviación.
medias = sapply(hayesroth, mean)
desviaciones = sapply(hayesroth, sd)

# Creamos una tabla y mostramos los resultados.
medydesv = cbind(medias, desviaciones)
medydesv

# cargamos la librería.
library(ggplot2)

# Pintamos un histograma por cada variable. Dibujaremos la media y la mediana también.
ggplot(hayesroth, aes(x=Hobby))+
    geom_histogram(bins = length(unique(hayesroth$Hobby)), fill="lightblue", color="black")+
    geom_vline(aes(xintercept = mean(Hobby),color = "mean"), 
             linetype = "dashed", size = 0.6) +
    geom_vline(aes(xintercept = median(Hobby),color="median"),
                  linetype = "dashed", size = 0.6 ) +
    scale_color_manual(name = "statistics", values = c(mean = "#FC4E07", median = "blue"))

# Pintamos un histograma por cada variable. Dibujaremos la media y la mediana también.
ggplot(hayesroth, aes(x=Age))+
    geom_histogram(bins = length(unique(hayesroth$Age)), fill="lightblue", color="black")+
    geom_vline(aes(xintercept = mean(Age),color = "mean"), 
             linetype = "dashed", size = 0.6) +
    geom_vline(aes(xintercept = median(Age),color="median"),
                  linetype = "dashed", size = 0.6 ) +
    scale_color_manual(name = "statistics", values = c(mean = "#FC4E07", median = "blue"))

# Pintamos un histograma por cada variable. Dibujaremos la media y la mediana también.
ggplot(hayesroth, aes(x=EducationalLevel))+
    geom_histogram(bins = length(unique(hayesroth$EducationalLevel)), fill="lightblue", color="black")+
    geom_vline(aes(xintercept = mean(EducationalLevel),color = "mean"), 
             linetype = "dashed", size = 0.6) +
    geom_vline(aes(xintercept = median(EducationalLevel),color="median"),
                  linetype = "dashed", size = 0.6 ) +
    scale_color_manual(name = "statistics", values = c(mean = "#FC4E07", median = "blue"))

# Pintamos un histograma por cada variable. Dibujaremos la media y la mediana también.
ggplot(hayesroth, aes(x=MaritalStatus))+
    geom_histogram(bins = length(unique(hayesroth$MaritalStatus)), fill="lightblue", color="black")+
    geom_vline(aes(xintercept = mean(MaritalStatus),color = "mean"), 
             linetype = "dashed", size = 0.6) +
    geom_vline(aes(xintercept = median(MaritalStatus),color="median"),
                  linetype = "dashed", size = 0.6 ) +
    scale_color_manual(name = "statistics", values = c(mean = "#FC4E07", median = "blue"))

# Pintamos un histograma por cada variable. Dibujaremos la media y la mediana también.
ggplot(hayesroth, aes(x=Class))+
    geom_histogram(bins = length(unique(hayesroth$Class)), fill="lightblue", color="black")+
    geom_vline(aes(xintercept = mean(Class),color = "mean"), 
             linetype = "dashed", size = 0.6) +
    geom_vline(aes(xintercept = median(Class),color="median"),
                  linetype = "dashed", size = 0.6 ) +
    scale_color_manual(name = "statistics", values = c(mean = "#FC4E07", median = "blue"))

# Dibujamos los datos y pintamos dependiendo de la clase a la que pertenezcan.
plot(hayesroth[,1:4],col=hayesroth[,5])

ggplot(hayesroth,aes(y=Hobby,x=Age))+geom_point(aes(col=as.factor(Class)),size=2)+ facet_grid(~Class)

ggplot(hayesroth,aes(y=Hobby,x=EducationalLevel))+geom_point(aes(col=as.factor(Class)),size=2)+ facet_grid(~Class)

ggplot(hayesroth,aes(y=Hobby,x=MaritalStatus))+geom_point(aes(col=as.factor(Class)),size=2) + facet_grid(~Class)

ggplot(hayesroth,aes(y=Age,x=EducationalLevel))+geom_point(aes(col=as.factor(Class)),size=2) + facet_grid(~Class)

ggplot(hayesroth,aes(y=Age,x=MaritalStatus))+geom_point(aes(col=as.factor(Class)),size=2)+ facet_grid(~Class)

ggplot(hayesroth,aes(y=MaritalStatus,x=EducationalLevel))+
    geom_point(aes(col=as.factor(Class)),size=2)+ facet_grid(~Class)

library(reshape2)
temp = hayesroth
nhayes <- melt(temp, id.vars = "Class")

ggplot(nhayes, aes(x=variable, y=value, group=Class)) + 
    geom_point(aes(color=as.factor(Class))) +
                facet_grid(~ Class) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))

require(caret)
temp = hayesroth
temp = temp[,-1]
transform = preProcess(temp[1:3],method=c("BoxCox", "center", 
                            "scale", "pca"))
PCA = predict(transform, temp[1:3])
head(PCA)

pairs(PCA, col=temp$Class)

pairs(hayesroth,col=hayesroth[,5])

cor(hayesroth)

# cargamos la librería.
require(caret)

# Para k utilizaremos los valores impares del 3 al 15, para evitar empates dentro de knn.
ks = 3:15
ks = ks[ks%%2 != 0]

# Separamos las variables de entrada y las clases en dos variables distintas para poder utilizarlo con caret.
hr.train = hayesroth[,c("Age","EducationalLevel","MaritalStatus")]
hr.labels.train = hayesroth[,"Class"]
hr.labels.train = factor(hr.labels.train, levels=c(1,2,3))

# Normalizamos los datos ya que estamos trabajando con knn, aunque al tener
# la misma escala en todas las variables de entrada no es necesario.
hr.train = as.data.frame(lapply(hr.train,
                               scale, center = TRUE, scale = TRUE))

# creamos nuestro modelo y obtenemos nuestro mejor k.
knnModel <- train(hr.train,hr.labels.train,
                  method="knn", metric="Accuracy",
                  tuneGrid = data.frame(.k=ks))
knnModel

# dibujamos los resultados de cada k.
resultados_k = knnModel$results[,c("k","Accuracy")]

# cargamos la librería ggplot
require(ggplot2)

ggplot(resultados_k, aes(x=k, y=Accuracy)) + geom_point(col="deepskyblue") +
    geom_line() + scale_x_continuous(breaks=resultados_k$k)

knnModel <- train(hr.train,hr.labels.train,
                  method="knn", metric="Accuracy",
                  tuneGrid = data.frame(.k=3))
knnModel

knnPred = predict(knnModel,newdata=hr.train) 
acc=postResample(pred = knnPred, obs = hr.labels.train)[1]

nombre <- "hayes-roth/hayes-roth"

run_knn_fold <- function(i, x, tt = "test") {
    file <- paste(x, "-10-", i, "tra.dat", sep="")
    x_tra <- read.csv(file, comment.char="@", header=FALSE)
    file <- paste(x, "-10-", i, "tst.dat", sep="")
    x_tst <- read.csv(file, comment.char="@", header=FALSE)
    In <- length(names(x_tra)) - 1
    names(x_tra)[1:In] <- paste ("X", 1:In, sep="")
    names(x_tra)[In+1] <- "Y"
    names(x_tst)[1:In] <- paste ("X", 1:In, sep="")
    names(x_tst)[In+1] <- "Y"
    if (tt == "train") {
        test <- x_tra
    }
    else {
        test <- x_tst
    }
    
    hr.train = x_tra[,c("X2","X3","X4")]
    hr.labels.train = x_tra[,"Y"]
    hr.labels.train = factor(hr.labels.train, levels=c(1,2,3))
    fitMulti=train(hr.train,hr.labels.train,
                  method="knn", metric="Accuracy",
                  tuneGrid = data.frame(.k=3))
    
    labels = factor(test[,"Y"],levels=c(1,2,3))
    yprime=predict(fitMulti,newdata=test[,c("X2","X3","X4")])
    err = 1-postResample(pred = yprime, obs = labels)[1]
    
}
knnMSEtrain.all<-mean(sapply(1:10,run_knn_fold,nombre,"train"))
knnMSEtest.all<-mean(sapply(1:10,run_knn_fold,nombre,"test"))
print(knnMSEtrain.all)
print(knnMSEtest.all)

# Separamos las variables de entrada y las clases en dos variables distintas para poder utilizarlo con caret.
hr.train = hayesroth[,c("Age","EducationalLevel","MaritalStatus")]
hr.labels.train = hayesroth[,"Class"]
hr.labels.train = factor(hr.labels.train, levels=c(1,2,3))

# Normalizamos los datos.
hr.train = as.data.frame(lapply(hr.train,
                               scale, center = TRUE, scale = TRUE))

# Cargamos la librería necesaria para comprobar si las distribuciones son normales.
require(MASS) # lda y qda.

# Comprobamos si tienen una distribución normal y misma varianza.
shapiro.test(hr.train$Age)
shapiro.test(hr.train$EducationalLevel)
shapiro.test(hr.train$MaritalStatus)

var(hr.train$Age)
var(hr.train$EducationalLevel)
var(hr.train$MaritalStatus)

# Ejecutamos el algoritmo LDA.
temp = hr.train
temp$Class = hr.labels.train
lda.fit = lda(Class~Age+EducationalLevel+MaritalStatus,
             data=temp)
lda.fit

require(klaR)
partimat(Class~Age+EducationalLevel+MaritalStatus,
             data=temp, method="lda")

# Calculamos el error de LDA.
lda.pred = predict(lda.fit, temp[,-4])
mean(lda.pred$class!=temp$Class)

# Validación cruzada para LDA.
nombre <- "hayes-roth/hayes-roth"

run_knn_fold <- function(i, x, tt = "test") {
    file <- paste(x, "-10-", i, "tra.dat", sep="")
    x_tra <- read.csv(file, comment.char="@", header=FALSE)
    file <- paste(x, "-10-", i, "tst.dat", sep="")
    x_tst <- read.csv(file, comment.char="@", header=FALSE)
    In <- length(names(x_tra)) - 1
    names(x_tra)[1:In] <- paste ("X", 1:In, sep="")
    names(x_tra)[In+1] <- "Y"
    names(x_tst)[1:In] <- paste ("X", 1:In, sep="")
    names(x_tst)[In+1] <- "Y"
    if (tt == "train") {
        test <- x_tra
    }
    else {
        test <- x_tst
    }
    
    hr.train = x_tra[,c("X2","X3","X4")]
    hr.labels.train = x_tra[,"Y"]
    hr.labels.train = factor(hr.labels.train, levels=c(1,2,3))
    hr.train$Y = hr.labels.train
    fitMulti=lda(Y~X2+X3+X4, data=hr.train)
    labels = factor(test[,"Y"],levels=c(1,2,3))
    yprime=predict(fitMulti,test[,c("X2","X3","X4")])
    err = mean(yprime$class!=labels)
    
}
ldaERRtrain.all<-mean(sapply(1:10,run_knn_fold,nombre,"train"))
ldaERRtest.all<-mean(sapply(1:10,run_knn_fold,nombre,"test"))
print(ldaERRtrain.all)
print(ldaERRtest.all)

# Comprobamos que las varianzas de los predictores para las diferentes clases
# son diferentes.
cat("Varianzas para Age:\n")
var(temp[temp$Class==1,]$Age)
var(temp[temp$Class==2,]$Age)
var(temp[temp$Class==3,]$Age)
cat("Varianzas para EducationalLevel:\n")
var(temp[temp$Class==1,]$EducationalLevel)
var(temp[temp$Class==2,]$EducationalLevel)
var(temp[temp$Class==3,]$EducationalLevel)
cat("Varianzas para MaritalStatus:\n")
var(temp[temp$Class==1,]$MaritalStatus)
var(temp[temp$Class==2,]$MaritalStatus)
var(temp[temp$Class==3,]$MaritalStatus)

# creamos el modelo con QDA.
qda.fit = qda(Class~Age+EducationalLevel+MaritalStatus,
             data=temp)
qda.fit

# dibujamos los resultados obtenidos por QDA.
partimat(Class~Age+EducationalLevel+MaritalStatus,
             data=temp, method="qda")

# Calculamos el error para qda.
qda.pred = predict(qda.fit, temp[,-4])
mean(qda.pred$class!=temp$Class)

# Realizamos validación cruzada para QDA
nombre <- "hayes-roth/hayes-roth"

run_knn_fold <- function(i, x, tt = "test") {
    file <- paste(x, "-10-", i, "tra.dat", sep="")
    x_tra <- read.csv(file, comment.char="@", header=FALSE)
    file <- paste(x, "-10-", i, "tst.dat", sep="")
    x_tst <- read.csv(file, comment.char="@", header=FALSE)
    In <- length(names(x_tra)) - 1
    names(x_tra)[1:In] <- paste ("X", 1:In, sep="")
    names(x_tra)[In+1] <- "Y"
    names(x_tst)[1:In] <- paste ("X", 1:In, sep="")
    names(x_tst)[In+1] <- "Y"
    if (tt == "train") {
        test <- x_tra
    }
    else {
        test <- x_tst
    }
    
    hr.train = x_tra[,c("X2","X3","X4")]
    hr.labels.train = x_tra[,"Y"]
    hr.labels.train = factor(hr.labels.train, levels=c(1,2,3))
    hr.train$Y = hr.labels.train
    fitMulti=qda(Y~X2+X3+X4, data=hr.train)
    labels = factor(test[,"Y"],levels=c(1,2,3))
    yprime=predict(fitMulti,test[,c("X2","X3","X4")])
    err = mean(yprime$class!=labels)
    
}
qdaERRtrain.all<-mean(sapply(1:10,run_knn_fold,nombre,"train"))
qdaERRtest.all<-mean(sapply(1:10,run_knn_fold,nombre,"test"))
print(qdaERRtrain.all)
print(qdaERRtest.all)

# cargamos la librería que contiene los árboles de decisión.
require(tree)

temp=hayesroth
temp$Class = factor(temp$Class,levels=c(1,2,3))

set.seed (2)
train=sample (1:nrow(temp), round(nrow(temp)*0.8) )
hayes.test=temp[-train ,]

# Construyo el arbol sobre el conjunto de entrenamiento
tree.hayes =tree(Class~Age+EducationalLevel+MaritalStatus ,
                temp ,subset =train )

summary(tree.hayes)

# Aplico el arbol sobre el conjunto de test
tree.pred =predict (tree.hayes,hayes.test,type ="class")

# Visualizo la matriz de confusion
table(tree.pred , hayes.test[,"Class"])
mean(tree.pred!=hayes.test$Class)

# Dibujamos el árbol.
plot(tree.hayes)
text(tree.hayes, pretty=0)

# Validación cruzada para el árbol de decisión.
nombre <- "hayes-roth/hayes-roth"

run_knn_fold <- function(i, x, tt = "test") {
    file <- paste(x, "-10-", i, "tra.dat", sep="")
    x_tra <- read.csv(file, comment.char="@", header=FALSE)
    file <- paste(x, "-10-", i, "tst.dat", sep="")
    x_tst <- read.csv(file, comment.char="@", header=FALSE)
    In <- length(names(x_tra)) - 1
    names(x_tra)[1:In] <- paste ("X", 1:In, sep="")
    names(x_tra)[In+1] <- "Y"
    names(x_tst)[1:In] <- paste ("X", 1:In, sep="")
    names(x_tst)[In+1] <- "Y"
    if (tt == "train") {
        test <- x_tra
    }
    else {
        test <- x_tst
    }
    
    hr.train = x_tra[,c("X2","X3","X4")]
    hr.labels.train = x_tra[,"Y"]
    hr.labels.train = factor(hr.labels.train, levels=c(1,2,3))
    hr.train$Y = hr.labels.train
    fitMulti=tree(Y~X2+X3+X4, data=hr.train)
    labels = factor(test[,"Y"],levels=c(1,2,3))
    yprime=predict(fitMulti,test, type="class")
    err = mean(yprime!=labels)
    
}
treeERRtrain.all<-mean(sapply(1:10,run_knn_fold,nombre,"train"))
treeERRtest.all<-mean(sapply(1:10,run_knn_fold,nombre,"test"))
print(treeERRtrain.all)
print(treeERRtest.all)

nombre <- "hayes-roth/hayes-roth"

run_knn_fold <- function(i, x, tt = "test") {
    file <- paste(x, "-10-", i, "tra.dat", sep="")
    x_tra <- read.csv(file, comment.char="@", header=FALSE)
    file <- paste(x, "-10-", i, "tst.dat", sep="")
    x_tst <- read.csv(file, comment.char="@", header=FALSE)
    In <- length(names(x_tra)) - 1
    names(x_tra)[1:In] <- paste ("X", 1:In, sep="")
    names(x_tra)[In+1] <- "Y"
    names(x_tst)[1:In] <- paste ("X", 1:In, sep="")
    names(x_tst)[In+1] <- "Y"
    if (tt == "train") {
        test <- x_tra
    }
    else {
        test <- x_tst
    }
    
    hr.train = x_tra[,c("X1","X2","X3","X4")]
    hr.labels.train = x_tra[,"Y"]
    hr.labels.train = factor(hr.labels.train, levels=c(1,2,3))
    fitMulti=train(hr.train,hr.labels.train,
                  method="knn", metric="Accuracy",
                  tuneGrid = data.frame(.k=3))
    
    labels = factor(test[,"Y"],levels=c(1,2,3))
    yprime=predict(fitMulti,newdata=test[,c("X1","X2","X3","X4")])
    err = postResample(pred = yprime, obs = labels)[1]
    
}
knnACCtrain.all<-mean(sapply(1:10,run_knn_fold,nombre,"train"))
knnACCtest.all<-mean(sapply(1:10,run_knn_fold,nombre,"test"))
print(knnACCtrain.all)
print(knnACCtest.all)

nombre <- "hayes-roth/hayes-roth"

run_knn_fold <- function(i, x, tt = "test") {
    file <- paste(x, "-10-", i, "tra.dat", sep="")
    x_tra <- read.csv(file, comment.char="@", header=FALSE)
    file <- paste(x, "-10-", i, "tst.dat", sep="")
    x_tst <- read.csv(file, comment.char="@", header=FALSE)
    In <- length(names(x_tra)) - 1
    names(x_tra)[1:In] <- paste ("X", 1:In, sep="")
    names(x_tra)[In+1] <- "Y"
    names(x_tst)[1:In] <- paste ("X", 1:In, sep="")
    names(x_tst)[In+1] <- "Y"
    if (tt == "train") {
        test <- x_tra
    }
    else {
        test <- x_tst
    }
    
    hr.train = x_tra[,c("X1","X2","X3","X4")]
    hr.labels.train = x_tra[,"Y"]
    hr.labels.train = factor(hr.labels.train, levels=c(1,2,3))
    hr.train$Y = hr.labels.train
    fitMulti=lda(Y~X2+X3+X4, data=hr.train)
    labels = factor(test[,"Y"],levels=c(1,2,3))
    yprime=predict(fitMulti,test[,c("X1","X2","X3","X4")])
    err = mean(yprime$class==labels)
    
}
ldaACCtrain.all<-mean(sapply(1:10,run_knn_fold,nombre,"train"))
ldaACCtest.all<-mean(sapply(1:10,run_knn_fold,nombre,"test"))
print(ldaACCtrain.all)
print(ldaACCtest.all)

nombre <- "hayes-roth/hayes-roth"

run_knn_fold <- function(i, x, tt = "test") {
    file <- paste(x, "-10-", i, "tra.dat", sep="")
    x_tra <- read.csv(file, comment.char="@", header=FALSE)
    file <- paste(x, "-10-", i, "tst.dat", sep="")
    x_tst <- read.csv(file, comment.char="@", header=FALSE)
    In <- length(names(x_tra)) - 1
    names(x_tra)[1:In] <- paste ("X", 1:In, sep="")
    names(x_tra)[In+1] <- "Y"
    names(x_tst)[1:In] <- paste ("X", 1:In, sep="")
    names(x_tst)[In+1] <- "Y"
    if (tt == "train") {
        test <- x_tra
    }
    else {
        test <- x_tst
    }
    
    hr.train = x_tra[,c("X1","X2","X3","X4")]
    hr.labels.train = x_tra[,"Y"]
    hr.labels.train = factor(hr.labels.train, levels=c(1,2,3))
    hr.train$Y = hr.labels.train
    fitMulti=qda(Y~X2+X3+X4, data=hr.train)
    labels = factor(test[,"Y"],levels=c(1,2,3))
    yprime=predict(fitMulti,test[,c("X1","X2","X3","X4")])
    err = mean(yprime$class==labels)
    
}
qdaACCtrain.all<-mean(sapply(1:10,run_knn_fold,nombre,"train"))
qdaACCtest.all<-mean(sapply(1:10,run_knn_fold,nombre,"test"))
print(qdaACCtrain.all)
print(qdaACCtest.all)

# Leemos los datos de la tabla
resultados.test = read.csv("clasif_test_alumos.csv", header=TRUE)
row.names(resultados.test) = resultados.test$X
resultados.test$X = NULL

# substituimos los datos obtenidos por nuestro dataset.
resultados.test["hayes-roth",]$out_test_knn = knnACCtest.all
resultados.test["hayes-roth",]$out_test_lda = ldaACCtest.all
resultados.test["hayes-roth",]$out_test_qda = qdaACCtest.all
resultados.test

# Realizamos el test de Friedman.
friedman.test = friedman.test(as.matrix(resultados.test))
friedman.test

tam = dim(resultados.test)
groups = as.numeric(rep(1:tam[2],each=tam[1]))
pairwise.wilcox.test(as.matrix(resultados.test),groups,p.adjust="holm",paired=TRUE)

# Leemos la tabla y sustituimos los datos.
resultados.train = read.csv("clasif_train_alumnos.csv",header=TRUE)
row.names(resultados.train) = resultados.train$X
resultados.train$X = NULL

resultados.train["hayes-roth",]$out_train_knn = knnACCtrain.all
resultados.train["hayes-roth",]$out_train_lda = ldaACCtrain.all
resultados.train["hayes-roth",]$out_train_qda = qdaACCtrain.all

resultados.train

# Realizamos el test de friedman.
test.friedman = friedman.test(as.matrix(resultados.train))
test.friedman

# Relizamos el test de post-hoc. (Holm)
tam = dim(resultados.train)
groups = as.numeric(rep(1:tam[2],each=tam[1]))
pairwise.wilcox.test(as.matrix(resultados.train),groups,p.adjust="holm",paired=TRUE)
