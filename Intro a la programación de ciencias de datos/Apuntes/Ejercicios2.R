# Ejercicio 1.1
matrix(data=5, nr=2, nc=2)
matrix(1:6, 2, 3)
matrix(1:6, 2, 3, byrow=TRUE)

# Ejercicio 1.2
z=1:30
m=matrix(z,nrow=3,ncol=10)
m

# Ejercicio 1.3
vec=m[,3]
vec

# Ejercicio 1.4
m_x = matrix(data=c(3,1,'2l',1),nrow = 2,ncol=2)
m_x

m_y = matrix(data = c(1,0,4,1,0,-1),nrow=2,ncol=3)
m_y

# Ejercicio 1.5
m_x[1,]
m_x[2,]
m_x[,2]
m_y[1,2]
m_y[,2:3]

# Ejercicio 1.6
dim(m)
m_array = array(data=m,dim=dim(m))
m_array     

# Ejercicio 1.7
x = array(1:50,dim=c(5,5,2))
x

# Ejercicio 1.8
m1 = matrix(1, nrow=2, ncol=2); m1
m2 = matrix(2, nrow=2, ncol=2); m2

M1 = rbind(m1,m2); M1
M2 = cbind(m1,m2); M2

# Ejercicio 1.9
M12 = M1 %*% M2; M12

# Ejercicio 1.10
t_M1 = t(M1); t_M1

# Ejercicio 1.11
diag(m1)
diag(rbind(m1,m2) %*% cbind(m1,m2))
diag(m1) = 10; diag(m1)
diag(3)
v=c(10,20,30)
diag(v)
diag(2.1, nrow=3, ncol=5)

# Ejercicio 1.12
x = matrix(1:100, ncol=10)
x


# Ejercicio 1.13
x1 = x
x1 = x[order(x[,2],decreasing=T),]
x1
               
x2 = x
x2 = x[order(x[2,],decreasing = T),]
x2

x[,1] = x[order(x[,1],decreasing=T),1]
x

# Ejercicio 1.14
new_hope = c(460.998007, 314.4)
empire_strikes = c(290.475067, 247.9)
return_jedi = c(309.306177, 165.8)
star_wars_matrix = matrix(rbind(new_hope,empire_strikes,return_jedi), ncol=2); star_wars_matrix
colnames(star_wars_matrix)=c('US','Non-US')
rownames(star_wars_matrix)=c('new_hope','empire_strikes','return_jedi')
star_wars_matrix

worldwide_vector=c(sum(star_wars_matrix[1,]),sum(star_wars_matrix[2,]),sum(star_wars_matrix[3,]))
worldwide_vector

all_wars_matrix = cbind(star_wars_matrix,worldwide_vector)
colnames(all_wars_matrix)=c('US','Non-US','Total'); all_wars_matrix

colSums(star_wars_matrix)

non_us_all = mean(star_wars_matrix[,2]); non_us_all

non_us_some = mean(star_wars_matrix[1:2,2]); non_us_some

tickets_sold_us_1 = star_wars_matrix[1,1]*1000000/5 ; tickets_sold_us_1
ticket_sold_us_2 = star_wars_matrix[2,1]*1000000/5; ticket_sold_us_2
ticket_sold_us_3 = star_wars_matrix[3,1]*1000000/5; ticket_sold_us_3
tickets_sold_non_us_1 = star_wars_matrix[1,2]*1000000/5; tickets_sold_non_us_1
ticket_sold_non_us_2 = star_wars_matrix[2,2]*1000000/5; ticket_sold_non_us_2
ticket_sold_non_us_3 = star_wars_matrix[3,2]*1000000/5; ticket_sold_non_us_3

mean_visitors_us = mean(c(ticket_sold_us_2,tickets_sold_us_1,ticket_sold_us_3)); mean_visitors_us
mean_visitors_non_us = mean(c(ticket_sold_non_us_2,tickets_sold_non_us_1,ticket_sold_non_us_3)); mean_visitors_non_us

# Ejercicio 2.2
i <- array(c(1:10),dim=c(5,2))
dim(i)
nrow(i)
ncol(i)

# Ejercicio 2.3
arr<- array(c(1:5,5:1),dim=c(5,2))
arr

# Ejercicio 2.4
x;x[i]

# Ejercicio 2.5
x[i] = 0;x

# Ejercicio 2.6
arr_tab = read.delim('array_datos.txt',header = T)
str(arr_tab)
write.csv(arr_tab,'salida_datos.csv')

# Ejercicio 3.1
factor(c(1,2,3,3,5,2,4,NA))

# Ejercicio 3.2
x <- c(11, 22, 47, 47, 11, 47, 11)
factor(x,levels=c(11,22,47),ordered = T)
# Ejercicio 3.3
z <- c("p", "a" , "g", "t", "b")
z[3] <- "b"
z

# Ejercicio 3.4
z = factor(c("p","q","p","r","q")); z
levels(z)[levels(z)=='p'] = 'w';z

# Ejercicio 3.5
str(iris)
help("table"); help("cut")
table(cut(iris$Sepal.Length,breaks = 5))

# Ejercicio 3.6
str(iris)
cut(iris$Sepal.Length,breaks=c(min(iris$Sepal.Length)-0.1,5,max(iris$Sepal.Length)+0.1),right = FALSE)
table(cut(iris$Sepal.Length,breaks=c(min(iris$Sepal.Length)-0.1,5,max(iris$Sepal.Length)+0.1),right = FALSE))
menor = table(iris[iris$Sepal.Length < 5,"Species"])
mayor = table(iris[iris$Sepal.Length >=5, "Species"])
tabla = rbind(menor, mayor)
tabla

# Ejercicio 3.7
responses <- factor(c("Agree","Agree","Strongly Agree","Disagree","Agree"))
responses
levels(responses) = c("Strongly Agree", "Agree", "Disagree", "Strongly Disagree")
responses

# Ejercicio 3.8
x <- factor(c("high", "low", "medium", "high", "high", "low", "medium"))
x
unique(x)
niveles = 1:length(unique(x))
x_numerico = x; levels(x_numerico) = niveles; x_numerico

# Ejercicio 4.1
dim(USArrests)
nrow(USArrests)
ncol(USArrests)
colnames(USArrests)
rownames(USArrests)
USArrests[1:6,]
ord_usarrests = USArrests[order(USArrests[,"UrbanPop"],decreasing = T),]
ord_usarrests
USArrests[,"Murder"]
USArrests[1:5,]
USArrests[,1:2]
USArrests[,c(1,3)]
USArrests[1:5,1:2]
USArrests[,"Murder"]
USArrests[which.min(USArrests[,"Murder"]),]
which.min(USArrests[,"Murder"])
USArrests[which(USArrests[,"Murder"] < 4.0),]

# Ejercicios 4.2
students = as.data.frame(read.table("student.txt",header = TRUE),header=TRUE)
students
colnames(students)
students$height
students[,"height"]
sym = ifelse(students$gender == "male","M","F")
colours = ifelse(students$population == "kuopio","Blue","Red")
students.new = data.frame(students$height,students$shoesize,sym,colours); students.new
class(students.new)

which(students$gender=="male")
which(students$gender=="female")

students.male = students[which(students$gender=="male"),]; students.male
students.female = students[which(students$gender == "female"),]; students.female

write.table(students.new,"student_new.txt")

