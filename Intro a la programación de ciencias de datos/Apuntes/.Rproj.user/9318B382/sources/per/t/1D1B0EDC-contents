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
x1[,2] = x[order(x[,2],decreasing = T),2]
x1

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
