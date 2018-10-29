# Apuntes funciones en R.
suma = function(x,y){
  x+y
}
suma(2,3)

# Ejercicio 1
imp_v = 1:10
impares = function(x){
  length(which(x%%2!=0))
}

impares(imp_v)

# Ejercicio 2
nas = c(1,2,NA,4,NA)
which(is.na(nas))
cambio = function(x){
  x[which(is.na(x))] = 0
  x
}

nas = cambio(nas); nas

# Ejercicio 3
vec1 = 1:5
vec2 = 3:7
unir = function(x,y){
  union(x,y)
}

new_vec = unir(vec1,vec2); new_vec

# Ejercicio 4
vyc = function(mi_string){
  separada = unlist(strsplit(mi_string,""))
  vocales=grep("[aeiou]",separada)
  all = grep("[a-z]",separada)
  consonantes=which(!(all %in% vocales) )
  list(vocales=separada[vocales],consonantes=separada[consonantes])
}

vyc("hola")


# Ejercicio 5
partir = function(v,x,y=length(v)){
  pos_x = which(v==x)[1]
  pos_y = which(v==y)[1]
  new_v = v[pos_x:y]
  new_v
}

vect = c(1,2,3,5)
partir(vect,2)
partir(vect,1,3)
