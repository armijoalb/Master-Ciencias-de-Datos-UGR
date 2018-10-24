
class("Ciencia de Datos")

nchar("Ciencia de Datos")

paste("Ciencia", "de", "Datos")

paste("Ciencia", "de", "Datos", sep="_") 

paste("Ciencia", c("hola", "mundo"), "cierta")

help(paste)

paste(1:3, 1:5, sep="_", collapse="|") 

substr("Ciencia de Datos", 1, 5)

substr("Ciencia de Datos", 12, nchar("Ciencia de Datos"))

print(date())

strsplit(date(), split=" ")

strsplit(c("Esta es una frase", "Esta es otra linda frase"), split=" ")

strsplit(c("Esta es una frase", "Esta es otra frase"), split=c(" ", "e"))

strsplit(date(), split="[0-9]+")

# For example, the expression “[ia]” refers to any string that contains either of the letters i or a
grep("[ia]", c("Ciencia","de","Datos"))

# A period (.) represents any single character
grep(".e", c("Ciencia","de","Datos"))

# Another example using strsplit()
strsplit("a.b.c", ".")

strsplit("a.b.c", "[.]")

strsplit("a.b.c", "\\.")

ml <- list(uno=c(1:3, 7), dos="hola", tres=list())
print(ml)

unlist(ml)
class(unlist(ml))

unlist(ml, use.names = FALSE)

ml <- strsplit(c("Esta es una frase", "Esta es otra linda frase"), split=" ")
print(ml)

unlist(ml)

ml <- list(uno=c(1:3, 7), dos=c(4,-9,5.2), tres=seq(1,20,3))
print(ml)

unlist(ml, use.names = FALSE)
class(unlist(ml, use.names = FALSE))

scan("file.txt")

scan("file.txt",what=character())

scan("file.txt",sep="\n")

x <- 1:3
print(x^2)

cat(x^2)

cat(x^2, x, "hola")

cat(matrix(c(1,5,3,8), nrow = 2), fill=1)

help(cat)

cat(x^2, x, "hola", sep="_")

read.table("matrix.txt", header=TRUE)
class(read.table("matrix.txt", header=TRUE))

write.table(matrix(1:6, nrow=2), "output.txt", row.names=TRUE, col.names=TRUE)

cat("abc\n",file="u.txt")
cat("de\n",file="u.txt", append=TRUE)

help(read.table)

suma <- function(x, y) {
    x + y
}

suma(2, 3)

# Default values
suma <- function(x, y=5) {
    x + y
}

suma(2)

suma <- function(x=5, y) {
    x + y
}

suma(y=2)

x <- 10
if(x == 2) y <- x else y <- x+1
y

y <- if(x == 2) x else x+1
y

x <- 1:10
y <- if(x == 2) x else x+1

x <- 1:10
ifelse(x %% 2 == 0, "par", "impar")

x <- c(5,2,9,12)
ifelse(x > 6, 2*x, 3*x)

k <- 0
for(n in x) {
   if (n %% 2 == 1) k <- k+1  
} 
k

k <- 0
for (i in 1:length(x)) {
   if (x[i] %% 2 == 1) k <- k+1
} 
k

for (fn in c("file1.txt","file2.txt")) {
    print(scan(fn))
}

i <- 1
while (i <= 10) {
    i <- i+4
}
i

i <- 1
repeat {
    i <- i+4
    if (i > 10) break
}
i

z <- matrix(c(1,1,2,2,3,3), nrow=3, byrow=TRUE)
apply(z, 1, mean)

apply(z, 2, mean)

apply(z, 1:2, mean)

z <- list(vector=c(1,2,3), matriz=matrix(1,nrow=2,ncol=2))
lapply(z,mean)

sapply(z, mean)

x <- runif(1000000)
y <- runif(1000000)
z <- vector(length=1000000)
system.time(for (i in 1:length(x)) z[i] <- x[i] + y[i])

system.time(z <- x + y)

xs <- runif(1000)
res <- c()
for (x in xs) {
  # This is slow!
  res <- c(res, sqrt(x))
}

xs <- runif(1000)
res <- numeric(length(xs))
for (i in seq_along(xs)) {
  res[i] <- sqrt(xs[i])
}

amat <- matrix(1:20, nrow=4)
bmat <- matrix(NA, nrow(amat)/2, ncol(amat))
for(i in 1:nrow(bmat))
    bmat[i,] <- amat[2*i-1,] * amat[2*i,] 

amat <- matrix(1:20, nrow=4)
bmat2 <- amat[seq(1, nrow(amat), by=2),] * amat[seq(2, nrow(amat), by=2),]

the.seq <- 1:4
which(outer(outer(the.seq, the.seq, '+'), the.seq, '+') == 6, arr.ind=TRUE)
