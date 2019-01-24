library(NoiseFiltersR)

data(iris)

set.seed(1)
out <- IPF(Species~., data = iris, s = 2)
summary(out, explicit = TRUE)
identical(out$cleanData, iris[setdiff(1:nrow(iris),out$remIdx),])
 
