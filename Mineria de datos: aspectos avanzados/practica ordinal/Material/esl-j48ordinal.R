library(RWeka)

set.seed (2)
train=sample (1:nrow(iris), 100)
iris.test=iris [-train ,]


modelC4.5 = J48(Species~., data=iris, subset=train)

library(partykit)
plot(modelC4.5)
modelC4.5.pred = predict(modelC4.5, iris.test,"probability")

modelC4.5.pred