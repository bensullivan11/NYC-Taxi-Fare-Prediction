### Regular Tree 
rm(list=ls())
library(tree)
set.seed(4)
taxi <- read.csv("taxi 2.csv")
taxi=taxi[,c(2,7,8,9,10)]
train = sample (1:nrow(taxi), nrow(taxi)/2)
tree.taxi=tree(fare_amount ~.,data=taxi,subset=train)
summary(tree.taxi)
plot(tree.taxi)
text(tree.taxi,pretty=0)
yhat=predict(tree.taxi,newdata=taxi[-train,])
taxi.test=taxi[-train,"fare_amount"]
mean((taxi.test-yhat)^2)

### MSE 7.567747

#### Use cross validation to prune the tree ###
set.seed(4)
cv.taxi=cv.tree(tree.taxi)
plot(cv.taxi$size ,cv.taxi$dev,type="b")
## best from plot is 2 
prune.taxi=prune.tree(tree.taxi,best = 2)
plot(prune.taxi)
abline(0,1)
text(prune.taxi,pretty=0)
yhat=predict(prune.taxi,pruneData=taxi[-train,"fare_amount"])
mean((yhat-taxi.test)^2)

### MSE 97.11734