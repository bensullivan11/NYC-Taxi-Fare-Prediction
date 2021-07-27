###################################################
## Random Forest
###################################################

rm(list=ls())
library(randomForest)
set.seed(1)
taxi <- read.csv("taxi 2.csv")
taxi <- subset(taxi,select=c(2,3,4,5,6,7,8,9))
train = sample (1:nrow(taxi), nrow(taxi)/2)
bag.taxi = randomForest(fare_amount~.,data=taxi,mtry=5,importance = TRUE,subset=train)
bag.taxi
taxi.test=taxi[-train ,"fare_amount"]
yhat.bag = predict(bag.taxi, newdata=taxi[-train ,])
plot(yhat.bag,taxi.test)
abline (0,1)
mean((yhat.bag-taxi.test)^2)

### MSE 12.71582



###################################################
## Boosting Model with first 200
###################################################
rm(list=ls())
library(ISLR)
library(gbm)
set.seed(1)
taxi <- read.csv("taxi 2.csv")
taxi <- data.frame(fare_amount= taxi$fare_amount,pickup_longitude=taxi$pickup_longitude,
                   pickup_latitude = taxi$pickup_latitude,
                   dropoff_longitude=taxi$dropoff_longitude,
                   dropoff_latitude=taxi$dropoff_latitude,
                   passenger_count = taxi$passenger_count,
                   weekend= taxi$weekend,hour_of_the_day= taxi$hour_of_the_day)
train = taxi[1:200,]
taxi.test = taxi[201:nrow(taxi),]
boost.taxi = gbm(fare_amount~.,data=train,distribution = "gaussian",n.trees=5000,
                 interaction.depth =4 )
summary(boost.taxi)
yhat.boost=predict(boost.taxi,newdata=taxi.test,
                    n.trees=5000)
mean((yhat.boost - taxi.test)^2)


###################################################
## Boosting Model with half training 
###################################################
rm(list=ls())
library(ISLR)
library(gbm)
set.seed(1)
taxi <- read.csv("taxi 2.csv")
taxi <- subset(taxi,select=c(2,3,4,5,6,7,8,9))
train = sample (1:nrow(taxi), nrow(taxi)/2)
taxi.test=taxi[-train ,"fare_amount"]
boost.taxi = gbm(fare_amount~.,data=taxi[train ,],distribution = "gaussian",n.trees=5000,
                 interaction.depth =4 )
summary(boost.taxi)
yhat.boost=predict(boost.taxi,newdata=taxi[-train ,],
                   n.trees=5000)
mean((yhat.boost - taxi.test)^2)

### MSE 12.85598
###################################################
## Multiple Regression
###################################################
rm(list=ls())
set.seed(1)
taxi <- read.csv("taxi 2.csv")
taxi <- subset(taxi,select=c(2,3,4,5,6,7,8,9))
linearmodel <- lm(fare_amount~.,data=taxi)
summary(linearmodel)

###################################################
## K Nearest Neighbors
###################################################
rm(list=ls())
library(ggmaps)
library(kknn)
set.seed(1)
taxi <- read.csv("taxi 2.csv")
taxi <- subset(taxi,select=c(2,3,4,5,6,7,8,9))

logfareamount <- log(taxi$fare_amount)

n=dim(taxi)[1]

ind = sample(1:n,1000)

Y = logfareamount[ind]
taxidata = taxi[ind,]


train = data.frame(Y,taxidata$dropoff_longitude,taxidata$dropoff_latitude)
test = data.frame(Y,taxidata$dropoff_longitude,taxidata$dropoff_latitude)


near = kknn(Y~.,train,test,k=10,kernel = "rectangular")


res = Y - near$fitted
nclr = 10
plotclr = colorRampPalette(c("cyan","magenta"))(nclr)
predcol = heat.colors(9)[9:1] ## see help(heat.colors)
predbreaks = seq(min(Y),max(Y),length=nclr)
residbreaks = seq(min(res),max(res),length=nclr) # borders of resid color bins
residmap <- function(e){
  return(plotclr[cut(drop(e), residbreaks)]) ## cut sorts into bins
}
predmap <- function(y){
  return(predcol[cut(drop(y),predbreaks)]) ## cut sorts into bins
}


par(mfrow=c(1,2))
## preds
#mymap <- get_map(location = "New York", maptype = "roadmap")
map('state', 'new york')
mtext("fitted values (k=10)",cex=2) 
points(test[,3:2], col=predmap(near$fitted), pch=19, cex=1)
#mymap <- get_map(location = "New York", maptype = "roadmap")
map('state', 'new york')
mtext("Residuals (k=10)",cex=2) 
points(test[,3:2], col=residmap(res), pch=19, cex=1)

n = dim(taxidata)[1]

kcv = 10
n0 = round(n/kcv,0)

out_MSE = matrix(0,kcv,100)

used = NULL
set = 1:n

for(j in 1:kcv){
  
  if(n0<length(set)){val = sample(set,n0)}
  if(n0>=length(set)){val=set}
  
  train_i = train[-val,]
  test_i = test[val,]
  
  for(i in 1:100){
    
    near = kknn(Y~.,train_i,test_i,k=i,kernel = "rectangular")
    aux = mean((test_i[,1]-near$fitted)^2)
    
    out_MSE[j,i] = aux
  }
  
  used = union(used,val)
  set = (1:n)[-used]
  
  cat(j,'\n')
}





mMSE = apply(out_MSE,2,mean)
par(mfrow=c(1,1))
plot(log(1/(1:100)),sqrt(mMSE),type="l",ylab="out-of-sample RMSE",col=4,lwd=2,main="New York Taxi (knn)",xlab="Complexity")
best = which.min(mMSE)
text(log(1/best),sqrt(mMSE[best])+0.01,paste("k=",best))
text(log(1/100)+0.4,sqrt(mMSE[100]),"k=100")
text(log(1/1),sqrt(mMSE[1])+0.001,"k=1")




Fare = scale(taxidata$fare_amount)*sd(taxidata$dropoff_longitude)

train = data.frame(Y,taxidata$dropoff_longitude,taxidata$dropoff_latitude,Fare)
test = data.frame(Y,taxidata$dropoff_longitude,taxidata$dropoff_latitude,Fare)


near = kknn(Y~.,train,test,k=best,kernel = "rectangular")


res = Y - near$fitted
nclr = 10
plotclr = colorRampPalette(c("cyan","magenta"))(nclr)
predcol = heat.colors(9)[9:1] ## see help(heat.colors)
predbreaks = seq(min(Y),max(Y),length=nclr)
residbreaks = seq(min(res),max(res),length=nclr) # borders of resid color bins
residmap <- function(e){
  return(plotclr[cut(drop(e), residbreaks)]) ## cut sorts into bins
}
predmap <- function(y){
  return(predcol[cut(drop(y),predbreaks)]) ## cut sorts into bins
}


par(mfrow=c(1,2))
## preds
map('state', 'new york')
mtext("fitted values (k=9)",cex=2) 
points(test[,3:2], col=predmap(near$fitted), pch=19, cex=1)
map('state', 'new york')
mtext("Residuals (k=9)",cex=2) 
points(test[,3:2], col=residmap(res), pch=19, cex=1)

names(taxi)

