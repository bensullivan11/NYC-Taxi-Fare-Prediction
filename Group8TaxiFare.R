graphics.off()
rm(list=ls())

library(MASS) ## a library of example datasets
library(class) ## a library with lots of classification tools
library(kknn) ## knn library

NYtaxi <- read.csv("NYtaxi.csv")
## loggRating <- log(ar$gRating)


attach(NYtaxi)
n = dim(NYtaxi)[1]


##########################
#### Taxi Fare knn     ###
##########################


plot(dist_km,fare_amount)

train = data.frame(dist_km,fare_amount)
test = data.frame(dist_km,fare_amount)
ind = order(test[,1])
test =test[ind,]

MSE = NULL

kk = c(2,10,50,100,150,200,250,300,400,505)

for(i in kk){
  
  near = kknn(fare_amount~dist_km,train,test,k=i,kernel = "rectangular")
  aux = mean((test[,2]-near$fitted)^2)
  
  MSE = c(MSE,aux)
  
  plot(dist_km,fare_amount,main=paste("k=",i),pch=19,cex=0.8,col="darkgray")
  lines(test[,1],near$fitted,col=2,lwd=2)
  cat ("Press [enter] to continue")
  line <- readline()
}


plot(log(1/kk),sqrt(MSE),type="b",xlab="Complexity (log(1/k))",col="blue",ylab="RMSE",lwd=2,cex.lab=1.2)
text(log(1/kk[1]),sqrt(MSE[1])+0.3,paste("k=",kk[1]),col=2,cex=1.2)
text(log(1/kk[10])+0.4,sqrt(MSE[10]),paste("k=",kk[10]),col=2,cex=1.2)
text(log(1/kk[5])+0.4,sqrt(MSE[5]),paste("k=",kk[5]),col=2,cex=1.2)

near = kknn(fare_amount~dist_km,train,test,k=20,kernel = "rectangular")

for(i in seq(1,505,by=100)){
  ii = near$C[i,1:20] ## hi
  plot(dist_km,fare_amount,main=paste("k=",20),pch=19,cex=0.8,col="darkgray")
  lines(test[,1],near$fitted,col=2,lwd=2)
  abline(v=test[i,1],col=2,lty=2)
  points(dist_km[ii],fare_amount[ii],pch=19,col="blue")
  cat ("Press [enter] to continue")
  line <- readline()
}

######################################
## OUT-OF-SAMPLE Prediction
######################################

train = data.frame(dist_km,fare_amount)
test = data.frame(dist_km,fare_amount)

tr = sample(1:506,400)

train = train[tr,]
test = test[-tr,]

out_MSE = NULL

for(i in 2:350){
  
  near = kknn(fare_amount~dist_km,train,test,k=i,kernel = "rectangular")
  aux = mean((test[,2]-near$fitted)^2)
  
  out_MSE = c(out_MSE,aux)
}


best = which.min(out_MSE)

plot(log(1/(2:350)),sqrt(out_MSE),xlab="Complexity (log(1/k))",ylab="out-of-sample RMSE",col=4,lwd=2,type="l",cex.lab=1.2)
text(log(1/best),sqrt(out_MSE[best])+0.3,paste("k=",best),col=2,cex=1.2)
text(log(1/2),sqrt(out_MSE[2]),paste("k=",2),col=2,cex=1.2)
text(log(1/354)+0.4,sqrt(out_MSE[345]),paste("k=",345),col=2,cex=1.2)


near = kknn(fare_amount~dist_km,train,test,k=42,kernel = "rectangular")

ind = order(test[,1])
plot(dist_km,fare_amount,main=paste("k=",42),pch=19,cex=0.8,col="darkgray")
lines(test[ind,1],near$fitted[ind],col=2,lwd=2)

#########################################
# leave-one-out cross validation (LOOCV)#
#########################################

train = data.frame(dist_km,fare_amount)
test = data.frame(dist_km,fare_amount)


out_MSE = matrix(0,n,100)

for(j in 1:n){
  
  
  train_i = train[-j,]
  test_i = test[j,]
  
  for(i in 1:100){
    
    near = kknn(fare_amount~dist_km,train_i,test_i,k=i,kernel = "rectangular")
    aux = mean((test_i[,2]-near$fitted)^2)
    
    out_MSE[j,i] = aux
  }
  cat(j,'\n')
}

mMSE = apply(out_MSE,2,mean)

plot(log(1/(1:100)),sqrt(mMSE),xlab="Complexity (log(1/k))",ylab="out-of-sample RMSE",col=4,lwd=2,type="l",cex.lab=1.2)
best = which.min(mMSE)
text(log(1/best),sqrt(mMSE[best])+0.1,paste("k=",best),col=2,cex=1.2)
text(log(1/2),sqrt(mMSE[2])+0.3,paste("k=",2),col=2,cex=1.2)
text(log(1/100)+0.4,sqrt(mMSE[100]),paste("k=",100),col=2,cex=1.2)


