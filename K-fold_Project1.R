library(class) ## a library with lots of classification tools
library(kknn) ## knn library


taxi <- read.csv("processed_taxi_fare.csv")

names(taxi)
summary(taxi)
attach(taxi)

#Linear regression shows that distance and fare have a very strong relationship

pairs(taxi[,c("fare_amount","weekend", "passenger_count" )])
pairs(taxi[,c("fare_amount","hour_of_the_day", "dist_km" )])

fit.lm <- lm(fare_amount ~ dist_km, data = taxi )
summary(fit.lm)

fit.lm <- lm(fare_amount ~ hour_of_the_day, data = taxi )
summary(fit.lm)

fit.lm <- lm(fare_amount ~ passenger_count, data = taxi )
summary(fit.lm)

fit.lm <- lm(fare_amount ~ weekend, data = taxi )
summary(fit.lm)

fit.lm <- lm(fare_amount ~ weekend + hour_of_the_day + passenger_count + 
               dist_km, data = taxi )
summary(fit.lm)

###############################################################
#K-fold cross validation with fare amount predicted by distance
###############################################################

train = data.frame(dist_km,fare_amount)
test = data.frame(dist_km,fare_amount)
n = dim(taxi)[1]

kcv = 10 #divide the data set by kcv and round it
n0 = round(n/kcv,0)

out_MSE = matrix(0,kcv,100) 

used = NULL
set = 1:n

for(j in 1:kcv){
  
  if(n0<length(set)){val = sample(set,n0)} #sample from set n0 number (100)
  if(n0>=length(set)){val=set} #for rounding errors
  
  train_i = train[-val,] #9 parts train
  test_i = test[val,]  #1 part test, each part tested against other 9
  
  for(i in 1:100){
    
    near = kknn(fare_amount~dist_km,train_i,test_i,k=i,kernel = "rectangular") #run k=i to 100 nearest neighbors
    aux = mean((test_i[,2]-near$fitted)^2) #calculate mean square error
    
    out_MSE[j,i] = aux #fill mean square error into matrix
  }
  
  used = union(used,val) #create used data sets
  set = (1:n)[-used] #remove these from the data set for next test
  
  cat(j,'\n')
  
}

mMSE = apply(out_MSE,2,mean)

#Plot the complexity as it relates to RMSE
plot(log(1/(1:100)),sqrt(mMSE)/10,xlab="Complexity (log(1/k))", 
     main="NYC Taxi Fares with Distance (kcv = 10)", ylab="out-of-sample RMSE",col=4,lwd=2,type="l",cex.lab=1.2)
best = which.min(mMSE)
text(log(1/best),sqrt(mMSE[best])/10+0.1,paste("k=",best),col=2,cex=1.2)
text(log(1/2),sqrt(mMSE[2])/10+0.3,paste("k=",2),col=2,cex=1.2)
text(log(1/100)+0.4,sqrt(mMSE[100])/10,paste("k=",100),col=2,cex=1.2)
print(sqrt(min(mMSE))/10)



#run the model with best k fit
near = kknn(fare_amount~dist_km,train,test,k=16,kernel = "rectangular") #run model

#sort the test data, plot the data, and over lay the model
ind = order(test[,1])
plot(dist_km,fare_amount,main=paste("k=",best),pch=19,cex=0.8,col="darkgray")
lines(test[ind,1],near$fitted[ind],col=2,lwd=2)

#################################################################
#K-fold cross validation with fare amount predicted by passengers
#################################################################

train = data.frame(passenger_count,fare_amount) #train and test set are identical
test = data.frame(passenger_count,fare_amount)
n = dim(taxi)[1]

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
    
    near = kknn(fare_amount~passenger_count,train_i,test_i,k=i,kernel = "rectangular") #run k=i to 100 nearest neighbors
    aux = mean((test_i[,2]-near$fitted)^2) #calculate mean square error
    
    out_MSE[j,i] = aux #fill mean square error into matrix
  }
  
  used = union(used,val) #create used data sets
  set = (1:n)[-used] #remove these from the data set for next test
  
  cat(j,'\n')
  
}

mMSE = apply(out_MSE,2,mean) #apply the mean to every column of the matrix


#Plot the complexity as it relates to RMSE
plot(log(1/(1:100)),sqrt(mMSE),xlab="Complexity (log(1/k))", main="NYC Taxi 
     Fares with Passenger Count (kcv = 10)", ylab="RMSE",col=4,lwd=2,type="l",cex.lab=1.2)
best = which.min(mMSE)
print(best)
text(log(1/best),sqrt(mMSE[best])+0.1,paste("k=",best),col=2,cex=1.2)
text(log(1/2),sqrt(mMSE[2])+0.3,paste("k=",2),col=2,cex=1.2)
text(log(1/100)+0.4,sqrt(mMSE[100]),paste("k=",100),col=2,cex=1.2)
print(sqrt(min(mMSE)))

#run the actual model with best k fit
near = kknn(fare_amount~passenger_count,train,test,k=100,kernel = "rectangular") #run model

#sort the test data, plot the data, and over lay the model
ind = order(test[,1])
plot(passenger_count,fare_amount,main=paste("k=",best),pch=19,cex=0.8,col="darkgray")
lines(test[ind,1],near$fitted[ind],col=2,lwd=2)

##################################################################
#K-fold cross validation with fare amount predicted by time-of-day
##################################################################

train = data.frame(hour_of_the_day,fare_amount) #train and test set are identical
test = data.frame(hour_of_the_day,fare_amount)
n = dim(taxi)[1]

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
    
    near = kknn(fare_amount~hour_of_the_day,train_i,test_i,k=i,kernel = "rectangular") #run k=i to 100 nearest neighbors
    aux = mean((test_i[,2]-near$fitted)^2) #calculate mean square error
    
    out_MSE[j,i] = aux #fill mean square error into matrix
  }
  
  used = union(used,val) #create used data sets
  set = (1:n)[-used] #remove these from the data set for next test
  
  cat(j,'\n')
  
}

mMSE = apply(out_MSE,2,mean) #apply the mean to every column of the matrix

#Plot the complexity as it relates to RMSE
plot(log(1/(1:100)),sqrt(mMSE),xlab="Complexity (log(1/k))", main="NYC Taxi 
     Fares with Hour of the Day (kcv = 10)", ylab="RMSE",col=4,lwd=2,type="l",cex.lab=1.2)
best = which.min(mMSE)
text(log(1/best),sqrt(mMSE[best])+0.1,paste("k=",best),col=2,cex=1.2)
text(log(1/2),sqrt(mMSE[2])+0.3,paste("k=",2),col=2,cex=1.2)
text(log(1/100)+0.4,sqrt(mMSE[100]),paste("k=",100),col=2,cex=1.2)

#run the actual model with best k fit
near = kknn(fare_amount~hour_of_the_day,train,test,k=100,kernel = "rectangular") #run model

#sort the test data, plot the data, and over lay the model
ind = order(test[,1])
plot(hour_of_the_day,fare_amount,main=paste("k=",best),pch=19,cex=0.8,col="darkgray")
lines(test[ind,1],near$fitted[ind],col=2,lwd=2)



#######################################################################
#K-fold cross validation with fare amt predicted by drop off location
#########################################################################

logFareAmt <- log(fare_amount)

n=dim(taxi)[1]

ind = sample(1:n,500)

Y = logFareAmt[ind]
Taxidata = taxi[ind,]


train = data.frame(Y,Taxidata$dropoff_latitude,Taxidata$dropoff_longitude)
test = data.frame(Y,Taxidata$dropoff_latitude,Taxidata$dropoff_longitude)


near = kknn(Y~.,train,test,k=10,kernel = "rectangular")

n = dim(Taxidata)[1]

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
plot(log(1/(1:100)),sqrt(mMSE),type="l",ylab="out-of-sample RMSE",col=4,lwd=2,
     main="NYC Taxi Fares with Drop-off (kfold (10))",xlab="Complexity")
best = which.min(mMSE)
text(log(1/best),sqrt(mMSE[best])+0.01,paste("k=",best))
text(log(1/100)+0.4,sqrt(mMSE[100]),"k=100")
text(log(1/1),sqrt(mMSE[1])+0.001,"k=1")


################################################################################
#K-fold cross validation with fare amt predicted by distance and num passengers
################################################################################

logFareAmt <- log(fare_amount)

n=dim(taxi)[1]

ind = sample(1:n,500)

Y = logFareAmt[ind]
Taxidata = taxi[ind,]


train = data.frame(Y,Taxidata$dist_km,Taxidata$passenger_count)
test = data.frame(Y,Taxidata$dist_km,Taxidata$passenger_count)


near = kknn(Y~.,train,test,k=10,kernel = "rectangular")

n = dim(Taxidata)[1]

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
plot(log(1/(1:100)),sqrt(mMSE),type="l",ylab="out-of-sample RMSE",col=4,lwd=2,main="NYC Taxi Fares 
     with Distance and Passengers (kfold (10))",xlab="Complexity")
best = which.min(mMSE)
text(log(1/best),sqrt(mMSE[best])+0.01,paste("k=",best))
text(log(1/100)+0.4,sqrt(mMSE[100]),"k=100")
text(log(1/1),sqrt(mMSE[1])+0.001,"k=1")
print(sqrt(min(mMSE)))



for(i in seq(1,505,by=100)){
  ii = near$C[i,1:20]
  plot(dist_km,fare_amount,main=paste("k=",20),pch=19,cex=0.8,col="darkgray")
  lines(test[,1],near$fitted,col=2,lwd=2)
  abline(v=test[i,1],col=2,lty=2)
  points(dist_km[ii],fare_amount[ii],pch=19,col="blue")
  cat ("Press [enter] to continue")
  line <- readline()
}

