########################################################################################
### Group 8 NYC Taxi Fare Prediction 
#########################################################################################
rm(list=ls())
library(maps)
library(tree)
library(randomForest)
library(tidyverse)

library(ggplot2)
library(reshape2)
# install.packages("readxl")

library('lubridate')#

library(geosphere)
library(dplyr)

library(plotly)




set.seed(5)         

setwd('D:/stat_wk')
dff = read.csv("taxi_fares.csv")
dff_org= read.csv("taxi_fares.csv")
# dff=read.table("taxi_fares.xlsx",header=TRUE)


#EDA

dff$pickup_datetime=as_datetime(dff$pickup_datetime)
dff_org$pickup_datetime=as_datetime(dff_org$pickup_datetime)

summary(dff)



## Observing location, time and count based plots 
########################################################################################
## Slide 4
#########################################################################################
plot(dff$pickup_longitude,dff$fare_amount,col=as.factor(dff$passenger_count))
plot(dff$pickup_latitude,dff$fare_amount,col=as.factor(dff$passenger_count))

########################################################################################
## Slide 5
#########################################################################################

plot(dff$pickup_datetime,dff$fare_amount,col=as.factor(dff$passenger_count))

########################################################################################
## Slide 6
#########################################################################################
plot(dff$passenger_count,dff$fare_amount,col=as.factor(dff$passenger_count))

par(mfrow=c(1,3))
boxplot(dff$pickup_datetime,xlab='datetime')
boxplot(dff$pickup_longitude,xlab='pickup_long')
boxplot(dff$pickup_latitude,xlab='pickup_lat')



##BASELINE MODEL
########################################################################################
## Slide 8 BASELINE MODEL
#########################################################################################

summary(lm(fare_amount~.,dff))

n_row=nrow(dff)
test=sample(n_row,200)

temp_mlr=lm(fare_amount~.,dff[-test,])
temp_pre=predict(temp_mlr,dff[test,])
cat(mean((dff[test,c('fare_amount')]-temp_pre)^2))

#The adjusted R square of the model based on the raw data was quite low, we realized that the model is not able to explain the variable properly
#The error was too high 5.9 for a mean score of fare_amount of ~9.8

#We had datetime and location centric data so we decided to start out with creating distance and time based basic features
#Linear to non linear parameters (combination of predictors)

##Feature Creation
########################################################################################
## Slide 9 Feature Creation
#########################################################################################

dff$day_of_the_week=as.factor(weekdays(as.Date(dff$pickup_datetime)))
dff$weekend=ifelse(dff$day_of_the_week %in% c('Saturday','Sunday'),1,0)
dff=dff[,-8]# - ('day_of_the_week')]
dff$hour_of_the_day=as.integer(hour(dff$pickup_datetime))

dff$dist_km=distHaversine(cbind(dff$pickup_longitude,dff$pickup_latitude ), cbind(dff$dropoff_longitude,dff$dropoff_latitude ))/1000


##Exploratory DATA Analysis
summary(dff)
#Zoomed in quantile values
# as.data.frame(quantile(df$dare_amount,probs=c(0.25,0.5,seq(0.75,1,0.025))))
var(dff$pickup_latitude)# Quite low 
var(dff$dist_km)#Appreciable spreaded over wide range of values, can help to build better predictive models

hist(dff$dist_km)
hist(log(dff$dist_km))#Seems to be more normaly distributed can be equipped in our model

plot(dff$dist_km,dff$fare_amount,ylab='fare_amount')

#Unique instances of each column
n_u=c()
v=c()
m=c()
for (i in names(dff)){
  n_u=c(n_u,length(unique(dff[,i])))
  v=c(v,sqrt(var(dff[,i])))
  m=c(m,mean(dff[,i]))
}

print(data.frame(col_names=names(dff),n_unique=n_u,std_dev=v,mean=m))

#plotted combinations of pairplots to obtain a comprehensive view
pairs(dff[-c(1)])

num_dff=dff[-c(1)]

## Plot against gRatings
par(mfrow=c(3,3))
for (i in names(num_dff)){
  if (i != "fare_amount"){
    plot(num_dff$fare_amount,num_dff[,i],main=paste('col=',i),xlab='fareAmount',ylab=i)
  }}

########################################################################################
## Slide 10 Heatmap
#########################################################################################
#Generated a heatmap based correlation plot between columns to get an idea of which ones are associated with one another and to what extent
heatmap(cor(num_dff,method='pearson'),)
#Trying to analyse which pair of columns has high degree of correlation in absolute terms
heatmap((abs((cor(num_dff,method='pearson')))>0.6)*1)

df_cor=cor(num_dff,method='pearson')


melted_cormat = melt(df_cor)
head(melted_cormat)


# ggplot(data = melted_cormat, aes(x=Var1, y=Var2, fill=value)) 


ggheatmap = ggplot(melted_cormat, aes(Var2, Var1, fill = value))+
  geom_tile(color = "white")+
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", 
                       midpoint = 0, limit = c(-1,1), space = "Lab", 
                       name="Pearson\nCorrelation") +
  theme_minimal()+ # minimal theme
  theme(axis.text.x = element_text(angle = 45, vjust = 1, 
                                   size = 12, hjust = 1))+
  coord_fixed()

ggheatmap + 
  geom_text(aes(Var2, Var1, label = round(value,2)), color = "black", size = 4) +
  theme(
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    panel.grid.major = element_blank(),
    panel.border = element_blank(),
    panel.background = element_blank(),
    axis.ticks = element_blank(),
 )+
  guides(fill = guide_colorbar(barwidth = 7, barheight = 1,
                               title.position = "top", title.hjust = 0.5))



### Individual predictor analysis with greviews
coeff_pre=c()
pval_pre=c()

num_dff=dff

for (pre in names(num_dff)){
  if (pre =='fare_amount')
  {next}
  cat(pre,'\n')
  
  temp_pre=num_dff[,pre]
  model_pre=lm(fare_amount~temp_pre,data=num_dff)
  
  sum_pre=summary(model_pre)
  
  coeff_pre=c(coeff_pre,model_pre$coefficients['temp_pre'])
  pval_pre=c(pval_pre,sum_pre$coefficients[2,4])
  
}

df_pre=data.frame(predictors=names(num_dff)[-2],coeff=coeff_pre,p_vals=pval_pre)

#sorting wrt absolute values of coefficients
df_pre=df_pre[order(abs(df_pre$coeff),decreasing=TRUE),]


dev.new(width=5, height=4)
plot(dff$fare_amount,dff$hour_of_the_day)
dev.off()


########################################################################################
## Slide 12 Multiple Linear Regression
#########################################################################################

## Multiple Linear Regression
model_all=lm(fare_amount~.,data=num_dff)

summary(model_all)
model_pre_all=coefficients(model_all)



###Combinations of 3 predictors 
n_row=nrow(num_dff)
test=sample(n_row,200)
names(num_dff)

pre_lt=names(num_dff)[-2]
temp=combn(1:length(pre_lt),3,simplify=FALSE)


test_mse=c()
names_col=c()
for (i in temp){
  # print(num_dff[-test,c(i)])
  
  names_col=rbind(names_col,pre_lt[i])
  cat(pre_lt[c(i)],'\n')
  
  temp_df=data.frame(num_dff[,c(pre_lt[c(i)],'fare_amount')])
  train_df=temp_df[-test,]
  test_df=temp_df[test,]
  
  temp_model=lm(fare_amount~ .,data=train_df)
  pred=predict(temp_model,test_df)
  
  err=mean((num_dff[test,c('fare_amount')]-pred)^2)
  test_mse=c(test_mse,err)
  
  
}

err_df_3=data.frame(col1=names_col[,1],col2=names_col[,2],col3=names_col[,3],err=test_mse)
err_df_3=err_df_3[order(err_df_3$err),]

temp_mlr=lm(fare_amount~.,dff_org[-test,])
temp_pre=predict(temp_mlr,dff_org[test,])
cat(mean((dff_org[test,c('fare_amount')]-temp_pre)^2))

####
temp_df=num_dff
train_df=temp_df[-test,]
test_df=temp_df[test,]

temp_model=lm(fare_amount~ .,data=train_df)
pred=predict(temp_model,test_df)

err=mean((num_dff[test,c('fare_amount')]-pred)^2)



priority_ord=c('hour_of_the_day','passenger_count','dist_km')
# priority_ord=c('dist_km','hour_of_the_day','passenger_count')


tbu=c()

err=matrix(-1,1000,3)

r=0
for (i in priority_ord){
  tbu=c(tbu,i)
  temp=dff[,c(tbu,'fare_amount')]
  
  
  cat(names(temp),'\n')
  r=r+1
  for (j in 1:1000){
    
    # test=sample(nrow(dff),order((nrow(dff)/5),0))
    test=sample(1:1000,100)
    
    df_train=temp[-test,]
    df_test=temp[test,]
    
    mod=lm(fare_amount~.,df_train)
    
    pre=predict(mod,df_test)
    
    err[j,r]=mean((df_test[,'fare_amount']-pre)^2)
  } 
}




# dff_err=data.frame(err)
# names(dff_err)=c('Dist','Dist_passenger','Dist_passenger_hour')
# par(mfrow=c(1,3))
# boxplot(dff_err$Dist,xlab='pred-Dist')
# boxplot(dff_err$Dist_passenger,xlab='pred-Dist-PassengerNumber')
# boxplot(dff_err$Dist_passenger_hour,xlab='pred-Dist-PassengerNumber-Hour')

########################################################################################
## Slide 14 Repeated Out of Sample
#########################################################################################

dff_err=data.frame(err)
names(dff_err)=c('hour','hour_passenger','hour_passenger_dist')
par(mfrow=c(1,3))
boxplot(dff_err$hour,xlab='pred-Hour')
boxplot(dff_err$hour_passenger,xlab='pred-Hour-PassengerNumber')
boxplot(dff_err$hour_passenger_dist,xlab='pred-Hour-PassengerNumber-Dist')


summary(dff_err)
#Subset Selection
#install.packages('leaps')
library(leaps)


model_sub=regsubsets(fare_amount~.,data=num_dff,nvmax=9)
s=summary(model_sub)


par(mfrow=c(2,2))
plot(s$rss ,xlab="Number of Variables ",ylab="RSS",
     type="l")
plot(s$adjr2 ,xlab="Number of Variables ",
     ylab="Adjusted RSq",type="l")



head(dff)
write_csv(dff,'processed_taxi_fare.csv')
plot(dff$dist_km,dff$fare_amount,col=as.factor(dff$passenger_count))
plot(dff$hour_of_the_day,dff$fare_amount,col=as.factor(dff$passenger_count))


# plot_ly(dff,x=~dist_km,z=~fare_amount,y=~passenger_count)



########################################################################################
## Slide 13 Best Subset Selection
#########################################################################################
############ Best Subset Selection ############ 
library(olsrr)

taxifare = read.csv('processed_taxi_fare.csv')

model <- lm(fare_amount ~ dist_km + passenger_count + weekend + 
              hour_of_the_day, data = taxifare)

#best subset regression
ols_step_best_subset(model)
#dist_km, passenger_count, hour_of_the_day have highest R-square but dist_km & passenger_count have lower AIC



########################################################################################
## Slide 16 Simple Tree
#########################################################################################

## Simple Tree
rm(list=ls())
library(tree)

dff = read.csv('processed_taxi_fare.csv')

set.seed(1)
taxi_idx = sample(1:nrow(dff), nrow(dff) / 2)
taxi_trn = dff[taxi_idx,]
taxi_tst = dff[-taxi_idx,]

#fit unpruned regression tree to the training data
taxi_tree = tree(fare_amount ~., data = dff)
summary(taxi_tree)
#number of terminal nodes = 6

#plot unpruned tree
plot(taxi_tree)
text(taxi_tree, pretty = 0)
title(main = "Unpruned Regression Tree")

#now use cross-validation to select a good pruning of the tree.
#cv.tree() func helps us see if pruning improves performance
set.seed(1)
taxi_tree_cv = cv.tree(taxi_tree)
plot(taxi_tree_cv$size, sqrt(taxi_tree_cv$dev / nrow(taxi_trn)), type = "b",
     xlab = "Tree Size", ylab = "CV-RMSE")

#this verifies that it is already as small as it can be
#RMSE shows 6 is the right number of leaves


taxi.tree=prune.tree(taxi_tree,best=6)

summary(taxi.tree)

#--------------------------------------------------

#plot the tree and the fits.
par(mfrow=c(1,2))

#plot the tree
plot(taxi.tree,type="uniform")
text(taxi.tree,col="blue",label=c("yval"),cex=.8)

#plot data with fit
taxi.fit = predict(taxi.tree) #get training fitted values

plot(dff$dist_km,dff$fare_amount,cex=.5,pch=16) #plot data
oo=order(dff$dist_km)
lines(dff$dist_km[oo],taxi.fit[oo],col='red',lwd=3) #step function fit

cvals=c(7.47638,3.17677,1.18651,4.71284,17.0299)#cutpoints from tree
for(i in 1:length(cvals)) abline(v=cvals[i],col='magenta',lty=2) #cutpoints


########################################################################################
## Slide 17 Random Forest
#########################################################################################

## Random Forest
rm(list=ls())
library(tree)
library(randomForest)
set.seed(99)
taxi <- read.csv("processed_taxi_fare.csv")
n=nrow(taxi)
n1=floor(n/2)
n2=floor(n/4)
n3=n-n1-n2
ii = sample(1:n,n)
taxitrain=taxi[ii[1:n1],]
taxival = taxi[ii[n1+1:n2],]
taxitest = taxi[ii[n1+n2+1:n3],]
p=ncol(taxitrain)-1
mtryv = c(p,sqrt(p))
ntreev = c(100,500,1000)
parmrf = expand.grid(mtryv,ntreev)
colnames(parmrf)=c('mtry','ntree')
nset = nrow(parmrf)
olrf = rep(0,nset)
ilrf = rep(0,nset)
rffitv = vector('list',nset)
for(i in 1:nset) {
  cat('doing rf ',i,' out of ',nset,'\n')
  temprf = randomForest(fare_amount~.,data=taxitrain,mtry=parmrf[i,1],ntree=parmrf[i,2])
  ifit = predict(temprf)
  ofit=predict(temprf,newdata=taxival)
  olrf[i] = sum((taxival$fare_amount-ofit)^2)
  ilrf[i] = sum((taxitrain$fare_amount-ifit)^2)
  rffitv[[i]]=temprf
}

ilrf = round(sqrt(ilrf/nrow(taxitrain)),3); olrf = round(sqrt(olrf/nrow(taxival)),3)
varImpPlot(ilrf)

#----------------------------------------
#print losses
print(cbind(parmrf,olrf,ilrf))
plot(temprf,main="Error vs Tree Size")
varImpPlot(temprf)
#----------------------------------------
#write val preds
iirf=which.min(olrf)
therf = rffitv[[iirf]]
therfpred=predict(therf,newdata=taxival)
write(therfpred,file='therfpred.txt',ncol=1)


########################################################################################
## Slide 18 Ridge Regression and Lasso
#########################################################################################

## Ridge Regression and Lasso
rm(list=ls())

dff = read.csv('processed_taxi_fare.csv')

library(ISLR)
library(glmnet)
library(leaps)

summary(dff)

set.seed(11)
train <- sample(1:dim(dff)[1], dim(dff)[1]/2, rep=FALSE)
test <- -train

dff_train<- dff[train, ]
dff_test <- dff[test,]

dim(dff) #1000 10

dim(dff_train) #500 10
dim(dff_test) #500 10

#PART B: Fit a linear model using least squares on the training set, and 
#report the test error obtained.

lm_fit <- lm(fare_amount ~ ., data = dff)
lm_pred <- predict(lm_fit, dff_test)
mean((lm_pred - dff$fare_amount)^2) #120.7736 is the test MSE

#PART C: Fit a ridge regression model on the training set, with λ chosen 
#by cross-validation. Report the test error obtained.

#test and train matrix
train_mat <- model.matrix(fare_amount ~ ., data = dff)
test_mat <- model.matrix(fare_amount ~ ., data = dff)

#set the grid
grid <- 10 ^ seq(4, -2, length = 100)

#Fit the ridge regression model on training dataset
fit.ridge <- glmnet(train_mat, dff$fare_amount, alpha = 0, lambda = grid, thresh = 1e-12)
cv.ridge <- cv.glmnet(train_mat, dff$fare_amount, alpha = 0, lambda = grid, thresh = 1e-12)
bestlam.ridge <- cv.ridge$lambda.min
bestlam.ridge #0.04641589

#Get the test error rate
pred.ridge <- predict(fit.ridge, s = bestlam.ridge, newx = test_mat)
mean((pred.ridge - dff$fare_amount)^2) #0.001332124 ridge regression


#PART D: Fit a lasso model on the training set, with λ chosen by 
#cross-validation. Report the test error obtained, along with the 
#number of non-zero coefficient estimates.

fit_lasso <- glmnet(train_mat, dff$fare_amount, alpha = 1, lambda = grid, thresh = 1e-12)
cv_lasso <- cv.glmnet(train_mat,dff$fare_amount, alpha = 1, lambda = grid, thresh = 1e-12)
bestlam.lasso <- cv_lasso$lambda.min
bestlam.lasso #0.1417474

pred.lasso <- predict(fit_lasso, s = bestlam.lasso, newx = test_mat)
mean((pred.lasso - dff$fare_amount)^2) #3.139795 test mse for lasso

#Get all the coefficients 
predict(fit_lasso, s = bestlam.lasso, type = "coefficients")



#LASSO model

x <- model.matrix(fare_amount ~ ., dff)
y <- dff$fare_amount
cv_out <- cv.glmnet(x, y, alpha = 1, type.measure = "mse")
plot(cv_out)
cv_out

MSE_lasso = 6.267


#RIDGE REGRESSION model

cv.out <- cv.glmnet(x, y, alpha = 0, type.measure = "mse")
plot(cv.out)
cv.out

MSE_ridge = 45.23
########################################################################################
## Slide 19 and 20 K-Nearest Neighbors
#########################################################################################

## K-Nearest Neighbors 
rm(list=ls())
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


########################################################################################
#K-fold cross validation with fare amt predicted by distance and num passengers
#########################################################################################

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
