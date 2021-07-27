
rm(list=ls())

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

