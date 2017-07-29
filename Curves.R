#===================CURVES=======================================================================

#===============SALES VALUE, VOLUME and SPENDS===================================================

'Polynomial Regression - page 271'
'Regression Splines'
'chapters 6 and 7'

rm(list=ls())

library(ggplot2)
library(ISLR)
library(MASS)
library(corrplot)
library(leaps)
setwd("~/Desktop")

CS.Sales<- read.csv("CS-Sales.csv",sep = ",",header = T)
attach(CS.Sales)


#Data Check
dim(CS_Sales)

#Correlation Matrix
CS.Corr <- data.frame(CS.Sales$Value,CS.Sales$TV,CS.Sales$Digital,
                      CS.Sales$OOH,CS.Sales$Radio,CS.Sales$Print,CS.Sales$Distribution)

CS.Corr <- corrplot.mixed(corr=cor(CS.Corr,use="complete.obs"),upper="ellipse",tl.pos = "lt")

#Linear Model with all variables
CS_Model <- lm(Value ~ Volume + TV + OOH + Print + Digital
               + Radio + Distribution,data = CS_Sales)
summary(CS_Model)

MSE_CS_Media <- mean(Value-predict(CS_Model,CS_Sales))^2
MSE_CS_Media

#Simple Linear Regression with aggregate Spends
CS_Model_Spends <- lm(Value ~ Spend,data = CS_Sales)
summary(CS_Model_Spends)

MSE_CS_Spends <- mean(Value-predict(CS_Model_Spends,CS_Sales))^2
MSE_CS_Spends

#Using poly() to estimate test error for polynomial and cubic regressions
CS_Model_Spends2 <- lm(Value ~ poly(Spend,2),data = CS_Sales)
CS_Model_Spends3 <- lm(Value ~ poly(Spend,3),data = CS_Sales)

MSE_CS_Spends2 <- mean(Value-predict(CS_Model_Spends2,CS_Sales))^2
MSE_CS_Spends2
MSE_CS_Spends3 <- mean(Value-predict(CS_Model_Spends3,CS_Sales))^2
MSE_CS_Spends3

#6.5.1 Best Subset Selection ====== Find a way to do 
library(leaps)

CS_Sales_BestSS <- data.frame(CS_Sales$Value,CS_Sales$Volume,CS_Sales$TV,
                              CS_Sales$Digital,CS_Sales$OOH,CS_Sales$Radio,
                              CS_Sales$Print,CS_Sales$Distribution)

REGFIT_CS_Sales <- regsubsets(CS_Sales_BestSS$CS_Sales.Value ~., CS_Sales_BestSS)

summary(REGFIT_CS_Sales)
plot(REGFIT_CS_Sales,scale = "adjr2",main = "Adjusted R^2")

plot(REGFIT_CS_Sales$rss,xlab = "Number of Variables", ylab = "RSS",type = "l")


plot(REGFIT_CS_Sales,scale = "r2")
plot(REGFIT_CS_Sales,scale = "adjr2")
plot(REGFIT_CS_Sales,scale = "Cp")
plot(REGFIT_CS_Sales,scale = "bic")

#6.5.2 Forward and Backward Stepwise Selection
REGFIT_CS_Sales.fwd <- regsubsets(CS_Sales_BestSS$CS_Sales.Value ~ .,CS_Sales_BestSS,
                                  nvmax=20, method = "forward")
summary(REGFIT_CS_Sales.fwd)

plot(REGFIT_CS_Sales.fwd,scale = "r2")
plot(REGFIT_CS_Sales.fwd,scale = "adjr2")
plot(REGFIT_CS_Sales.fwd,scale = "Cp")
plot(REGFIT_CS_Sales.fwd,scale = "bic")


REGFIT_CS_Sales.bwd <- regsubsets(CS_Sales_BestSS$CS_Sales.Value ~.,CS_Sales_BestSS,
                                  nvmax = 20,method = "backward")
summary(REGFIT_CS_Sales.bwd)
plot(REGFIT_CS_Sales.bwd,scale = "r2")
plot(REGFIT_CS_Sales.bwd,scale = "adjr2")
plot(REGFIT_CS_Sales.bwd,scale = "Cp")
plot(REGFIT_CS_Sales.bwd,scale = "bic")

# #6.5.3 Choosing Among Models Using the Validation Set Approach and 
# #Cross-Validation
# set.seed(1)
# CS_Training <- sample(c(TRUE,FALSE), nrow(CS_Sales_BestSS),rep=TRUE)
# CS_Test <- (!CS_Training)
# 
# REGFIT_CS_Sales <- regsubsets(CS_Sales_BestSS$CS_Sales.Value ~.,
#                               CS_Sales_BestSS[CS_Training,], nvmax = 20)
# summary(REGFIT_CS_Sales)
# 
# CS_Training <- model.matrix(CS_Sales_BestSS$CS_Sales.Value ~.,
#                             data = CS_Sales_BestSS[CS_Test,])
# 
# 
# val.errors <- rep(NA,20)
# for(i in 1:20) {
#   coefi <- coef(REGFIT_CS_Sales,id = i)
#   pred <- CS_Training[,names(coefi)]%*%coefi
#   val.errors[i] <- mean((CS_Sales_BestSS$CS_Sales.Value[CS_Test]-pred)^2)
# }
# 
# #among the models of different sizes using Cross Validation
# k = 10
# set.seed(1)
# folds <- sample(1:k,nrow(CS_Sales_BestSS),replace = TRUE)
# cv.errors <- matrix(NA,k,20,dimnames = list(NULL,paste(1:20)))
# 
# for(j in 1:k){
#   best.fit <- regsubsets(CS_Sales_BestSS$CS_Sales.Value ~ .,CS_Sales_BestSS[folds != j,],nvmax = 20)
#   for (i in 1:20){
#     pred <- predict(best.fit,CS_Sales_BestSS[folds==j,],id=i)
#     cv.errors[j,i] <- mean((CS_Sales_BestSS$CS_Sales.Value[folds == j] - pred)^2)
#   }
# }

#6.6 Lab 2: Ridge Regression and the Lasso
x=model.matrix(CS_Sales_BestSS$CS_Sales.Value ~ .,CS_Sales_BestSS)[,-1]
y=CS_Sales_BestSS$CS_Sales.Value
library(glmnet)

grid=10^seq(10,-2,length=100)
ridge.mod <- glmnet(x,y,alpha=0,lambda=grid)
dim(coef(ridge.mod))

'comparing Ridge levels of lambda'
ridge.mod$lambda [50]
coef(ridge.mod)[,50]
sqrt(sum(coef(ridge.mod)[-1,50]^2))
ridge.mod$lambda [60]
coef(ridge.mod)[,60]
sqrt(sum(coef(ridge.mod)[-1,60]^2))

predict(ridge.mod, s=50, type="coefficients")[1:8,]

'splitting sets into training and testing'
set.seed (1)
train=sample(1:nrow(x), nrow(x)/2)
test=(-train)
y.test=y[test]

ridge.mod=glmnet(x[train,],y[train],alpha=0,lambda=grid, thresh =1e-12)
ridge.pred=predict(ridge.mod, s=4 ,newx=x[test,])
mean((ridge.pred-y.test)^2)
ridge.pred=predict(ridge.mod, s=1e10 ,newx=x[test,])
mean((ridge.pred-y.test)^2)

'Using cross validation to determine lambda'
set.seed(1)
cv.out <- cv.glmnet(x[train,],y[train],alpha=0)
plot(cv.out)
bestlam = cv.out$lambda.min
bestlam

'MSE for Ridge'
ridge.pred=predict(ridge.mod,s=bestlam ,newx=x[test,])
mean((ridge.pred - y.test)^2)

out=glmnet(x,y,alpha=0)
predict(out,type="coefficients",s=bestlam)[1:8,]

#6.6.2 The Lasso
lasso.mod=glmnet(x[train ,],y[train],alpha=1,lambda=grid)
plot(lasso.mod)

'cross validation and compute associate error'
set.seed(1)
cv.out <- cv.glmnet(x[train,],y[train],alpha=1)
plot(cv.out)
bestlam=cv.out$lambda.min
lasso.pred=predict(lasso.mod,s=bestlam ,newx=x[test,])
mean((lasso.pred-y.test)^2)

#7.8 Lab: Non-linear Modeling
#7.8.1 Polynomial Regression and Step Functions
#Apply to function by media
library(ISLR)
fit <- lm(Value ~ poly(TV,4,raw = T), CS_Sales)
summary(fit2)

#using Wrapper I()
fit2a = lm(Value ~ TV + I(TV^2) + I(TV^3) + I(TV^4),CS_Sales)
coef(fit2a)

fit2b = lm(Value ~ cbind(TV,TV^2,TV^3,TV^4),data=CS_Sales)

TVlims = range(TV)
TV.grid = seq(from=TVlims[1],to=TVlims[2])
preds = predict(fit,newdata = list(TV = TV.grid),se=TRUE)
se.bands = cbind(preds$fit + 2*preds$se.fit,preds$fit - 2*preds$se.fit)
'ploting the data'

'par(mfrow=c(1,2),mar=c(4.5,4.5,1,1) ,oma=c(0,0,4,0))'
plot(TV,Value,xlim=TVlims ,cex=.5,col="darkgrey")
title("Degree -4 Polynomial ",outer=T)
lines(TV.grid,preds$fit,lwd=2,col="blue")
matlines(TV.grid,se.bands,lwd=1,col="blue",lty=3)

preds2=predict(fit2,newdata=list(TV=TV.grid),se=TRUE)
max(abs(preds$fit -preds2$fit ))

#ANOVA
fit.1=lm(Value ~ TV,CS_Sales)
fit.2=lm(Value ~ poly(TV,2),CS_Sales)
fit.3=lm(Value ~ poly(TV,3),CS_Sales)
fit.4=lm(Value ~ poly(TV,4),CS_Sales)
fit.5=lm(Value ~ poly(TV,5),CS_Sales)

anova(fit.1,fit.2,fit.3,fit.4,fit.5)

#Splines
library(splines)

fit=lm(Value ~ bs(TV,knots=c(500000,750000,1000000)),data = CS_Sales)
pred = predict(fit,newdata = list(TV=TV.grid),se=T)
plot(TV,Value,col="blue")

lines(TV.grid,pred$fit, lwd = 2)

lines(TV.grid,pred$fit + 2*pred$se ,lty="dashed")
lines(TV.grid,pred$fit - 2*pred$se ,lty="dashed")

# Fitting a Natural Spline
fit2 = lm(Value ~ ns(TV,df = 4),data = CS_Sales)
pred2 = predict(fit2,newdata = list(TV = TV.grid), se=T)
lines(TV.grid,pred2$fit,col = "red",lwd = 2)

# plot smoothing spline
plot(TV,Value,xlim = TVlims, cex = .5,col = "darkgrey")
title("Smoothing Spline")
fit = smooth.spline(TV,Value,df = 16)
fit2 = smooth.spline(TV,Value,cv = TRUE)
fit2$df
lines(fit,col = "red", lwd = 2)
lines(fit2,col = "blue", lwd = 2)

legend("topright",legend=c("16 DF","3.8 DF"),
       col=c("red","blue"),lty=1,lwd=2,cex=.8)

#GAMs
library(gam)
gam1 = lm(Value ~ ns(TV,4) + ns(Radio,5) + Volume, data = CS_Sales)

gam.m3 = gam(Value ~ s(TV,4) + s(Radio,5) + Volume,data = CS_Sales)

plot(gam.m3, se=TRUE,col="blue")
plot.gam(gam1, se=TRUE, col="red")

# Perform ANOVA in a series of GAM models to determine better model.
# Model1 performs GAM excluding Radio
# Model2 performs GAM using linear function of Radio
# Model3 spline function of Radio

gam.m1 = gam(Value ~ s(TV,4) + Volume ,data = CS_Sales)
gam.m2=gam(Value ~ Radio + s(TV,4) + Volume ,data = CS_Sales)
anova(gam.m1,gam.m2,gam.m3,test="F")
summary(gam.m2)

# Making Predictions with GAM
preds = predict(gam.m2, newdata = Value)



#TESTING===============================================================
#Times Series Download
setwd("~/Desktop")
TS.Sales<- read.csv("TS-Sales.csv",sep = ",",header = T)
TS.Sales$Period <- as.Date(TS.Sales$Period, format="%d/%m/%Y")
attach(TS.Sales)


test <- function(data) {
  TS.fit <- lm(Value ~ TV + Digital + OOH + Radio + Print + Distribution, data)
  newData <- data.frame(data$TV,data$Digital,data$OOH,data$Radio,data$Print
                        ,data$Distribution)
  
  df <- TS.fit$coefficients
  return(df)}

test(TS.Sales)


testdf <- function(df) {
  df.fit <- df
  return(df.fit)
}
testdf(TS.Sales)

test(TS.Sales)

Pica <- function(cock) {
  if (cock > 0) {
    result <- "Pau no CU"
  }
  else if (cock < 0 ){
    result <- "suck my dick"
  }
  else {
    result <- "deu merda"
  }
  df <- data.frame(result)
  return(df)
}
  
Pica(10)













