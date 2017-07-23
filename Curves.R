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

CS_Sales<- read.csv("CS-Sales.csv",sep = ",",header = T)
attach(CS_Sales)

#Add All media spends into one column:  Don't do this now....No Real Data to Use
CS_Sales$Spend <- CS_Sales$TV + CS_Sales$Digital + CS_Sales$OOH + CS_Sales$Radio +CS_Sales$Print

#Data Check
dim(CS_Sales)

plot.explanatory.variable <- function(data, variable) {
  ggplot(data, aes_string(x=variable, y="Value")) + geom_point()
}


#Correlation Matrix
CS_Corr <- data.frame(CS_Sales$Value,CS_Sales$TV,CS_Sales$Digital,
                      CS_Sales$OOH,CS_Sales$Radio,CS_Sales$Print,CS_Sales$Distribution)

CS_Corr <- corrplot.mixed(corr=cor(CS_Corr,use="complete.obs"),upper="ellipse",tl.pos = "lt")

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

#Bootstrap
# CS_Sales_Bootstrap <- data.frame(CS_Sales$Spend,CS_Sales$Value)
# 
# alpha.fn = function(data,index){
#     X=data$X[index]
#     Y=data$Y[index]
#     return((var(Y)-cov(X,Y))/(var(X)+var(Y)-2*cov(X,Y)))}
# set.seed(1)
# 
# alpha.fn(CS_Sales_Bootstrap,1:10)


#6.5.1 Best Subset Selection ====== Find a way to do 
library(leaps)

CS_Sales_BestSS <- data.frame(CS_Sales$Value,CS_Sales$Volume,CS_Sales$TV,
                              CS_Sales$Digital,CS_Sales$OOH,CS_Sales$Radio,CS_Sales$Print,
                              CS_Sales$Distribution)

REGFIT_CS_Sales <- regsubsets(CS_Sales_BestSS$CS_Sales.Value ~., CS_Sales_BestSS)

## CREATE MORE OUTPUT - NOT WORKING
summary(REGFIT_CS_Sales)
plot(REGFIT_CS_Sales,scale = "adjr2",main = "Adjusted R^2")

plot(REGFIT_CS_Sales$rss,xlab = "Number of Variables", ylab = "RSS",type = "l")

plot(REGFIT_CS_Sales,scale = "r2")
plot(REGFIT_CS_Sales,scale = "adjr2")
plot(REGFIT_CS_Sales,scale = "Cp")
plot(REGFIT_CS_Sales,scale = "bic")

#6.5.2 Forward and Backward Stepwise Selection
REGFIT_CS_Sales.fwd <- regsubsets(CS_Sales_BestSS$CS_Sales.Value ~ .,CS_Sales_BestSS,
                                  nvmax=8, method = "forward")
summary(REGFIT_CS_Sales.fwd)

REGFIT_CS_Sales.bwd <- regsubsets(CS_Sales_BestSS$CS_Sales.Value ~.,CS_Sales_BestSS,
                                  nvmax = 8,method = "backward")
summary(REGFIT_CS_Sales.bwd)

#6.5.3 Choosing Among Models Using the Validation Set Approach and 
#Cross-Validation

set.seed(1)
train <- sample(c(TRUE,FALSE), nrow(CS_Sales_BestSS),rep=TRUE)
test <- (!train)





