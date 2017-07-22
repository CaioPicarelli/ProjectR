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

Chart1 <- ggplot(CS_Sales,aes(x=TV,y=Value)) + geom_point()
Chart1
Chart2 <- ggplot(CS_Sales,aes(x=Digital,y=Value)) + geom_point()
Chart2
Chart3 <- ggplot(CS_Sales,aes(x=OOH,y=Value)) + geom_point()
Chart3
Chart4 <- ggplot(CS_Sales,aes(x=Radio,y=Value)) + geom_point()
Chart4
Chart5 <- ggplot(CS_Sales,aes(x=Print,y=Value)) + geom_point()
Chart5
Chart6 <- ggplot(CS_Sales,aes(x=Spend,y=Value)) + geom_point()
Chart6

plot.explanatory.variable <- function(data, variable) {
  ggplot(data, aes_string(x=variable, y="Value")) + geom_point()
}

#Breakage of Money by Media


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


#Best Subset Selection
library(leaps)

CS_Sales_BestSS <- data.frame(CS_Sales$Value,CS_Sales$Volume,CS_Sales$TV,
                              CS_Sales$Digital,CS_Sales$OOH,CS_Sales$Radio,CS_Sales$Print,
                              CS_Sales$Distribution)

REGFIT_CS_Sales <- regsubsets(CS_Sales_BestSS$CS_Sales.Value ~., CS_Sales_BestSS)

## CREATE MORE OUTPUT
summary(REGFIT_CS_Sales)
plot(REGFIT_CS_Sales,scale = "adjr2",main = "Adjusted R^2")

#Forward and Backward Stepwise Selection




#Checking Residuals vs Fitted Values
Check1 <- ggplot(CS_Model,aes(x=CS_Model$fitted.values,y=CS_Model$residuals)) + geom_point()
Check1
