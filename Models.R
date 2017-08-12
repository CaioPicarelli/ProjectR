
library(ggplot2)
library(glmnet)
library(scales)
rm(list=ls())

#' Returns a ggplot scatter plot for chosen explanatory variables
plot.explanatory.variable <- function(data, variable) {
  ggplot(data, aes_string(x=variable, y="Value")) + geom_point()
}

#' Returns a histogram for selected variable =======================================
# TODO: Replace with real implementation
plot.explanatory.histogram <- function(data, variable) {
  temp.data <- data.frame(
    x = rnorm(1000)
  )
  ggplot(temp.data, aes(x)) + geom_histogram()
}

#' Returns linear model regression coeffs =======================================
# TODO: Replace with real implementation
linear.reg.coeffs <- function(data) {
  data.frame(
    Medium = c("TV", "Radio", "Print"),
    Coefficient = c(1234.0, 5678.0, 9876.0)
  )
}

#' Returns linear regression spend curve plot =======================================
# TODO: Replace with real implementation
linear.reg.spend.curve <- function(data) {
  temp.data <- data.frame(
    Spend = seq(0, 1e6, length.out = 100),
    Sales = c(seq(0, 1e7, length.out = 100),
              seq(0, 2e7, length.out = 100)),
    Media = c(rep("TV", 100), rep("Radio", 100))
  )
  
  ggplot(temp.data, aes(x=Spend, y=Sales, color=Media)) + geom_line()
}

#' Returns a ggplot scatter plot for TS period and Value =======================================
plot.TS.period <- function(data, variable) {
  ggplot(data, aes(x=Period, y=Value)) + geom_line() + 
  theme(axis.text.x = element_text(size = 10,angle = 90)) + 
  theme(axis.text.y = element_text(size = 10,angle = 0)) + 
  labs(y = "Value",x = "Period",title = "Value vs Period")
}

adstock <- function(GRP, ret.factor, n) {
  adstock <- integer(length = n)
  adstock[1] <- GRP[1]
  for(i in 2:n) {
    adstock[i] <- (adstock[i - 1] * ret.factor) + GRP[i]
  }
  
  return(adstock)
}


# Building new data frame transforming Spends to Adstocks =======================================
TS.Ads.CO <- function(data, input){
  carryOver <- data.frame(co_TV = input$co_TV,
                          co_Radio = input$co_Radio,
                          co_OOH = input$co_OOH,
                          co_Print = input$co_Print,
                          co_Dig = input$co_Dig)
  Cost <- data.frame(CGRP_TV = input$CGRP_TV,
                     CGRP_Radio = input$CGRP_Radio,
                     CGRP_OOH = input$CGRP_OOH,
                     CGRP_Print = input$CGRP_Print,
                     CGRP_Dig = input$CGRP_Dig)
  data$TVGRPs <- data$TV/Cost$CGRP_TV
  data$RadioGRPs <- data$Radio/Cost$CGRP_Radio
  data$OOHGRPs <- data$OOH/Cost$CGRP_OOH
  data$PrintGRPs <- data$Print/Cost$CGRP_Print
  data$DigGRPs <- data$Digital/Cost$CGRP_Dig
  
  # create adstocks
  N <- nrow(data)
  data$TVads <- adstock(data$TVGRPs, carryOver$co_TV, N)
  data$Radioads <- adstock(data$RadioGRPs, carryOver$co_Radio, N)
  data$OOHads <- adstock(data$OOHGRPs, carryOver$co_OOH, N)
  data$Printads <- adstock(data$PrintGRPs, carryOver$co_Print, N)
  data$Digads <- adstock(data$DigGRPs, carryOver$co_Dig, N)
  
  # dummies for media
  data$TVd <- ifelse(data$TV == 0, 0,1)
  data$Radiod <- ifelse(data$Radio == 0, 0,1)
  data$OOHd <- ifelse(data$OOH == 0, 0,1)
  data$Printd <- ifelse(data$Print == 0, 0,1)
  data$Digd <- ifelse(data$Digital == 0, 0,1)
  
  df <- data.frame(data$Period,data$Value,data$Distribution,data$TVads,data$Digads,data$Radioads,
                   data$OOHads,data$Printads,data$TVd,data$Radiod,data$OOHd,data$Printd,data$Digd)

  return(df)
  
}

#' Runs OLS TS Regression on Value and media =======================================
LM <- function(data, input) {
  df <- TS.Ads.CO(data, input)
  TS.fit.du <- lm(data.Value ~ data.TVads + data.Digads + data.OOHads + data.Radioads + 
                 data.Printads + data.Distribution, df)
  
  Results <- data.frame(predictor = c("Intercept","TV","Digital","OOH","Radio","Print","Distribution"))
  coefs <- as.data.frame(summary(TS.fit.du)$coefficients)
  Results[,names(coefs)] <- coefs
  
  return(Results)
}

# Get Spend Slope for each media = 100 GRPs * Cost per GRPs
SpendSlope_TS <- function(data,input){
  df <- TS.Ads.CO(data, input)
  
  data.frame(TV = input$CGRP_TV * 100,
             Radio = input$CGRP_Radio * 100,
             OOH = input$CGRP_OOH * 100,
             Print = input$CGRP_Print * 100,
             Digital = input$CGRP_Dig * 100)
}

#' Runs OLS TS Regression on Value and DUMMY media =======================================
TS.OLS.du <- function(data, input) {
  df <- TS.Ads.CO(data, input)
  TS.fit.du <- lm(data.Value ~ data.TVd + data.Digd + data.OOHd + data.Radiod + 
                 data.Printd , df)
  
  Results <- data.frame(predictor = c("Intercept","TV","Digital","OOH","Radio","Print"))
  coefs <- as.data.frame(summary(TS.fit.du)$coefficients)
  Results[,names(coefs)] <- coefs
  
  return(Results)
}

# Get Spend Slope for each media = 100 GRPs * Cost per GRPs
SpendSlope_TS <- function(data,input){
  df <- TS.Ads.CO(data, input)
  results <- TS.OLS.du(data, input)
  
  results[, "Slope"] <-  c(0,
                          input$CGRP_TV * 100,
                          input$CGRP_Radio * 100,
                          input$CGRP_OOH * 100,
                          input$CGRP_Print * 100,
                          input$CGRP_Dig * 100)
  return(results)
}

neg.exp <- function(sales.max, spend.range, spend.slope) {
  sales.max*(1 - exp(-spend.range/spend.slope))
}

 # Build curves out of regression of dummy variables
Curves <- function(data,input){
  
  # Range of spends for chart
  coeffs <- SpendSlope_TS(data,input)
  spendsRange <- seq(0,500000,length.out = 25)

  media = c("TV", "Digital", "OOH", "Radio","Print")

  curveData <- NULL
  for(i in 1:length(media)) {
    medium <- media[i]
    curveData <- c(curveData, neg.exp(
      coeffs$Estimate[i + 1],
      spendsRange,
      coeffs$Slope[i + 1]
    ))
  }
  
  labels <- NULL
  for(medium in media) {
    labels <- c(labels, rep(medium, length(spendsRange)))
  }

  curves <- data.frame(
    Spend = rep(spendsRange, length(media)),
    Sales = curveData,
    Media = labels
  )

  
  ggplot(curves, aes(x=Spend,y=Sales, color=Media)) + geom_line()
}



#' Get Predictions and Error from LM =======================================
MSE.LM <- function(data, input) {
  df <- TS.Ads.CO(data, input)
  TS.fit <- lm(data.Value ~ data.TVads + data.Digads + data.OOHads + data.Radioads + 
                 data.Printads + data.Distribution, df)
  pred.data <- df[, c(
    "data.TVads",
    "data.Digads",
    "data.OOHads",
    "data.Radioads",
    "data.Printads",
    "data.Distribution")]
  
  pred <- predict(TS.fit,pred.data,se=TRUE)
  
  MSE <- mean(df$data.Value - predict(TS.fit,pred.data))^2
  
  
  return(MSE)
}

  # Get plot of observed and fitted =======================================
plot.LM <- function(data,input){
  df <- TS.Ads.CO(data, input)
  TS.fit <- lm(data.Value ~ data.TVads + data.Digads + data.OOHads + data.Radioads + 
                 data.Printads + data.Distribution, df)
  pred.data <- df[, c(
                "data.TVads",
                "data.Digads",
                "data.OOHads",
                "data.Radioads",
                "data.Printads",
                "data.Distribution")]

  pred <- predict(TS.fit,pred.data,se=TRUE)
  
  ggplot(data, aes(x=Period, y = Value)) + 
    geom_line(colour = "red") +
    geom_line(aes(y = pred$fit), colour = "blue") + 
    theme(axis.text.x = element_text(size = 10,angle = 90)) + 
    theme(axis.text.y = element_text(size = 10,angle = 0)) + 
    labs(y = "Value vs Predict",x = "Period",title = "Value vs fitted")
}


 # Ridge Regression on Input.
LM.Ridge <- function(data,input){
  
  df <- TS.Ads.CO(data, input)
  x <- model.matrix(df$data.Value ~ df$data.TVads + data.Digads + data.OOHads + data.Radioads + 
                      data.Printads + data.Distribution,df)[,-1]
  y <- df$data.Value
  grid = 10^seq(10,-2,length=100)
  
  'splitting sets into training and testing'
  set.seed (1)
  train = sample(1:nrow(x), nrow(x)/2)
  test = (-train)
  y.test = y[test]
  
  ridge <- glmnet(x[train,], y[train], alpha=0, lambda = grid, thresh =1e-12)
  cv.out <- cv.glmnet(x[train,], y[train], alpha = 0)
  
  #names
  cv.out.df <- data.frame(cv.out$lambda,cv.out$cvm)
  names(cv.out.df)[1] <- paste("lambda")
  names(cv.out.df)[2] <- paste("cvm")
  
  bestlam = cv.out$lambda.min

ggplot(cv.out.df,aes(x=log(lambda),y=cvm)) + geom_line(colour = "red",size = 3)
}

# Run Lasso TS data
LM.Lasso <- function(data,input){
  
  df <- TS.Ads.CO(data, input)
  x <- model.matrix(df$data.Value ~ df$data.TVads + data.Digads + data.OOHads + data.Radioads + 
                      data.Printads + data.Distribution,df)[,-1]
  y <- df$data.Value
  grid = 10^seq(10,-2,length=100)
  
  'splitting sets into training and testing'
  set.seed (1)
  train = sample(1:nrow(x), nrow(x)/2)
  test = (-train)
  y.test = y[test]
  
  set.seed(1)
  cv.out <- cv.glmnet(x[train,],y[train],alpha = 1)
  
  #names
  cv.out.df <- data.frame(cv.out$lambda,cv.out$cvm)
  names(cv.out.df)[1] <- paste("lambda")
  names(cv.out.df)[2] <- paste("cvm")
  
  bestlam = cv.out$lambda.min
  
  ggplot(cv.out.df,aes(x=log(lambda),y=cvm)) + geom_line(colour = "red",size = 3)
}


# =========================CROSS SECTIONAL ===============================================

#' Returns Spends by media for each brand in Category
CS.Spends <- function(data,variable) {
  ggplot(data, aes_string(x=variable, y="Value")) + geom_point()
}

# Return data with Spends column summing all spends across channels
CS.SUM.Spends <- function(data) {
  data$Spends <- data$TV + data$Radio +data$Print + data$Digital + data$OOH
  return(data)
}

# Linear model with Total Spends
CS.lm <- function(data) {
  total.spends <- CS.SUM.Spends(data)
  CS.Linear <- lm(Value ~ Volume + Spends + Distribution, total.spends)
  
  Results.cs <- data.frame(predictor = c("Intercept","Volume","Spends","Distribution"))
  coeffs <- as.data.frame(summary(CS.Linear)$coefficients)
  Results.cs[,names(coeffs)] <- coeffs
  
  return(Results.cs)
}

# Stats from Linear model with Total Spends
CS.lm.MSE.R <- function(data) {
  total.spends <- CS.SUM.Spends(data)
  CS.Linear <- lm(Value ~ Volume + Spends + Distribution, total.spends)
    
    pred.data <- total.spends[,c("Volume","Spends","Distribution")]
    MSE <- mean((total.spends$Value - predict(CS.Linear,pred.data))^2)
    Rsqr <- summary(CS.Linear)$r.squared
    
    stats <- data.frame(MSE = MSE,Rsqr = Rsqr)
  return(stats)
}

# Log-Log model with Total Spends
CS.log.lm <- function(data) {
  total.spends <- CS.SUM.Spends(data)
  CS.log.Linear <- lm(log(Value) ~ log(Volume) + log(Spends) + log(Distribution), total.spends)
  
  Results.log.cs <- data.frame(predictor = c("log(Intercept)","log(Volume)","log(Spends)","log(Distribution)"))
  coeffs <- as.data.frame(summary(CS.log.Linear)$coefficients)
  Results.log.cs[,names(coeffs)] <- coeffs
  
  return(Results.log.cs)
}

# Stats from Log-Log model with Total Spends
CS.log.MSE.R <- function(data) {
  total.spends <- CS.SUM.Spends(data)
  CS.log.Linear <- lm(log(Value) ~ log(Volume) + log(Spends) + log(Distribution), total.spends)
  
  pred.data <- data.frame(Volume = log(total.spends$Volume),Spends = log(total.spends$Spends),
                          Distribution = log(total.spends$Distribution))
  
  MSE <- mean((total.spends$Value - predict(CS.log.Linear,pred.data))^2)
  Rsqr <- summary(CS.log.Linear)$r.squared
  
  stats <- data.frame(MSE = MSE,Rsqr = Rsqr)
  return(stats)
}

# Plot Regression Curve between Value ~ Spends
CS.ggplot.line <- function(data, selectedBrand) {
  total.spends <- CS.SUM.Spends(data)
  CS.log.Linear <- lm(log(Value) ~ log(Spends), total.spends)
  
  ggplot(total.spends,aes(x=Spends,y=Value,
                          color=(Brand == selectedBrand))) + 
    geom_point() +
    geom_line(aes(y=exp(CS.log.Linear$fitted.values)))
}


# Plot Box and Whisker
boxnwhisker <- function(data){
  total.spends <- CS.SUM.Spends(data)
  ggplot(total.spends,aes(y = Value,x="")) + 
    geom_boxplot(colour="blue") + 
    geom_jitter(width = 0.2) +
    geom_boxplot(outlier.colour ="red", outlier.shape = 1)
}

# =========================PRICE AND PROMOTIONS ===========================================

# Histogram of Modelled Product - Frequency of Sales - Product 1
hist.p1 <- function(data){
  ggplot(data,aes(data$p1sales)) + geom_histogram() +
    theme(axis.text.y = element_text(size = 10,angle = 0)) +
    scale_y_continuous(labels = comma) +
    labs(y = "Frequency",title = "Product 1 - Weekly Sales Frequency") +
    labs(x="Product 1 Sales (Units)") 
    
}
# Histogram of Modelled Product - Frequency of Sales - Product 2
hist.p2 <- function(data){
  ggplot(data,aes(data$p2sales)) + geom_histogram() +
    theme(axis.text.y = element_text(size = 10,angle = 0)) +
    scale_y_continuous(labels = comma) +
    labs(y = "Frequency",title = "Product 2 - Weekly Sales Frequency") +
    labs(x="Product 2 Sales (Units)")
}

# Distribution by store for product 1
store.dist1 <- function(data){
  ggplot(data,aes(p1sales,fill = storeID)) + geom_histogram() +
    facet_wrap(~storeID,ncol = 1) + 
    labs(y="Frequency", title = "Product 1 - by Store") +
    labs(x = "Product 1 Sales (Units) by store")
}

# Distribution by store for product 2
store.dist2 <- function(data){
  ggplot(data,aes(p2sales,fill = storeID)) + geom_histogram() +
    facet_wrap(~storeID,ncol = 1) + 
    labs(y="Frequency", title = "Product 2 - by Store") +
    labs(x = "Product 2 Sales (Units) by store")
}





