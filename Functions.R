
library(ggplot2)
library(glmnet)
library(scales)
library(boot)

rm(list=ls())

#' Returns a ggplot scatter plot for chosen explanatory variables
plot.explanatory.variable <- function(data, variable) {
  ggplot(data, aes_string(x=variable, y="Value")) + geom_point()
}

#' Returns a ggplot bar plot for TS TV and Value =======================================
plot.TS.TV <- function(data,variable) {
  ggplot(data) + 
    geom_line(aes(x=Period,y=Value),stat = "identity",lwd=1.2) +
    geom_bar(aes(x=Period,y=100*TV),stat = "identity",fill = "tan1",colour="sienna3") +
    scale_y_continuous(sec.axis = sec_axis(~./100,name="TV"),labels = comma) +
    labs(y = "Monthly Sales Value", x = "Period")
}

#' Returns a ggplot bar plot for TS Radio and Value =======================================
plot.TS.Radio <- function(data,variable) {
  ggplot(data) + 
    geom_line(aes(x=Period,y=Value),stat = "identity",lwd=1.2) +
    geom_bar(aes(x=Period,y=100*Radio),stat = "identity",fill = "tan1",colour="sienna3") +
    scale_y_continuous(sec.axis = sec_axis(~./100,name="Radio"),labels = comma) +
    labs(y = "Monthly Sales Value", x = "Period")
}

#' Returns a ggplot bar plot for TS Print and Value =======================================
plot.TS.Print <- function(data,variable) {
  ggplot(data) + 
    geom_line(aes(x=Period,y=Value),stat = "identity",lwd=1.2) +
    geom_bar(aes(x=Period,y=100*Print),stat = "identity",fill = "tan1",colour="sienna3") +
    scale_y_continuous(sec.axis = sec_axis(~./100,name="Print"),labels = comma) +
    labs(y = "Monthly Sales Value", x = "Period")
}

#' Returns a ggplot bar plot for TS OOH and Value =======================================
plot.TS.OOH <- function(data,variable) {
  ggplot(data) + 
    geom_line(aes(x=Period,y=Value),stat = "identity",lwd=1.2) +
    geom_bar(aes(x=Period,y=100*OOH),stat = "identity",fill = "tan1",colour="sienna3") +
    scale_y_continuous(sec.axis = sec_axis(~./100,name="OOH"),labels = comma) +
    labs(y = "Monthly Sales Value", x = "Period")
}

#' Returns a ggplot bar plot for TS Digital and Value =======================================
plot.TS.Digital <- function(data,variable) {
  ggplot(data) + 
    geom_line(aes(x=Period,y=Value),stat = "identity",lwd=1.2) +
    geom_bar(aes(x=Period,y=100*Digital),stat = "identity",fill = "tan1",colour="sienna3") +
    scale_y_continuous(sec.axis = sec_axis(~./100,name="Digital"),labels = comma) +
    labs(y = "Monthly Sales Value", x = "Period")
}

# Function to Calculate Adstocks on TS Media Spends converted to GRPs
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
  TS.fit <- lm(data.Value ~ data.TVads + data.Digads + data.OOHads + data.Radioads + 
                 data.Printads + data.Distribution, df)
  
  Results <- data.frame(predictor = c("Intercept","TV","Digital","OOH","Radio","Print","Distribution"))
  coefs <- as.data.frame(summary(TS.fit)$coefficients)
  Results[,names(coefs)] <- coefs
  
  return(Results)
}

#' Return Stats for OLS TS Value ~ Media =======================================
MSE.Rsq.LM <- function(data, input) {
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
  Rsqr <- summary(TS.fit)$r.squared
  
  TS.stats <- data.frame(MSE = MSE, Rsqr = Rsqr)
  
  return(TS.stats)
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

# Print stats from OLS with Dummies ================================================
MSE.Rsq.LM.du <- function(data, input) {
  df <- TS.Ads.CO(data, input)
  TS.fit.du <- lm(data.Value ~ data.TVd + data.Digd + data.OOHd + data.Radiod + 
                    data.Printd , df)
  pred.data <- df[, c(
    "data.TVd",
    "data.Digd",
    "data.OOHd",
    "data.Radiod",
    "data.Printd")]
  
  pred <- predict(TS.fit.du,pred.data,se=TRUE)
  MSE <- mean(df$data.Value - predict(TS.fit.du,pred.data))^2
  Rsqr <- summary(TS.fit.du)$r.squared
  
  TS.d.stats <- data.frame(MSE = MSE, Rsqr = Rsqr)
  
  return(TS.d.stats)
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

# Function to build a range with Negative exponential functional form
neg.exp <- function(sales.max, spend.range, spend.slope) {
  sales.max*(1 - exp(-spend.range/spend.slope))
}

 # Build curves out of regression of dummy variables
Curves <- function(data,input){
  
  # Range of spends for chart
  coeffs <- SpendSlope_TS(data,input)
  spendsRange <- seq(0,300000,length.out = 25)

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
  ggplot(curves, aes(x=Spend,y=Sales, color=Media)) + geom_line() +
    scale_y_continuous(label = comma) + scale_x_continuous(label = comma)
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


# =========================CROSS SECTIONAL ===============================================


#' Returns Spends by media for each brand in Category
CS.Spends <- function(data,variable) {
  ggplot(data, aes_string(x=variable, y="Value")) +
    geom_point() +
    geom_text(aes(label=Brand),hjust=0,vjust=0) +
    scale_x_continuous(labels = comma) +
    scale_y_continuous(labels = comma)
}

# Return data with Spends column summing all spends across channels
CS.SUM.Spends <- function(data) {
  data$Spends <- data$TV + data$Radio +data$Print + data$Digital + data$OOH
  data$Spend.Sales.Ratio <- data$Spends/data$Value
  data$totalSpends <- sum(data$Spends)
  data$totalValue <- sum(data$Value)
  data$SOS <- data$Spends/data$totalSpends
  data$SOM <- data$Value/data$totalValue
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
  
  MSE <- mean((log(total.spends$Value) - predict(CS.log.Linear,pred.data))^2)
  Rsqr <- summary(CS.log.Linear)$r.squared
  
  stats <- data.frame(MSE = MSE,Rsqr = Rsqr)
  return(stats)
}

# Plot Regression Curve between Value ~ Spends
CS.ggplot.line <- function(data, selectedBrand) {
  total.spends <- CS.SUM.Spends(data)
  CS.log.Linear <- lm(log(Value) ~ log(Spends), total.spends)
  
  ggplot(total.spends,aes(x=Spends,y=Value,color=(Brand == selectedBrand))) + 
    geom_point() +
    geom_line(aes(y=exp(CS.log.Linear$fitted.values))) +
    scale_x_continuous(labels = comma) +
    scale_y_continuous(labels = comma)
}


# Plot Box and Whisker
boxnwhisker <- function(data){
  total.spends <- CS.SUM.Spends(data)
  ggplot(total.spends,aes(y = Value,x="")) + 
    geom_boxplot() + 
    geom_jitter(width = 0.2) +
    geom_text(aes(label = Brand),hjust=0,vjust=0)
}

# CS Spends vs Brands
CS.Spend.Brand <- function(data){
  total.spends <- CS.SUM.Spends(data)
  
  ggplot(total.spends,aes(x=Brand,y=Spends)) +
    geom_col() +
    theme(axis.text.x = element_text(angle=90)) +
    scale_y_continuous(labels = comma)
}

# CS Spend/Sales Ratio
CS.Spend.Sales.Ratio<-function(data){
  total.spends <- CS.SUM.Spends(data)
  
  ggplot(total.spends,aes(x=Brand,y=Spend.Sales.Ratio)) +
    geom_col() +
    theme(axis.text.x = element_text(angle=90))
}

# CS SOS vs SOM
CS.SOS.SOM <-function(data){
  total.spends <- CS.SUM.Spends(data)
  ggplot(total.spends,aes(x=SOS,y=SOM))+
    geom_point() +
    geom_text(aes(label=Brand),hjust=0,vjust=0)+
    geom_abline(intercept = 0,slope = 1,colour = "blue")
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

# Sales per Period for Product 1
p1.sales.period <- function(data){
  ggplot(data,aes(y = p1sales,x = Week,color = as.factor(Year))) +
    geom_line()
}

# Prices per Period for Product 1
p1.sales.price.period <- function(data){
  ggplot(data,aes(y = p1sales,x = p1price,color = as.factor(Year))) +
    geom_line()
}


# BoxPlot Sales per Period for Product 1
p1.sales.prom.box <- function(data){
  boxplot(p1sales ~ p1prom, data=data,yaxt="n",xlab="P2 promoted in Store?",ylab="Weekly Sales",
          main="Weekly Sales of P2 with and without Promotion")
  axis(side = 2, at=c(1,2),labels = c("No","Yes"))
}

# Model running GLM on p1prom and sales
PP.model <- function(data){
  fit <- glm(p1prom ~ p1sales,data, family = binomial)

  pp.stats <- data.frame(summary(fit)$coefficients)
  return(pp.stats)
}

# PP Model with prediction
PP.Prediction <-function (data,input){
  fit <- glm(p1prom ~ p1sales,data, family = binomial)
  x <- data.frame(p1sales = input$PP.Value)
  
  Probability <- predict(fit,x,type="response")
  return(as.data.frame(Probability))
}


# PP Model Fitted vs Sales Chart
PP.Prediction.chart <-function (data){
  fit <- glm(p1prom ~ p1sales,data, family = binomial)
  
  plot(data$p1sales,fit$fitted.values,col="blue",pch="o",xlab = "p1sales",ylab = "Fitted Values")
  xrange = seq(min(data$p1sales),max(data$p1sales),length.out = 100)
  
  lines(xrange,inv.logit(fit$coefficients[1]+fit$coefficients[2]*xrange),col="red")
  y <- data$p1prom
  x <- data$p1sales
  glm.chart <- glm(y ~ x,family = binomial)
  yrange <- predict(glm.chart,data.frame(x=xrange),type="response")
  lines(xrange,yrange,col="red")
}

# Model running GLM on p1sales and p1prices
PP.model.price <- function(data){
  fit <- lm(log(p1sales) ~ log(p1price),data)
  
  pp.price.stats <- data.frame(summary(fit)$coefficients)
  return(pp.price.stats)
}

# MSE and Rsqr
PP.model.price.stats <- function(data){
  fit <- lm(log(p1sales) ~ log(p1price),data)
  
  pred.data <- data.frame(p1price = log(data[, c("p1price")]))
  
  predicted <- predict(fit, pred.data)
  MSE <- mean((log(data$p1sales) - predicted)^2)
  Rsqr <- summary(fit)$r.squared
  stats <- data.frame(MSE = MSE,Rsqr = Rsqr)
  return(stats)
}


