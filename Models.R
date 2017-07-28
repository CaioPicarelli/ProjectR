
library(ggplot2)
rm(list=ls())

#' Returns a ggplot scatter plot for chosen explanatory variables
plot.explanatory.variable <- function(data, variable) {
  ggplot(data, aes_string(x=variable, y="Value")) + geom_point()
}

#' Returns a histogram for selected variable
# TODO: Replace with real implementation
plot.explanatory.histogram <- function(data, variable) {
  temp.data <- data.frame(
    x = rnorm(1000)
  )
  ggplot(temp.data, aes(x)) + geom_histogram()
}

#' Returns linear model regression coeffs
# TODO: Replace with real implementation
linear.reg.coeffs <- function(data) {
  data.frame(
    Medium = c("TV", "Radio", "Print"),
    Coefficient = c(1234.0, 5678.0, 9876.0)
  )
}

#' Returns linear regression spend curve plot
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

#' Returns a ggplot scatter plot for TS period and Value
plot.TS.period <- function(data, variable) {
  ggplot(data, aes_string(x=Period, y=Value)) + geom_point() + 
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

# Building new data frame transforming Spends to Adstocks
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
  
  N <- nrow(data)
  data$TVads <- adstock(data$TVGRPs, carryOver$co_TV, N)
  data$Radioads <- adstock(data$RadioGRPs, carryOver$co_Radio, N)
  data$OOHads <- adstock(data$OOHGRPs, carryOver$co_OOH, N)
  data$Printads <- adstock(data$PrintGRPs, carryOver$co_Print, N)
  data$Digads <- adstock(data$DigGRPs, carryOver$co_Dig, N)
  
  #how to assign this data to an object and call it on function below?
  
  df <- data.frame(data$Value,data$Distribution,data$TVads,data$Digads,data$Radioads,
                   data$OOHads,data$Printads)

  return(df)
  
}

#' Runs Linear TS Regression on Value and media
LM <- function(df) {
  TS.fit <- lm(df$Value ~ df$TVads + df$Digads + df$OOHads + df$Radioads + 
                 df$Printads + df$Distribution, df)
  
  coefficients <- data.frame(TS.fit$coefficients)
  
  return(coefficients)
}


plot.LM.TS.period <- function(TS.fit,newData){
  TS.pred <- predict(TS.fit,newData)
  ggplot(data, aes_string(x=Period)) + geom_line(y = Value,colour = "red") +
    geom_line(y = TS.pred,colour = "blue")
    theme(axis.text.x = element_text(size = 10,angle = 90)) + 
    theme(axis.text.y = element_text(size = 10,angle = 0)) + 
    labs(y = "Value vs Predict",x = "Period",title = "Value vs fitted")
}






