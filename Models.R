library(ggplot2)


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


# Building new data frame transforming Spends to Adstocks
TS.Ads.CO <- function(transformTS){
  carryOver <- data.frame(input$co_TV,input$co_Radio,input$co_OOH,input$co_Print,input$co_Dig)
  Cost <- data.frame(input$CGRP_TV,input$CGRP_Radio,input$CGRP_OOH,input$CGRP_Print,input$CGRP_Dig)
  TS.Sales$TVGRPs <- TV/CGRP_TV
  TS.Sales$RadioGRPs <- Radio/CGRP_Radio
  TS.Sales$OOHGRPs <- OOH/CGRP_OOH
  TS.Sales$PrintGRPs <- Print/CGRP_Print
  TS.Sales$DigGRPs <- Digital/CGRP_Dig
  
'===========Fix formula below'  
  TS.Sales$TVads <- (TS.Sales$TVads*co_TV) + TS.Sales$TVGRPs
  TS.Sales$Radioads <- (TS.Sales$TVads*co_TV) + TS.Sales$TVGRPs
  TS.Sales$OOHads <- (TS.Sales$OOHads*co_TV) + TS.Sales$OOHGRPs
  TS.Sales$Printads <- (TS.Sales$Printads*co_Print) + TS.Sales$PrintGRPs
  TS.Sales$Digads <- (TS.Sales$Digads*co_Dig) + TS.Sales$DigGRPs
  
}


#' Returns a ggplot scatter plot for TS Regression Value and media
plot.LM.TS.period <- function(data) {
  
  TS.fit <- lm(Value ~ TV + Digital + OOH + Radio + Print + Distribution, data)
  newData <- data.frame(TS.Sales$TV,TS.Sales$Digital,TS.Sales$OOH,
                        TS.Sales$Radio,TS.Sales$Print,TS.Sales$Distribution)
  TS.pred <- predict(TS.fit,newData)
  ggplot(data, aes_string(x=Period)) + geom_line(y = Value,colour = "red") +
    geom_line(y = TS.pred,colour = "blue")
    theme(axis.text.x = element_text(size = 10,angle = 90)) + 
    theme(axis.text.y = element_text(size = 10,angle = 0)) + 
    labs(y = "Value vs Predict",x = "Period",title = "Value vs fitted")
}





