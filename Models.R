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

