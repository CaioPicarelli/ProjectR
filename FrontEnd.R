
library(shiny) # load shiny at beginning at both scripts
library(shinydashboard)
library(flexdashboard)

options(shiny.maxRequestSize=30*1024^2) 

ui <- dashboardPage(
  dashboardHeader(title = "Analysis"),
  
  dashboardSidebar(
    
    sidebarMenu(
      
      menuItem("Upload File",tabName = "csvUpload",icon = icon("upload")),
      menuItem("Data", tabName = "dataTable", icon = icon("table")),
      menuItem("Visualisation",tabName = "visualization", icon = icon("bar-chart")),
      menuItem("Model", tabName = "modelOutput", icon = icon("flask"))
      )),
      
  dashboardBody(
    
    tabItems(
      
    # CSV Upload page
    tabItem(tabName = "csvUpload",
            fluidPage(
              titlePanel("Uploading Files"),
              fluidRow(
                fileInput('file1','Choose CSV File',
                          accept = c('text/csv', 
                                     'text/comma-separated-values', 
                                     'text/plain',
                                     '.csv'))))),
    # Data table viewer
    tabItem(tabName = "dataTable",
            fluidPage(
              titlePanel("Uploaded CSV data"),
              dataTableOutput('CSVfigures'))),

    # Data visualizations page
    tabItem(tabName = "visualization",
            fluidPage(
              titlePanel("Data Visualisation"),
              fluidRow(
                box(plotOutput("explanatoryVariablesPlot")),
                box(plotOutput("explanatoryVariablesHistogram"))
              ),
              fluidRow(
                box(title = "",
                radioButtons("explanatory", "Explanatory Variables:", 
                             c("TV", "Radio", "OOH","Digital","Print")))))),
    
    # Model output page
    tabItem(tabName = "modelOutput",
            fluidPage(
              titlePanel("Model Outputs"),
              fluidRow(
                column(width = 12,
                  radioButtons("category", "Choose type of model:", 
                                   c("Cross-sectional", "Time series")))
              ),
              
              # Linear regression model output
              fluidRow(
                column(width = 12,
                       titlePanel("Linear Regression"),
                       "Model coefficients:",
                       tableOutput("linearRegressionCoef"),
                       "Spend Curve:",
                       plotOutput("linearRegressionSpendCurve"))
              ),
              
              # Ridge regression output
              fluidRow(
                column(width = 12,
                       titlePanel("Ridge Regression"))
              )
            ))
    
    )))
       

#===============================SERVER======================================================

'source("Curves.R")'
source("Models.R")

read.data <- function(inFile) {
  if (is.null(inFile)) 
    return(NULL)
  read.csv(inFile$datapath, header = T)
}

server <- function(input, output) {
  # Explanatory variables chart
  output$explanatoryVariablesPlot <- renderPlot({
    plot.explanatory.variable(
      read.data(input$file1),
      input$explanatory
    )
  })
  
  # Explanatory variables histogram
  output$explanatoryVariablesHistogram <- renderPlot({
    plot.explanatory.histogram(
      read.data(input$file1),
      input$explanatory
    )
  })
  
  # Data table
  output$CSVfigures <- renderDataTable({
    read.data(input$file1)
  })
  
  # Linear regression output
  output$linearRegressionCoef <- renderTable({
    linear.reg.coeffs(read.data(input$file1))
  })
  
  output$linearRegressionSpendCurve <- renderPlot({
    linear.reg.spend.curve(read.data(input$file1))
  })
}


shinyApp(ui, server)



#ISSUES:
# 
# input CSV has to be part of Global environment;
# Create a database? is it worth?






