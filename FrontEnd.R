
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
      menuItem("AdStocks", tabName = "adstocks", icon = icon("table")),
      menuItem("Visualisation",tabName = "visualization", icon = icon("bar-chart")),
      menuItem("Model", tabName = "modelOutput", icon = icon("flask"))
      
      )),
      
  dashboardBody(
    
    tabItems(
      
    # CSV Upload page======================================================
    tabItem(tabName = "csvUpload",
            fluidPage(
              titlePanel("Uploading Files"),
              fluidRow(
                fileInput('file1','Choose CSV File',
                          accept = c('text/csv', 
                                     'text/comma-separated-values', 
                                     'text/plain',
                                     '.csv'))),
    
              # Selection of Model
              fluidRow(
                column(width = 12,
                       radioButtons("category", "Choose type of model:", 
                                    c("Cross-sectional Sales", "Time series Sales",
                                      "Times Series Price and Promotions")))))
            ),
    
    # Data table viewer======================================================
    tabItem(tabName = "dataTable",
            fluidPage(
              titlePanel("Uploaded CSV data"),
              dataTableOutput('CSVfigures'))),
    
    # Data table with adstocks======================================================
    tabItem(tabName = "adstocks",
            fluidPage(
              titlePanel("Transformation from Costs to Adstocks"),
              
              # input Cost per GRP and Carry Over
              fluidRow(
                tags$style("[type = 'number'] {font-size:15px;height:50px;}"),
                titlePanel("Insert Cost per GRPs and Carry Over"),
                column(3,
                       tags$style("[type ='text/css'] {font-size: 20px;}"),
                       numericInput("CGRP_TV",
                                    label = h5("TV"),
                                    value = 1500),
                       numericInput("CGRP_Dig",
                                    label = h5("Digital"),
                                    value = 1300),
                       numericInput("CGRP_Radio",
                                    label = h5("Radio"),
                                    value = 350),
                       numericInput("CGRP_OOH",
                                    label = h5("OOH"),
                                    value = 500),
                       numericInput("CGRP_Print",
                                    label = h5("Print"),
                                    value = 250)),
                column(3,
                       tags$style("[type ='text/css'] {font-size: 20px;}"),
                       numericInput("co_TV",
                                    label = h5("TV"),
                                    value = 0.55),
                       numericInput("co_Dig",
                                    label = h5("Digital"),
                                    value = 0.67),
                       numericInput("co_Radio",
                                    label = h5("Radio"),
                                    value = 0.67),
                       numericInput("co_OOH",
                                    label = h5("OOH"),
                                    value = 0.55),
                       numericInput("co_Print",
                                    label = h5("Print"),
                                    value = 0.66))
                
                
              ),
              
              # New data frame with AdStocks
              fluidRow(
              h4("Data with Adstocks"),
              dataTableOutput("Adstock")))
              ),
              

    # Data visualizations page======================================================
    tabItem(tabName = "visualization",
            fluidPage(
              titlePanel("Data Visualisation"),
              fluidRow(
                box(plotOutput("explanatoryVariablesPlot")),
                box(plotOutput("TS.SalesPeriod")),
                box(plotOutput("explanatoryVariablesHistogram"))
              ),
              fluidRow(
                box(title = "",
                radioButtons("explanatory", "Explanatory Variables:", 
                             c("TV", "Radio", "OOH","Digital","Print")))))),
    
    # Model output page======================================================
    tabItem(tabName = "modelOutput",
            fluidPage(
              titlePanel("Model Outputs"),
              
              
              # Linear regression model output
              fluidRow(
                column(width = 12,
                       titlePanel("Linear Regression"),
                       "Model coefficients:",
                       dataTableOutput("LM.TS"),
                       "Predicted:",
                       plotOutput("LM.pred"))
              ),
              
              # Ridge regression output
              fluidRow(
                column(width = 12,
                       titlePanel("Ridge Regression"))),
              fluidRow(
                titlePanel("Predicticted"),
                plotOutput("")
              )
            ))

    )))
       
#===============================SERVER======================================================

'source("Curves.R")'
source("Models.R")

read.data <- function(inFile) {
  if (is.null(inFile)) 
    return(NULL)
  data <- read.csv(inFile$datapath, header = T)
  if("Period" %in% names(data)) {
    data$Period <- as.Date(data$Period, format="%d/%m/%Y")
  }
  
  return(data)
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
  
  # Data table as Input
  output$CSVfigures <- renderDataTable({
    read.data(input$file1)
  })
  
  # Data table with Adstocks
  output$TS.Sales.Ads <- renderDataTable({
    read.data(input$TS.Sales.Ads)
  })
  
  # Linear regression output
  output$linearRegressionCoef <- renderTable({
    linear.reg.coeffs(read.data(input$file1))
  })
  
  output$linearRegressionSpendCurve <- renderPlot({
    linear.reg.spend.curve(read.data(input$file1))
  })
  
  # Plot TS.Value against period
  output$TS.SalesPeriod <- renderPlot({
    plot.TS.period(read.data(input$file1))
  })
  
  # Pick Cost per GRPs and Carry Over for channels
  output$Adstock <- renderDataTable({
    TS.Ads.CO(read.data(input$file1), input)
  }, options = list(scrollX = TRUE))
  
  # Run Linear regression on TS.Value against period
  output$LM.TS<- renderDataTable({
    LM(read.data(input$file1), input)
  })
  
  # Plot LM predictions
  output$LM.pred<- renderPlot({
    plot.LM(read.data(input$file1), input)
  })
  
  
}

shinyApp(ui, server)









