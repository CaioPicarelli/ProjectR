
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
      menuItem("AdStocks", tabName = "dataTable2", icon = icon("table")),
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
    
    # Data table with adstocks
    tabItem(tabName = "adstocks",
            h4("Type values for Costs per GRP and Carry Over"),
            fluidRow(dataTableOutput("TS.Sales.Ads"))),

    # Data visualizations page
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
    
    # Model output page
    tabItem(tabName = "modelOutput",
            fluidPage(
              titlePanel("Model Outputs"),
              fluidRow(
                column(width = 12,
                  radioButtons("category", "Choose type of model:", 
                                   c("Cross-sectional", "Time series")))
              ),
              
              # input Cost per GRP and Carry Over
              fluidRow(
                tags$style("[type = 'number'] {font-size:15px;height:50px;}"),
                titlePanel("Insert Cost per GRPs and Carry Over"),
                column(3,
                      tags$style("[type ='text/css'] {font-size: 20px;}"),
                      numericInput("CGRP_TV",
                                    label = h5("TV"),
                                    value = ""),
                      numericInput("CGRP_Dig",
                                    label = h5("Digital"),
                                    value = ""),
                      numericInput("CGRP_Radio",
                                     label = h5("Radio"),
                                     value = ""),
                      numericInput("CGRP_OOH",
                                     label = h5("OOH"),
                                     value = ""),
                      numericInput("CGRP_Print",
                                   label = h5("Print"),
                                   value = "")),
                column(3,
                       tags$style("[type ='text/css'] {font-size: 20px;}"),
                       numericInput("co_TV",
                                    label = h5("TV"),
                                    value = ""),
                       numericInput("co__Dig",
                                    label = h5("Digital"),
                                    value = ""),
                       numericInput("co__Radio",
                                    label = h5("Radio"),
                                    value = ""),
                       numericInput("co__OOH",
                                    label = h5("OOH"),
                                    value = ""),
                       numericInput("co__Print",
                                    label = h5("Print"),
                                    value = ""))
                
                
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
  
  # Plot Linear regression on TS.Value against period
  output$LM.TS.SalesPeriod <- renderPlot({
    plot.LM.TS.period(read.data(input$file1))
  })
  
  # Pick Cost per GRPs and Carry Over for channels
  output$CostPerGRP <- renderText({
    CGRP_CO(read.data(input$CGRP_TV,input$CGRP_Radio,input$CGRP_OOH,
                      input$CGRP_Print,input$CGRP_Dig,input$co_TV,
                      input$co_Radio,input$co_OOH,input$co_Print,
                      input$co_Dig))
  })
  
  
}


shinyApp(ui, server)









