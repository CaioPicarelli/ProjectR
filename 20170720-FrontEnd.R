
library(shiny) # load shiny at beginning at both scripts
library(shinydashboard)
library(flexdashboard)

options(shiny.maxRequestSize=30*1024^2) 

ui <- dashboardPage(
  dashboardHeader(title = "Analysis"),
  
  dashboardSidebar(
    
    sidebarMenu(
      
      menuItem("Upload File",tabName = "csvUpload",icon = icon("upload")),
      menuItem("Data", tabName = "dashboard", icon = icon("dashboard")),
      menuItem("Model", tabName = "widgets", icon = icon("th")),
      menuItem("Variables",tabName = "Test",icon = icon("euro")),
      menuItem("Table",tabName = "table",icon = icon("table"))
      
      )),
      
  dashboardBody(
    
    tabItems(
      
    tabItem(tabName = "csvUpload",
            fluidPage(
              titlePanel("Uploading Files"),
              sidebarLayout(
                sidebarPanel(
                  fileInput('file1','Choose CSV File',accept = c('text/csv',
                                                                'text/comma-separated-values,
                                                                text/plain',
                                                                '.csv')),
                  radioButtons("UserChoice","Choose Model Dickhead",
                               c('Price and Promotions',
                                 'MMM'))),
            mainPanel()
            ))),
            
            
    tabItem(tabName = "dashboard",
            fluidPage(
              titlePanel("Uploaded Data"),
              fluidRow(
                box(plotOutput("explanatoryVariablesPlot", height = 400)),
                box(title = "",
                radioButtons("explanatory", "Explanatory Variables:", 
                             c("TV", "Radio", "OOH")))))),
    
    tabItem(tabName = "table",
            fluidPage(
              dataTableOutput('CSVfigures'))))))
       

#===============================SERVER======================================================

source("20170720-Curves.R")

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
  
  # Data table
  output$CSVfigures <- renderDataTable({
    read.data(input$file1)
  })
}


shinyApp(ui, server)



#ISSUES:
# 
# input CSV has to be part of Global environment;
# Create a database? is it worth?






