
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
              titlePanel("Chart"),
              fluidRow(
                box(plotOutput("plot1", height = 250)),
                box(title = "",
                sliderInput("slider", "Number of Weeks:", 1, 1000, 50))))),
    
    tabItem(tabName = "table",
            fluidPage(
              dataTableOutput('CSVfigures'))))))
       

#===============================SERVER======================================================


server <- function(input, output) {
  output$plot1 <- renderPlot({Chart1})
  
  output$CSVfigures <- renderDataTable({
    inFile <- input$file1

    if (is.null(inFile)) 
      return(NULL)
    read.csv(inFile$datapath, header = T)
    
    # CSV_Sales_Model <- read.csv(inFile$datapath)
    # assign('data',CSV_Sales_Model,envir = .GlobalEnv)
    
    })
}


shinyApp(ui, server)



#ISSUES:
# 
# input CSV has to be part of Global environment;
# Create a database? is it worth?






