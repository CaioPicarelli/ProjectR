
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
                                      "Price and Promotions")))))
            ),
    
    # Data table viewer======================================================
    tabItem(tabName = "dataTable",
            fluidPage(
              titlePanel("Uploaded CSV data"),
              dataTableOutput('CSVfigures'))),
    
    # Data table with adstocks======================================================
    tabItem(tabName = "adstocks",
            fluidPage(
              conditionalPanel(
                condition = "input.category == 'Time series Sales'",
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
              )),
              

    # Data visualizations page======================================================
    tabItem(tabName = "visualization",
            fluidPage(
              titlePanel("Data Visualisation"),
              conditionalPanel(
                condition = "input.category == 'Time series Sales'",
                h3("Value and TV"),
                plotOutput("TS.Sales.TV"),
                h3("Value and Radio"),
                plotOutput("TS.Sales.Radio"),
                h3("Value and Print"),
                plotOutput("TS.Sales.Print"),
                h3("Value and OOH"),
                plotOutput("TS.Sales.OOH"),
                h3("Value and Digital"),
                plotOutput("TS.Sales.Digital")
              ),
              
              conditionalPanel(
                condition = "input.category == 'Cross-sectional Sales'",
                box(title="",
                radioButtons("CS.explanatory", "Pick one Channel:", 
                             c("TV", "Radio", "OOH","Digital","Print"))),
                title = "Plot of Values and Media Spends",
                box(plotOutput("CS.Spends.Brand")),
                titlePanel("Uploaded CSV data"),
                dataTableOutput('CStotalSpends'),
                h3("Spend per Brand"),
                plotOutput("plot.spend.brand"),
                h3("Spend - Value Ratio"),
                plotOutput("plot.spend.value.ratio"),
                h3("Share of Market vs Share of Spends"),
                plotOutput("plot.som.sos")),
              
              conditionalPanel(
                condition = "input.category == 'Price and Promotions'",
                titlePanel("Visualization Price and Promotions"),
                plotOutput("PP.hist.p1"),
                plotOutput("PP.hist.p2"),
                plotOutput("bystore1"),
                plotOutput("bystore2"),
                plotOutput("plot.p1.sales.period"),
                plotOutput("plot.p1.sales.price.period"),
                plotOutput("p1.sales.prom.box")
                
              ))),
    
    
    # Model output page======================================================
    tabItem(tabName = "modelOutput",
            fluidPage(
              titlePanel("Model Outputs"),
              
              
              # Linear regression model output
              conditionalPanel(
                  condition = "input.category == 'Time series Sales'",
                  column(width = 12,
                   h3("Linear Model"),
                   dataTableOutput("LM.TS"),
                   dataTableOutput("TS.OLS.stats"),
                   h3("Predicted vs Observed Sales Value: "),
                   plotOutput("LM.pred"),
                   h3("Linear Model with Dummies"),
                   dataTableOutput("OLS.du_TS"),
                   dataTableOutput("OLS.d.stats"),
                   h3("Spends Level for Curves"),
                   dataTableOutput("SpendSlopeTS"),
                   h3("Curves at Channel Level"),
                   plotOutput("curvesPlot"))
              ),
              
              # CS Linear Model with sum of Spends
              conditionalPanel(
                  condition = "input.category == 'Cross-sectional Sales'",
                  column(width = 12,
                  titlePanel("Linear Regression with all Spends"),
                  dataTableOutput("LM.CS"),
                  dataTableOutput("cs.stats"),
                  dataTableOutput("CS.log"),
                  dataTableOutput("cs.log.stats"),
                  textInput("selectedBrand",label = "Type Brand Name: ",value = "Client Name"),
                  plotOutput("cs.log.plot"),
                  plotOutput("CS.boxwhisker"))),
            
              conditionalPanel(
                  condition = "input.category == 'Price and Promotions'",
                  column(width=12,
                  h3("GLM Model Promotions ~ Sales"),
                  dataTableOutput("PP.glm"),
                  numericInput("PP.Value",
                  h5("Predict Value of "),value = 150),
                  h5("...has probability of "),
                  dataTableOutput("PP.model.prob"),
                  h3("Fitted vs Sales"),
                  plotOutput("PP.model.plot"),
                  h3("Log-Log Model Sales and Price for P1"),
                  dataTableOutput("PP.price.model"),
                  dataTableOutput("PP.price.model.Stats")))
                
              )
            )



# Close bracket of UI:
  )))
       
#===============================SERVER======================================================

# Call file where functions are run
source("Functions.R")

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
  
  
  # Data table as Input
  output$CSVfigures <- renderDataTable({
    read.data(input$file1)
  },options = list(scrollX = TRUE))
  
  # ================================= Time Series Sales Edit=================== 

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
  
  # Plot TS.Value against Media
  output$TS.Sales.TV <- renderPlot({
    if (input$category == "Time series Sales") {
      plot.TS.TV(read.data(input$file1))
    }
  })
  
  # Plot TS.Value against Media
  output$TS.Sales.Radio <- renderPlot({
    if (input$category == "Time series Sales") {
      plot.TS.Radio(read.data(input$file1))
    }
  })
  
  # Plot TS.Value against Media
  output$TS.Sales.Print <- renderPlot({
    if (input$category == "Time series Sales") {
      plot.TS.Print(read.data(input$file1))
    }
  })
  
  # Plot TS.Value against Media
  output$TS.Sales.OOH <- renderPlot({
    if (input$category == "Time series Sales") {
      plot.TS.OOH(read.data(input$file1))
    }
  })
  
  # Plot TS.Value against Media
  output$TS.Sales.Digital <- renderPlot({
    if (input$category == "Time series Sales") {
      plot.TS.Digital(read.data(input$file1))
    }
  })
  
  # Pick Cost per GRPs and Carry Over for channels
  output$Adstock <- renderDataTable({
    if (input$category == "Time series Sales") {
      TS.Ads.CO(read.data(input$file1), input)
    }
  },
  options = list(scrollX = TRUE))
  
  # Run Linear regression on TS.Value against period
  output$LM.TS<- renderDataTable({
    if (input$category == "Time series Sales") {
      LM(read.data(input$file1), input)
    }
  })
  
  # Calculate MSE for LM
  output$TS.OLS.stats<- renderDataTable({
    if (input$category == "Time series Sales") {
      MSE.Rsq.LM(read.data(input$file1), input)
    }
  })
  
  # Plot LM predictions
  output$LM.pred<- renderPlot({
    if (input$category == "Time series Sales") {
      plot.LM(read.data(input$file1), input)
    }
  })
  
  # Check Spend Slope data for TS
  output$SpendSlopeTS <- renderDataTable({
    if (input$category == "Time series Sales") {
      SpendSlope_TS(read.data(input$file1), input)
    }
  })
  
  # Check OLS for TS with Dummies
  output$OLS.du_TS <- renderDataTable({
    if (input$category == "Time series Sales") {
      TS.OLS.du(read.data(input$file1), input)
    }
  })
  
  
  # Check OLS for TS with Dummies
  output$OLS.d.stats <- renderDataTable({
    if (input$category == "Time series Sales") {
      MSE.Rsq.LM.du(read.data(input$file1), input)
    }
  })
  
  # GGplot with Curves
  output$curvesPlot <- renderPlot({
    if (input$category == "Time series Sales") {
      Curves(read.data(input$file1), input)
    }
  })
  
# ================================= Cross Sectional Sales Edit===================  
  
  # Plot CS Media Spends
  output$CS.Spends.Brand<- renderPlot({
    if (input$category == "Cross-sectional Sales") {
      CS.Spends(read.data(input$file1), input$CS.explanatory)
    }
  })
  
  # Get total spends across channels
  output$CStotalSpends <- renderDataTable({
    if (input$category == "Cross-sectional Sales") {
      CS.SUM.Spends(read.data(input$file1))
    }
  },options = list(scrollX = TRUE))
  
  # OLS model with CS and Spends
  output$LM.CS <- renderDataTable({
    if (input$category == "Cross-sectional Sales") {
      CS.lm(read.data(input$file1))
    }
  })

  # Stats from OLS LM model stats
  output$cs.stats <- renderDataTable({
    if (input$category == "Cross-sectional Sales") {
      CS.lm.MSE.R(read.data(input$file1))
    }
  }) 
  
    # Log-Log OLS model with CS and Spends
    output$CS.log <- renderDataTable({
      if (input$category == "Cross-sectional Sales") {
        CS.log.lm(read.data(input$file1))
      }
  })

    # Stats from Log-Log OLS model
    output$cs.log.stats <- renderDataTable({
      if (input$category == "Cross-sectional Sales") {
        CS.log.MSE.R(read.data(input$file1))
      }
    }) 
    
    # CS-CURVE - GGPLOT log-log Value and Spends
    output$cs.log.plot <- renderPlot({
      if (input$category == "Cross-sectional Sales") {
        CS.ggplot.line(read.data(input$file1), input$selectedBrand)
      }
    })
    
    # Plot box and whisker 
    output$CS.boxwhisker<- renderPlot({
      if (input$category == "Cross-sectional Sales") {
        boxnwhisker(read.data(input$file1))
      }
    })
    
    # Plot Spend vs Brands
    output$plot.spend.brand<- renderPlot({
      if (input$category == "Cross-sectional Sales") {
        CS.Spend.Brand(read.data(input$file1))
      }
    })
    
    # Plot Ratio of Spends and Sales Value
    output$plot.spend.value.ratio<- renderPlot({
      if (input$category == "Cross-sectional Sales") {
        CS.Spend.Sales.Ratio(read.data(input$file1))
      }
    })
    
    
    # Plot sos vs som
    output$plot.som.sos<- renderPlot({
      if (input$category == "Cross-sectional Sales") {
        CS.SOS.SOM(read.data(input$file1))
      }
    })

  
  # ===========================Price and Promotions Edit=============================
  
  # Price and Promotions histogram for product 1
  output$PP.hist.p1 <- renderPlot({
    if (input$category == "Price and Promotions") {
      hist.p1(read.data(input$file1))
    }
  })
  
  # Price and Promotions histogram for product 2
  output$PP.hist.p2 <- renderPlot({
    if (input$category == "Price and Promotions") {
      hist.p2(read.data(input$file1))
    }
  })
  
  # Price and Promotions distribution by shop - 1
  output$bystore1 <- renderPlot({
    if (input$category == "Price and Promotions") {
      store.dist1((read.data(input$file1)))
    }
  })
  
  # Price and Promotions distribution by shop - 2
  output$bystore2 <- renderPlot({
    if (input$category == "Price and Promotions") {
      store.dist2(read.data(input$file1))
    }
  })
  
  # p1 sales per period
  output$plot.p1.sales.period <- renderPlot({
    if (input$category == "Price and Promotions") {
      p1.sales.period(read.data(input$file1))
    }
  })
  
  # p1 sales vs price per period
  output$plot.p1.sales.price.period <- renderPlot({
    if (input$category == "Price and Promotions") {
      p1.sales.price.period(read.data(input$file1))
    }
  })
  
  # p1 sales vs prom box
  output$plot.p1.sales <- renderPlot({
    if (input$category == "Price and Promotions") {
      p1.sales.prom.box(read.data(input$file1))
    }
  })
  
  # GLM Promotions ~ Sales
  output$PP.glm <- renderDataTable({
    if (input$category == "Price and Promotions") {
      PP.model(read.data(input$file1))
    }
  })
  
  # PP Prediction Probability
  output$PP.model.prob <- renderDataTable({
    if (input$category == "Price and Promotions") {
      PP.Prediction(read.data(input$file1),input)
    }
  })
  
  # PP Prediction chart
  output$PP.model.plot <- renderPlot({
    if (input$category == "Price and Promotions") {
      PP.Prediction.chart(read.data(input$file1))
    }
  })
  
  # log-log Sales ~ price
  output$PP.price.model <- renderDataTable({
    if (input$category == "Price and Promotions") {
      PP.model.price(read.data(input$file1))
    }
  })
  
  # log-log Sales ~ price Stats
  output$PP.price.model.Stats <- renderDataTable({
    if (input$category == "Price and Promotions") {
      PP.model.price.stats(read.data(input$file1))
    }
  })

  
  
  
}

shinyApp(ui, server)









