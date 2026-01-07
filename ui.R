#ui.R
library(MASS)
library(plotly)
library(shinyWidgets)
library(shinythemes)




shinyUI(fluidPage(theme = shinytheme("spacelab"),
  #tags$style(HTML("div#distPlot img {width: auto; height: auto; max-width: auto; max-height: auto;}")),
  titlePanel(div(img(src="PHO.png", height = 80, width = 240), "Regression with Ct values"),windowTitle = "ANALYSIS OF Ct"),
  
  withMathJax(),
  
  fluidRow(
    column(3,
           
        
           
           fileInput('file1', label = h5('Upload data in xlsx format:'),accept=c(".xlsx")),
           
           uiOutput("namesY"),
    
           uiOutput("namesD"),
           uiOutput("expLevels"),
           numericInput("decimal", h5("Decimal places to display:"), 2, min = 0, max = 10)
           #uiOutput("namesCT")
           #checkboxInput("manD", "Use dilution column in the data", FALSE)
           # 
           # conditionalPanel(
           #   condition = "input.manD #== false",
           #   
           #   
           #   numericInput("dilution", h5("Initial dilution in \\( \\log_{10} \\):"), -1, min = -10, max = 0)
           #   
           # ),
           # 
           # conditionalPanel(
           #   condition = "input.manD == true",
           #   uiOutput("namesD")
           # 
           #   ),
           # 
           # 
           # selectInput("p", h5("Percentile of interest:"),
           #             c(seq(0.01, 0.10, by= 0.01), seq(0.15, 0.85,by = 0.05),seq(0.9, 0.99, by= 0.01)),selected=0.95)
           # 
           ),
    column(9,
           
           
           
           mainPanel(
             
             tabsetPanel(
               
              
               tabPanel("Data", 
                       DT::dataTableOutput("table"),
                       value = "hide"),
       
                tabPanel("Variance by dilution", 
                         DT::dataTableOutput("table1"),
                         value = "hide"),
               # tabPanel("Plot",
               #          plotOutput("plot",height = "800px"),
               #          value = "hide"),

               tabPanel("Plotly",
                        plotlyOutput("plotly",height = "800px"),
                        value = "hide"),
              id="tabs")
             
           )#
           
           
           
           
           )
  )
))



