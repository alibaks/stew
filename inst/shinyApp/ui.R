library(shiny)
library(shinydashboard)
library(shinyjs)
library(elsa)
library(stew)
library(raster)
library(terra)
library(earlywarnings)
library(bfast)




ui <- dashboardPage(
  dashboardHeader(title = "NDVI Analysis App"),
  dashboardSidebar(
    fileInput("ndviFile", "Choose NDVI File"),
    dateRangeInput("dateRange", "Choose Date Range", 
                   start = as.Date("2005-01-01"), end = as.Date("2013-12-31")),
    numericInput("pv_sig", "Enter pv_sig for Stew:", value = 0.05, min = 0, max = 1, step = 0.01),
    numericInput("hh", "Enter value for 'h' in bfast:", value = 1, min = 1),
    actionButton("calculateStew", "Calculate Stew")
  ),
  dashboardBody(
    fluidRow(
      box(
        title = "NDVI Plot",
        plotOutput("plotA", click = "plotA_click"),
        verbatimTextOutput("clickedCoordinates"), 
        width = 6
      ),
      box(
        title = "Extracted Values",
        plotOutput("plotExtractedValues"),
        verbatimTextOutput("extractedValues"),
        downloadButton("downloadCSV", label = "Download Extracted Values", icon = icon("download"), class = "btn-success"),
        width = 6
      )
    ),
    fluidRow(
      box(
        title = "Stew Plot",
        plotOutput("plotStew"),
        width = 6
      ),
      box(
        title = "Early Warning Signal Plot",
        plotOutput("plotEarlyWarning"),
        width = 6
      )
    ),
    fluidRow(
      box(
        title = "BFAST Fit Plot",
        plotOutput("fit"),  
        width = 6
      )
    ),
    fluidRow(
      box(
        title = "BFAST Fit Trend Plot",
        plotOutput("plotBFAST_Trend"),
        width = 6
      ),
      box(
        title = "BFAST Fit All Plots",
        plotOutput("plotBFAST_All"),
        width = 6
      )
    ),
    fluidRow(
      box(
        title = "BFAST Fit Output",
        verbatimTextOutput("fitOutput"),
        width = 6
      )
    )
  )
)


