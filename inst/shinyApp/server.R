

server <- function(input, output, session) {
  clickedPoint <- reactiveValues(x = NULL, y = NULL)
  
  ndviData <- reactive({
    req(input$ndviFile)
    brick(input$ndviFile$datapath)
  })
  
  nt <- reactive({
    if (!is.null(ndviData())) {
      start_date <- input$dateRange[1]
      end_date <- input$dateRange[2]
      d <- seq(start_date, end_date, length.out = nlayers(ndviData()))
      rts(ndviData(), d)
    }
  })

    output$plotA <- renderPlot({
    if (!is.null(ndviData()))
      plot(ndviData()[[3]])
  })
  
  observe({
    shinyjs::enable("plotA")
  })
  
  observeEvent(input$plotA_click, {
    clickedPoint$x <- input$plotA_click$x
    clickedPoint$y <- input$plotA_click$y
    

        output$clickedCoordinates <- renderPrint({
      paste("Clicked Coordinates: (", round(clickedPoint$x, 2), ", ", round(clickedPoint$y, 2), ")", sep = "")
    })
    

    expectedValues <- extract(nt(), cellFromXY(nt(),cbind(clickedPoint$x, clickedPoint$y)))
    output$extractedValues <- renderPrint({
      expectedValues
    })

    output$plotExtractedValues <- renderPlot({
      plot(expectedValues, col = "blue", lwd = 2, xlab = "Time", ylab = "NDVI Value")
    })
    
   
    output$downloadCSV <- downloadHandler(
      filename = function() {
        paste("expectedValues_", Sys.Date(), ".csv", sep = "")
      },
      content = function(file) {
        exp=as.matrix(expectedValues)
        write.csv(exp, file)
      }
    )
    

      earlyWarningSignal <- reactive({
      qda_ews(expectedValues, param = NULL, winsize = 50,
              detrending='gaussian', bandwidth=NULL,
              boots = 10, s_level = 0.05, cutoff=0.05,
              detection.threshold = 0.002, grid.size = 50,
              logtransform=FALSE, interpolate=T)
    })
    
    output$plotEarlyWarning <- renderPlot({
      if (!is.null(earlyWarningSignal())) {
        earlyWarningSignal()
      }
    })
    
    
    
    bfastFit <- reactive({

      .ind <- index(expectedValues)
      ee <- ts(as.numeric(na.omit(expectedValues[,1])),start = .ind[1],frequency = 12)
      bfast(ee)
      
      
    })
    
    
    
    output$fit <- renderPlot({
      plot(bfastFit())
    })
    
    
    output$plotBFAST_Trend <- renderPlot({
      plot(bfastFit(), type = "trend", largest = TRUE)
      
    })
    
    output$plotBFAST_All <- renderPlot({
      plot(bfastFit(), type = "all")
      
    })
    
    output$fitOutput <- renderPrint({
      df
    })
    
  })
  observeEvent(input$calculateStew, {
    stewOutput <- reactive({
      if (!is.null(nt())) {
        st <- stew(nt(), output = c('tau'), pv_sig = input$pv_sig)
        st
      }
    })
    
    output$plotStew <- renderPlot({
      if (!is.null(stewOutput())) {
        plot(stewOutput())
      }
    })
  })
}



