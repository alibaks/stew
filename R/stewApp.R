
#--------

if (!isGeneric("stewApp")) {
  setGeneric("stewApp", function(x="ANY")
    standardGeneric("stewApp"))
}


setMethod('stewApp', signature(),
          function(x) {
            
            shiny::runApp(system.file("shinyApp/",package = "stew"))
          }
)
