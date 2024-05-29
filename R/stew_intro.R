
.stew_intro <- function() {
  message("Welcome to the stew package!")
  message("================================")
  message("stew: Spatial Weighted Statistics")
  message("================================")
  message("The stew package provides functions for spatial weighted statistics.")
  message("It allows you to perform LISA analysis and other spatial statistics on raster data.")
  message("To get started, use the stew function and explore the various options.")
  message("For more information and usage examples, please refer to the package documentation.")
  message("Citations:
  
Alibakhshi, S. (2023). A robust approach and analytical tool for identifying early warning signals of forest mortality, Ecological Indicators. 155.

Alibakhshi, S., Groen, T. A.,Rautiainen, M, and Naimi, B (2017). Remotely-sensed early warning signals of a critical transition in a wetland ecosystem. Remote Sensing 9(4): 352-352.

Alibakhshi, S., T. A. Groen, M. Rautiainen and B. Naimi (2017). Remotely-sensed early warning signals of a critical transition in a wetland ecosystem. Remote Sensing 9(4): 352-352.
}

# Call the welcome message function when the package is loaded
.onAttach <- function(libname, pkgname) {
  .stew_intro()
  invisible()"
  )}
