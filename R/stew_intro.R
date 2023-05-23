#' @export
stew_intro <- function() {
  message("Welcome to the stew package!")
  message("================================")
  message("stew: Spatial Weighted Statistics")
  message("================================")
  message("The stew package provides functions for spatial weighted statistics.")
  message("It allows you to perform LISA analysis and other spatial statistics on raster data.")
  message("To get started, use the stew function and explore the various options.")
  message("For more information and usage examples, please refer to the package documentation.")
}

# Call the welcome message function when the package is loaded
.onAttach <- function(libname, pkgname) {
  stew_intro()
  invisible()
}