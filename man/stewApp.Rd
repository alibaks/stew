\name{stewApp}
\alias{stewApp}
\alias{stewApp,Any-method}

\title{Launch the Stew Shiny Application}
\description{
Launches the Stew Shiny application for spatio-temporal analysis, incorporating functionalities from the 'earlywarnings' and 'bfast' packages.
}
\usage{
stewApp(x = "ANY")
}
\arguments{
  \item{x}{A parameter to ensure generic method functionality.}
}
\details{
The \code{stewApp} function initializes and runs a Shiny application designed for analyzing time series of raster data. This application uses the 'earlywarnings' and 'bfast' packages to provide advanced analysis features such as early warning signals and breakpoint detection in time-series data.

\strong{Key Features:}
\itemize{
  \item \strong{Time series of data input:} Upload and visualize raster data (raster brick or raster stack) files.
  \item \strong{Coordinate Selection:} Interactive plotting allows users to click on the time series of data plot to extract and display NDVI values for specific geographic locations.
  \item \strong{Early Warning Signals:} Calculate and visualize early warning signals using the \code{qda_ews} function from the 'earlywarnings' package.
  \item \strong{BFAST Analysis:} Perform BFAST (Breaks For Additive Season and Trend) analysis to detect breakpoints and trends in the time series using the \code{bfast} function.
  \item \strong{Stew Calculation:} Compute the Stew statistic for the time series of data to identify significant changes over time.
}
}
\value{
Runs the application for time series of data analyses.
}

\seealso{
\code{\link[shiny]{runApp}}, \code{\link[earlywarnings]{qda_ews}}, \code{\link[bfast]{bfast}}
}
\author{
Sara Alibakhshi
 
Department of Geosciences and Geography, University of Helsinki, Finland
 
sara.alibakhshi@gmail.com
}

\references{
Alibakhshi, S. (2023). A robust approach and analytical tool for identifying early warning signals of forest mortality, Ecological Indicators. 155.


Alibakhshi, S., Groen, T. A.,Rautiainen, M, and Naimi, B (2017). "Remotely-sensed early warning signals of a critical transition in a wetland ecosystem." Remote Sensing 9(4): 352-352.

}

\examples{
if (interactive()) {
  stewApp()
}
}
