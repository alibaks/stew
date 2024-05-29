\name{stew}
\alias{stew}
\alias{stew,RasterStackBrickTS-method}
\alias{stew,SpatRasterTS-method}

\title{Spatio-temporal early warning signals of a critical transition}
\description{
This function generate early warning signals of critical transition using a local spatial autocorrelation approach.
}
\usage{
  \method{stew}{RasterStackBrickTS}(x, stat = "stg", d = NULL, output = "rts", pv_sig = NULL, ...)
  \method{stew}{SpatRasterTS}(x, stat = "stg", d = NULL, output = "rts", pv_sig = NULL, ...)
}

\arguments{
  \item{x}{A raster time series object of class \code{RasterStackBrickTS} or \code{SpatRasterTS}.}

  \item{stat}{The spatial autocorrelation statistic to use (\code{"stg"} for Geary's C, \code{"moran"} for Moran's I).}

  \item{d}{Spatial weights matrix.}

  \item{output}{\code{character}. Desired output types. Options include 'raster', 'rts', and 'tau'. You can specify multiple types by providing a vector. Default is 'raster'.}
  
  \item{pv_sig}{\code{numeric or NULL}. specifies the p_value (e.g., pv_sig = 0.05) greater than which the calculated kandal tau is not significant and therefore, NA is assigned to the corresponding pixel. If NULL (default), the tau values for all pixels are returned even for non-significant trends}
  
  \item{...}{Additional parameters to be passed.}
}

\details{
  The function applies Moran's I or Geary's C on each raster layer in a time series to evaluate spatial trends. It can output raster layers, raster time series, and Kendall's Tau values.
}
  The output types include:
  \describe{
    \item{'raster'}{Returns time series of input rasters.}
    \item{'rts'}{Returns time series of output raster.}
    \item{'tau'}{Calculates the Kendall's Tau spatial autocorrelation measure.}
  }
}
\value{
  The \code{stew} function returns the calculated spatial statistics and trend analysis results based on the specified output types.
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
\dontrun{
# Load the stew package
library(stew)

# File path to the ndvi dataset
file <- system.file("external/ndvi.tif", package="stew")

# Read raster object
r <- rast(file)

# In order to create a raster time series object, you need the corresponding dates
d <- seq(as.Date("2000-01-03"), as.Date("2019-01-01"), length.out = nlyr(r))

# Create raster time series object
rt <- rts(r, d)

# Calculate spatio-temporal early warning signals
spe <- stew(rt, stat = "moran", output = c("rts", "tau"), pv_sig = 0.05)

# Plot the results (uncomment the line below to plot)
# plot(spe$tau)
}
}
}