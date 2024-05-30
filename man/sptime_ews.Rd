\name{sptime_ews}
\alias{sptime_ews}
\alias{sptime_ews,RasterStackBrickTS-method}
\alias{sptime_ews,SpatRasterTS-method}

\title{Spatio-temporal Early Warning Signals of a Critical Transition}

\description{
  The \code{sptime_ews} function provides tools for analyzing spatio-temporal patterns in raster time series data to detect early warning signals of critical transitions.
}

\usage{
  \method{sptime_ews}{RasterStackBrickTS}(x, detrend = NULL, bw = 5, interpolate = NULL, k = 6, rw = 50)
  \method{sptime_ews}{SpatRasterTS}(x, detrend = NULL, bw = 5, interpolate = NULL, k = 6, rw = 50)
}

\arguments{
  \item{x}{A raster time series object of class \code{RasterStackBrickTS} or \code{SpatRasterTS}.}

  \item{detrend}{Optional. Method for detrending the data. Options include \code{"gaussian"}, \code{"fdif"}, or \code{NULL} for no detrending.}

  \item{bw}{Bandwidth for detrending. Default is 5.}

  \item{interpolate}{Optional. Interpolation method for handling missing values. Options include \code{"approx"}, \code{"ma"}, or \code{NULL} for no interpolation.}

  \item{k}{Parameter for interpolation methods. Default is 6.}

  \item{rw}{Size of the rolling window for analysis. Default is 50.}
}

\details{
  The function employs various detrending and interpolation methods to calculate autocorrelation and detect early warning signals of critical transitions in spatio-temporal raster time series data.


The output types include:
  \describe{
    \item{'raster'}{Returns a modified raster object with LISA analysis results for each layer.}
    \item{'rts'}{Returns a raster time series object.}
    \item{'tau'}{Calculates the Kendall's Tau spatial autocorrelation measure.}
  }
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

file <- system.file("external/ndvi.tif", package="stew") # path to ndvi dataset


# Read raster object
r <- rast(file)
r

# in order to make a raster time series object, you need the corresponsing date 
# for raster layers:

d <- seq(as.Date("2000-01-03"),as.Date("2019-01-01"),length.out = nlyr(r))

head(d) # date object with the length of 229 corresponding to 229 layers in r 

rt <- rts(r, d) # create raster time series object

rt

# Calculate spatio-temporal early warning signals:


spe <- sptime_ews(rt, detrend=NULL,bw=5, interpolate=NULL,k=6,rw=50)

plot(spe)

}


}
