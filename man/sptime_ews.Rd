\name{sptime_ews}
\alias{sptime_ews}
\alias{sptime_ews,RasterStackBrickTS-method}
\alias{sptime_ews,SpatRasterTS-method}

\title{Spatio-temporal early warning signals of a critical transition}
\description{
This function generate early warning signals of critical transition using a novel local spatial autocorrelation approach.
}

\usage{
sptime_ews(x, detrend=NULL,bw=5, interpolate=NULL,k=6,rw=50)
}

\arguments{
  \item{x}{Raster Time Series object (created using the rts package)}
  \item{detrend}{whether detrending is applied, either NULL (for not detrending), or name of detrending method including: 'fdif','gaussian' }
  \item{bw}{a single numeric value specifies band width}
  \item{interpolate}{NULL (for no intepolation) or a single character specifying the interpolation method, either of "approx", or "ma" (moving average)}
  \item{k}{parameter}
  \item{rw}{rolling window size (default: 50)}
  }
  
\details{
  The \code{stew} function calculates spatial statistics and performs LISA analysis on each layer of the input raster stack or brick object. It internally uses the \code{.getLisa} function to compute the LISA results.

  The function supports different types of statistics and provides options for output types. If 'stat' is set to 'stg', 'geary', 'localgeary', or 'c', the function calculates the Getis-Ord G statistic. If 'stat' is set to 'stm', 'moran', 'localmoran', 'm', or 'i', the function calculates the Moran's I statistic. 

  The output types include:
  \describe{
    \item{'raster'}{Returns a modified raster object with LISA analysis results for each layer.}
    \item{'rts'}{Returns a raster time series object.}
    \item{'tau'}{Calculates the Kendall's Tau spatial autocorrelation measure.}
  }
}
\value{
  The \code{stew} function returns the calculated spatial statistics and LISA analysis results based on the specified output types.
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

#plot(spe[[1:4]])

}


}
