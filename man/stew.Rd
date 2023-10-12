\name{stew}
\alias{stew}
\alias{stew,RasterStackBrickTS-method}
\alias{stew,SpatRasterTS-method}

\title{Spatio-temporal early warning signals of a critical transition}
\description{
This function generate early warning signals of critical transition using a novel local spatial autocorrelation approach.
}
\usage{
  stew(x, stat, d, output, pv_sig, ...)
}
\arguments{
  \item{x}{\code{RasterStackBrickTS} object. The raster stack or brick to be analyzed.}

  \item{stat}{\code{character}. The statistic to compute for LISA analysis. Options include 'stg', 'geary', 'localgeary', 'c', 'stm', 'moran', 'localmoran', 'm', or 'i'. Default is 'stg'.}

  \item{d}{\code{numeric}. The distance parameter for LISA analysis. Default is NULL, which sets it to maximum resolution * sqrt(2).}

  \item{output}{\code{character}. Desired output types. Options include 'raster', 'rts', and 'tau'. You can specify multiple types by providing a vector. Default is 'raster'.}
  
  \item{pv_sig}{\code{numeric or NULL}. specifies the p_value (e.g., pv_sig = 0.05) greater than which the calculated kandal tau is not significant and therefore, NA is assigned to the corresponding pixel. If NULL (default), the tau values for all pixels are returned even for non-significant trends}
  

  \item{...}{Additional arguments passed to internal functions.}
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
library(raster)

# Create a sample raster stack
r1 <- raster(matrix(runif(100), 10, 10))
r2 <- raster(matrix(runif(100), 10, 10))
s <- stack(r1, r2)
rts_obj <- rts(x = s, time = as.Date(c("2023-01-01", "2023-02-01")))

# Calculate results using the stew function


result_raster <- stew(rts_obj, stat = "stg", output= "raster")
plot(result_raster)

result_rts <- stew(rts_obj, stat = "stg", output= "rts")
plot(result_rts)
}

}
