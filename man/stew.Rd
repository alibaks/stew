\name{stew}
\alias{stew}
\title{Spatio-temporal early warning signals of a critical transition}
\usage{
stew(x,stat,d,output,...)
}
\arguments {
  \item{x}{a Raster object or a Raster Time Series}
  \item{stat}{a character specifying the statistics used to generate stew, can be either of 'stg' or 'stm'}
  \item{d}{a numeric value specifying the geographic distance used to calculate local spatial autocorrelation}
  \item{output}{a character vector specifying the outputs including 'tau', 'raster', or 'rts'}
}

\description{
stew employs LISA (Local Indicators of Spatial Association). Depending on whether a local Geary's c or local Moran's I is used, the stat can be 'stg', or 'stm', respectively.

The output can be either the time series of LISAs, or the trend of the LISA values obtained from applying Kendal's tau, or both.
}
\examples{
#-------

}
