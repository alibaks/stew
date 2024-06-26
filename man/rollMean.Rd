\name{rollMean}
\alias{rollMean}
\alias{rollMean,numeric-method}


\title{Compute Rolling Mean}
\description{
  This function calculates the rolling mean of a numeric vector.
}
\usage{
  rollMean(x, k)
}
\arguments{
  \item{x}{A numeric vector.}
  \item{k}{An integer specifying the window size. Default is 3.}
}
\details{
  The function calculates the rolling mean of the input vector \code{x} using a sliding window of size \code{k}. The window starts at the first element of \code{x} and moves one element at a time until the end of the vector. The mean is calculated for each window.
}
\value{
  A numeric vector of length \code{length(x) - k + 1}, containing the rolling mean values.
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
  x <- c(1, 2, 3, 4, 5)
  rollMean(x, k = 3) # returns: 2, 3, 4
}
