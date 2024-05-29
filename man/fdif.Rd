\name{fdif}
\alias{fdif}
\alias{fdif,numeric-method}

\title{Estimate Trend for Vector}
\description{
  This function estimates the trend for a vector and returns the trend value if the vector has more than 20 non-missing values.
}
\usage{
  fdif(x, k )
}
\arguments{
  \item{x}{A vector.}
  \item{k}{An integer specifying the window size for computing the rolling mean. Default is 3.}
  
}
\details{
  The function first converts the input vector \code{ex} to a numeric vector using \code{as.vector}. Then, it calculates the rolling mean of \code{ex} using the function \code{.rollMean} with the specified window size \code{k}. Any missing values are removed using \code{na.omit}. If the resulting vector has more than 20 non-missing values, the function applies the \code{trend::mk.test} function to estimate the trend and returns the estimated trend value. Otherwise, it returns \code{NA}.
}
\value{
  If the length of the resulting vector after removing missing values is greater than 20, the function returns the estimated trend value as a numeric value. Otherwise, it returns \code{NA}.
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
ex <- c(1, 2, 3, 4, 5)
fdif(ex, k = 3) # returns: NA, as length(ex) is not greater than 20
}
