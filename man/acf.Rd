\name{acfk}
\alias{acfk}
\alias{acfk,numeric-method}


\title{Compute Autocorrelation Function and trend analysis}
\description{
  This function calculates the autocorrelation function (ACF) for a numeric vector and calculate a trend using Kendal Tau.
}
\usage{
  acfk(x, r )
}
\arguments{
  \item{x}{A numeric vector.}
  \item{r}{An optional integer specifying the rolling window size for computing the ACF. If not provided, the default value is set to \code{floor(n / 2)}, where \code{n} is the length of \code{x}.}
}
\details{
  The function calculates the ACF for the input numeric vector \code{x}. The ACF measures the correlation between a time series and its lagged values at different time intervals.

  If the rolling window size \code{r} is not provided, it is set to \code{floor(n / 2)}, where \code{n} is the length of \code{x}. The rolling window should be less than or equal to the length of \code{x}.

  The function iteratively calculates the ACF for each rolling window of size \code{r}. For each window, the \code{acf} function from the \code{stats} package is called with the specified options: \code{plot=FALSE} to prevent plotting the ACF, \code{lag.max = 1} to calculate only the first lag, and \code{na.action = na.pass} to handle missing values by passing them through.

  The computed ACF values are stored in the vector \code{o}. Any missing values in \code{o} are removed using \code{!is.na(o)}. If there are more than one non-missing ACF values, the function also includes the Kendall's rank correlation coefficient (\code{tau}) calculated using the \code{.getKendal} function, as well as leading \code{NA} values in the ACF vector to align with the rolling window size.
}
\value{
  A list containing the Kendall's rank correlation coefficient and the ACF values.
}

\examples{
x <- c(1, 2, 3, 4, 5)
acfk(x, r = 2) # returns: tau=1, acfk=c(NA, NA, 1)
}
