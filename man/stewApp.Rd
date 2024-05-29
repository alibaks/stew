\name{stewApp}
\alias{stewApp}
\alias{stewApp,Any-method}
\title{Stew Shiny Application}
\description{
  A shiny-based GUI 
}
\usage{
  stewApp(x)
}
\arguments{
  \item{x}{not implemented}
}
\details{
  The function calculates Kendall's rank correlation coefficient and its associated p-value for the input numeric vector \code{x}. Kendall's rank correlation measures the strength and direction of the relationship between two variables based on the ranks of their values.

  The function first checks if \code{x} is a numeric vector, and throws an error if it is not. Any missing values are removed using the \code{na.omit} function to ensure valid calculations.

  Kendall's rank correlation coefficient (\code{tau}) is computed by dividing the sum of signs of pairwise differences between elements of \code{x} by a measure of variability (\code{D}). The coefficient ranges between -1 and 1, where 1 indicates a perfect positive relationship, -1 indicates a perfect negative relationship, and 0 indicates no correlation.

  The standard error (\code{varS}) is calculated based on the number of tied values in \code{x}. Tied values occur when two or more elements have the same value. The presence of ties affects the variability of the data and needs to be taken into account in the calculation.

  The test statistic (\code{z}) is computed as the signed difference between \code{S} and 1, divided by the square root of \code{varS}. The sign of \code{z} indicates the direction of the correlation.

  The p-value is calculated as twice the minimum of 0.5 and the cumulative distribution function of the standard normal distribution evaluated at the absolute value of \code{z}. It represents the probability of obtaining a correlation as extreme as the observed correlation, assuming the null hypothesis of no correlation.
}

\value{
  A list containing the Kendall's rank correlation coefficient, p-value, test statistic, and estimates.
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
getKendal(x) 
}
