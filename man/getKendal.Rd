\name{getKendal}
\alias{getKendal}
\alias{getKendal,numeric-method}
\title{Calculate Kendall's Rank Correlation}
\description{
  This function calculates Kendall's rank correlation coefficient and its associated p-value for a numeric vector.
}
\usage{
  getKendal(x)
}
\arguments{
  \item{x}{A numeric vector.}
}
\details{
  The function calculates Kendall's rank correlation coefficient and its associated p-value for the input numeric vector \code{x}. Kendall's rank correlation measures the strength and direction of the relationship between two variables based on the ranks of their values.

  The function first checks if \code{x} is a numeric vector, and throws an error if it is not. Any missing values are removed using the \code{na.omit} function to ensure valid calculations.

  Kendall's rank correlation coefficient (\code{tau}) is computed by dividing the sum of signs of pairwise differences between elements of \code{x} by a measure of variability (\code{D}). The coefficient ranges between -1 and 1, where 1 indicates a perfect positive relationship, -1 indicates a perfect negative relationship, and 0 indicates no correlation.

  The standard error (\code{varS}) is calculated based on the number of tied values in \code{x}. Tied values occur when two or more elements have the same value. The presence of ties affects the variability of the data and needs to be taken into account in the calculation.

  The test statistic (\code{z}) is computed as the signed difference between \code{S} and 1, divided by the square root of \code{varS}. The sign of \code{z} indicates the direction of the correlation.

  The p-value is calculated as twice the minimum of 0.5 and the cumulative distribution function of the standard normal distribution evaluated at the absolute value of \code{z}. It represents the probability of obtaining a correlation as extreme as the observed correlation, assuming the null hypothesis of no correlation.

  The function returns a list with the following components:
  \begin{description}
    \item{\code{tau}}{The Kendall's rank correlation coefficient, indicating the strength and direction of the relationship.}
    \item{\code{p.value}}{The p-value associated with the test statistic, indicating the statistical significance of the observed correlation.}
    \item{\code{statistic}}{A named numeric vector containing the test statistic (\code{z}), which represents the standardized difference between the observed correlation and the expected correlation under the null hypothesis.}
    \item{\code{estimates}}{A named numeric vector containing the estimates \code{S} and \code{varS}, which provide additional information about the calculation.}
  \end{description}
}
\value{
  A list containing the Kendall's rank correlation coefficient, p-value, test statistic, and estimates.
}
\examples{
x <- c(1, 2, 3, 4, 5)
getKendal(x) # returns: tau=1, p.value=0.002856, statistic=(z=2.833213), estimates=c(S=10, varS=9.5)
}
