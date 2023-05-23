
#--------------
#--------------
.getKendal <- function(x) {
  # based on the mk.test function in the package trend
  if(!is.numeric(x)){
    stop("'x' must be a numeric vector")
  }
  x <- x[!is.na(x)]

  n <- length(x)

  if(n < 3) stop("'x' must have at least 3 elements!")

  S <- 0.0
  for(j in 1:n) S <- S + sum(sign(x[j] - x[1:j]))
  ## get ties
  t <- table(x)
  names(t) <- NULL

  tadjs <- sum(t * (t - 1) * (2 * t + 5))
  varS <- (n * (n-1) * (2 * n + 5) - tadjs) / 18

  tadjd <- sum(t * (t - 1))
  D <- sqrt(1/2 * n * (n - 1) - 1/2 * tadjd) * sqrt(1/2 * n * (n - 1))
  tau <- S / D

  ## compute z
  sg <- sign(S)
  z <- sg * (abs(S) - 1) / sqrt(varS)


  ## get the pvalue
  pval <- 2 * min(0.5, pnorm(abs(z), lower.tail=FALSE)) # two.sided
  #pval <- pnorm(z, lower.tail=FALSE) # greater
  #pval <- pnorm(z, lower.tail=TRUE) # less

  list(tau=tau,
       p.value=pval,
       statistic=(z = z),
       estimates=c(S=S, varS = varS)
       )
}
#------------

#--------
if (!isGeneric("getKendal")) {
  setGeneric("getKendal", function(x)
    standardGeneric("getKendal"))
}




setMethod('getKendal', signature(x='numeric'),
          function(x) {
            .getKendal(x)
          }
)
