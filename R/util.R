
#-------------------
.fdif <- function(ex,k=3,..) {
  ex <- as.vector(ex)
  ex1 <- .rollMean(ex,k=k)
  ex <- na.omit(ex1)

  if (length(ex) > 20) {
    kt <- trend::mk.test(ex)
    ktt <- kt$estimates[3]
    ktt??
  } else NA
}
#----------------
.rollMean <- function(x, k=3) {
  if (k < 2 || k > length(x)) stop('k should be greater than 1 and less than the length of x')
  o <- c()
  for (i in 1:(length(x) - k + 1)) {
    o <- c(o,mean(x[c(i:(i + k - 1))],na.rm = TRUE))
  }
  o
}
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

#---------
.acf <- function(x,r=NULL) {
  n <- length(x)
  if (is.null(r)) r <- floor(n / 2)
  else if (r > n) stop("rolling window should be less than the length of x")

  s <- 1:r

  o <- c()
  while(s[r] <= n) {
    a <- acf(x[s],plot=FALSE,lag.max = 1,na.action = na.pass)
    o <- c(o, a$acf[2])
    s <- s + 1
  }
  o <- o[!is.na(o)]
  if (length(o) > 1) {
    list(tau = .getKendal(x)$tau,acf=c(rep(NA,r-1),o))
  }
}

