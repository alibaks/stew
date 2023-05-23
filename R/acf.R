
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

#--------
if (!isGeneric("acfk")) {
  setGeneric("acfk", function(x,r)
    standardGeneric("acfk"))
}




setMethod('acfk', signature(x='numeric'),
          function(x,r) {
            .acf(x,r)
          }
)

