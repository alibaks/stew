
#----------------
.rollMean <- function(x, k=3) {
  if (k < 2 || k > length(x)) stop('k should be greater than 1 and less than the length of x')
  o <- c()
  for (i in 1:(length(x) - k + 1)) {
    o <- c(o,mean(x[c(i:(i + k - 1))],na.rm = TRUE))
  }
  o
}

#------

#--------
if (!isGeneric("rollMean")) {
  setGeneric("rollMean", function(x,k)
    standardGeneric("rollMean"))
}




setMethod('rollMean', signature(x='numeric'),
          function(x,k) {
            .rollMean(x,k)
          }
)