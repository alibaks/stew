
#-------------------
.fdif <- function(ex,k=3) {
  ex <- as.vector(ex)
  ex1 <- .rollMean(ex,k=k)
  ex <- na.omit(ex1)

  if (length(ex) > 20) {
    kt <- trend::mk.test(ex)
    ktt <- kt$estimates[3]
    ktt
  } else NA
}



#--------
if (!isGeneric("fdif")) {
  setGeneric("fdif", function(x,k)
    standardGeneric("fdif"))
}




setMethod('fdif', signature(x='numeric'),
          function(x,k) {
            .fdif(x,k)
          }
)
