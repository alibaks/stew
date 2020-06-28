#--------
if (!isGeneric("spews")) {
  setGeneric("spews", function(x,stat,r,output,...)
    standardGeneric("spews"))
}




setMethod('spews', signature(x='RasterStackBrickTS'),
          function(x,stat,r,output,...) {

          }
)
