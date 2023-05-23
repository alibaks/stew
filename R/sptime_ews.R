
.sptime_ews <- function(x, detrend=NULL,bw=5, interpolate=NULL,k=6,rw=50) {
  bw <- round(nlayers(x@raster) * bw / 100)
  rw <- round(nlayers(x@raster) * rw / 100)
  
  timeindex <- index(x[1])
  
  if (!is.null(interpolate)) {
    if (interpolate == "approx") {
      if (is.null(detrend)) {
        #---------
        cat('\n no detrending, approx for interpolation, procedure is started for raster time series')
        .f <- function(x,...) {
          if (all(is.na(x))) {
            return(NA)
          } else {
            x <- na.approx(x)
            a <- .ac(x,r=rw)
            if (!is.null(a)) a$tau
            else NA
          }
        }
        #-------------------
        calc(x@raster,.f)
        
        
      } else if (detrend == "gaussian") {
        cat('\n gaussian detrending, approx for interpolation, procedure is started for raster time series')
        .f <- function(x,...) {
          if (all(is.na(x))) {
            return(NA)
          } else {
            x <- na.approx(x)
            x <- .gauss.detrend(x, bw = bw, timeindex = timeindex)
            a <- .ac(x,r=rw)
            if (!is.null(a)) a$tau
            else NA
          }
        }
        #-------------------
        calc(x@raster,.f)
      } else if (detrend == "fdif") {
        cat('\n fdif detrending, approx for interpolation, procedure is started for raster time series')
        .f <- function(x,...) {
          if (all(is.na(x))) {
            return(NA)
          } else {
            x <- na.approx(x)
            x <- .fdif.detrend(x)
            a <- .ac(x,r=rw)
            if (!is.null(a)) a$tau
            else NA
          }
        }
        #-------------------
        calc(x@raster,.f)
        
        
        
      }
      
      
    } else if (interpolate == "ma") {
      if (is.null(detrend)) {
        #---------
        cat('\n no detrending, ma for interpolation, procedure is started for raster time series')
        .f <- function(x,...) {
          if (all(is.na(x))) {
            return(NA)
          } else {
            x <- imputeTS::na.ma(x,k=k)
            a <- .ac(x,r=rw)
            if (!is.null(a)) a$tau
            else NA
          }
        }
        #-------------------
        calc(x@raster,.f)
        
        
      } else if (detrend == "gaussian") {
        cat('\n gaussian detrending, ma for interpolation, procedure is started for raster time series')
        .f <- function(x,...) {
          if (all(is.na(x))) {
            return(NA)
          } else {
            x <- imputeTS::na.ma(x,k=k)
            x <- .gauss.detrend(x, bw = bw, timeindex = timeindex)
            a <- .ac(x,r=rw)
            if (!is.null(a)) a$tau
            else NA
          }
        }
        #-------------------
        calc(x@raster,.f)
      } else if (detrend == "fdif") {
        cat('\n fdif detrending, ma for interpolation, procedure is started for raster time series')
        .f <- function(x,...) {
          if (all(is.na(x))) {
            return(NA)
          } else {
            x <- imputeTS::na.ma(x,k=k)
            x <- .fdif.detrend(x)
            
            x<-rollapply(x, width=6, FUN=mean, na.rm = TRUE, fill = NA)
            a <- .ac(x,r=rw)
            if (!is.null(a)) a$tau
            else NA
          }
        }
        #-------------------
        calc(x@raster,.f)
        
      }
    }
  } else {
    if (is.null(detrend)) {
      #---------
      cat('\n no detrending, no interpolation, procedure is started for raster time series')
      .f <- function(x,...) {
        if (all(is.na(x))) {
          return(NA)
        } else {
          a <- .ac(x,r=rw)
          if (!is.null(a)) a$tau
          else NA
        }
      }
      #-------------------
      calc(x@raster,.f)
      
      
    } else if (detrend == "gaussian") {
      cat('\n gaussian detrending, no interpolation, procedure is started for raster time series')
      .f <- function(x,...) {
        if (all(is.na(x))) {
          return(NA)
        } else {
          x <- .gauss.detrend(x, bw = bw, timeindex = timeindex)
          a <- .ac(x,r=rw)
          if (!is.null(a)) a$tau
          else NA
        }
      }
      #-------------------
      calc(x@raster,.f)
    } else if (detrend == "fdif") {
      cat('\n fdif detrending, no interpolation, procedure is started for raster time series')
      .f <- function(x,...) {
        if (all(is.na(x))) {
          return(NA)
        } else {
          x <- .fdif.detrend(x)
          a <- .ac(x,r=rw)
          if (!is.null(a)) a$tau
          else NA
        }
      }
      #-------------------
      calc(x@raster,.f)
      
    }
  }
  
}
#----------



#--------
if (!isGeneric("sptime_ews")) {
  setGeneric("sptime_ews", function(x, detrend=NULL,bw=5, interpolate=NULL,k=6,rw=50)
    standardGeneric("sptime_ews"))
}




setMethod('sptime_ews', signature(x='RasterStackBrickTS'),
          function(x, detrend=NULL,bw=5, interpolate=NULL,k=6,rw=50) {
            .sptime_ews(x, detrend=NULL,bw=5, interpolate=NULL,k=6,rw=50)
          }
)

