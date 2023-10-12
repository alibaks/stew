



.f <- function(ex,...) {
  ex <- ex[!is.na(ex)]
  if (length(ex) > 30) {
    ex1<-rollapply(ex, width=3, FUN=mean, na.rm = TRUE, fill = NA)
    ex<-na.omit(ex1)
    if (length(ex) > 10) {
      #ex1<-na.omit(ex) 
      kt<-mk.test(ex)
      ktt<-kt$estimates[3]
      ktt
    } else NA
  } else NA
} 

#----

.ac <- function(x,r=50) {
  n <- length(x)
  if (r > n) stop("rolling window should be less than the length of x")
  s <- 1:r
  o <- c()
  while(s[r] <= n) {
    a <- acf(x[s],plot=FALSE,lag.max = 1,na.action = na.pass)
    o <- c(o, a$acf[2])
    s <- s + 1
  }
  o <- o[!is.na(o)]
  if (length(o) > 1) {
    list(tau = trend::mk.test(o)$estimates[3],acf=c(rep(NA,r-1),o))
  }
  
  
}

.gauss.detrend <- function(x,bw,timeindex) {
  ksmooth(timeindex, x, kernel = "normal", bandwidth = bw, range.x = range(timeindex), 
          x.points = timeindex)$y
}


.fdif.detrend <- function(x) {
  (x[2:length(x)] + x[1:(length(x)-1)]) / 2
}


#---------


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
            x <- na.ma(x,k=k)
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
            x <- na.ma(x,k=k)
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
            x <- na.ma(x,k=k)
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

.sptime_ews.terra <- function(x, detrend=NULL,bw=5, interpolate=NULL,k=6,rw=50) {
  bw <- round(nlyr(x@raster) * bw / 100)
  rw <- round(nlyr(x@raster) * rw / 100)
  
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
        app(x@raster,.f)
        
        
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
        app(x@raster,.f)
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
        app(x@raster,.f)
        
        
        
      }
      
      
    } else if (interpolate == "ma") {
      if (is.null(detrend)) {
        #---------
        cat('\n no detrending, ma for interpolation, procedure is started for raster time series')
        .f <- function(x,...) {
          if (all(is.na(x))) {
            return(NA)
          } else {
            x <- na.ma(x,k=k)
            a <- .ac(x,r=rw)
            if (!is.null(a)) a$tau
            else NA
          }
        }
        #-------------------
        app(x@raster,.f)
        
        
      } else if (detrend == "gaussian") {
        cat('\n gaussian detrending, ma for interpolation, procedure is started for raster time series')
        .f <- function(x,...) {
          if (all(is.na(x))) {
            return(NA)
          } else {
            x <- na.ma(x,k=k)
            x <- .gauss.detrend(x, bw = bw, timeindex = timeindex)
            a <- .ac(x,r=rw)
            if (!is.null(a)) a$tau
            else NA
          }
        }
        #-------------------
        app(x@raster,.f)
      } else if (detrend == "fdif") {
        cat('\n fdif detrending, ma for interpolation, procedure is started for raster time series')
        .f <- function(x,...) {
          if (all(is.na(x))) {
            return(NA)
          } else {
            x <- na.ma(x,k=k)
            x <- .fdif.detrend(x)
            
            x<-rollapply(x, width=6, FUN=mean, na.rm = TRUE, fill = NA)
            a <- .ac(x,r=rw)
            if (!is.null(a)) a$tau
            else NA
          }
        }
        #-------------------
        app(x@raster,.f)
        
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
      app(x@raster,.f)
      
      
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
      app(x@raster,.f)
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
      app(x@raster,.f)
      
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
            .sptime_ews(x,  detrend=detrend,bw=bw, interpolate=interpolate,k=k,rw=rw)
          }
)


setMethod('sptime_ews', signature(x='SpatRasterTS'),
          function(x, detrend=NULL,bw=5, interpolate=NULL,k=6,rw=50) {
            .sptime_ews.terra(x, detrend=detrend,bw=bw, interpolate=interpolate,k=k,rw=rw)
          }
)
