
# measure local indicators of spatial association (LISA) using the package elsa
.getLisa <- function(x,d1,d2,stat) {
  lisa(x,d1,d2,statistic = stat)
}
#---
.rasKendal <- function(x,pv_sig=0.05,...) {
  # based on the mk.test function in the package trend
  if (all(is.na(x))) return(NA)
  
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
  
  if (!is.null(pv_sig) && !is.na(pval)) {
    if (pval > pv_sig) tau <- NA
  }
  
  tau
  
}
#--------
if (!isGeneric("stew")) {
  setGeneric("stew", function(x,stat,d,output,pv_sig,...)
    standardGeneric("stew"))
}




setMethod('stew', signature(x='RasterStackBrickTS'),
          function(x,stat,d,output,pv_sig,...) {
            ind <- index(x)
            
            k <- NULL
            
            if (missing(pv_sig)) pv_sig <- NULL

            if (missing(stat)) stat <- 'stg'
            else stat <- tolower(stat)

            xx <- x@raster

            if (missing(d)) d <- max(res(xx)) * sqrt(2)
            
            if (missing(output)) output <- 'rts'
            else {
              if (!all(output %in% c('raster','rts','tau'))) {
                warning('Not all the specified items in output is recognized, default "rts" is used')
                output <- 'rts'
              }
            }
            
            
            
            o <- raster(xx[[1]])

            if (stat %in% c('stg','geary','localgeary','c')) {
              for (i in 1:nlayers(xx)) {
                o <- addLayer(o, .getLisa(xx[[i]],d1=0,d2=d,stat='c'))
              }
            } else if (stat %in% c('stm','moran','localmoran','m','i')) {
              for (i in 1:nlayers(xx)) {
                o <- addLayer(o, .getLisa(xx[[i]],d1=0,d2=d,stat='i'))
              }
            }
            #-------
            if ('tau' %in% output) {
              k <- calc(o,.rasKendal,pv_sig=pv_sig)
            }
            #--------
            if ('rts' %in% output) {
              o <- rts(o, ind)
            }
            #--------
            if (length(output) == 1) {
              if (output == 'tau') return(k)
              else return(o)
            } else {
              oo <- list()
              if ('tau' %in% output) oo[['tau']] <- k
              if ('rts' %in% output) oo[['rts']] <- o
              else oo[['raster']] <- o
              return (oo)
            }

          }
)
#------

setMethod('stew', signature(x='SpatRasterTS'),
          function(x,stat,d,output,pv_sig,...) {
            ind <- index(x)
            
            k <- NULL
            
            if (missing(pv_sig)) pv_sig <- NULL
            
            if (missing(stat)) stat <- 'stg'
            else stat <- tolower(stat)
            
            xx <- x@raster
            
            if (missing(d)) d <- max(res(xx)) * sqrt(2)
            
            if (missing(output)) output <- 'rts'
            else {
              if (!all(output %in% c('raster','rts','tau'))) {
                warning('Not all the specified items in output is recognized, default "rts" is used')
                output <- 'rts'
              }
            }
            
            
            
            o <- rast(xx[[1]])
            
            if (stat %in% c('stg','geary','localgeary','c')) {
              for (i in 1:nlyr(xx)) {
                o <- c(o, .getLisa(xx[[i]],d1=0,d2=d,stat='c'))
              }
            } else if (stat %in% c('stm','moran','localmoran','m','i')) {
              for (i in 1:nlyr(xx)) {
                o <- c(o, .getLisa(xx[[i]],d1=0,d2=d,stat='i'))
              }
            }
            #-------
            if ('tau' %in% output) {
                k <- app(rast(o),.rasKendal,pv_sig=pv_sig)
            }
            #--------
            if (c('rts') %in% output) {
              o <- rts(o, ind)
            }
            #--------
            if (length(output) == 1) {
              if (output == 'tau') return(k)
              else return(o)
            } else {
              oo <- list()
              if ('tau' %in% output) oo[['tau']] <- k
              if ('rts' %in% output) oo[['rts']] <- o
              else oo[['raster']] <- o
              return (oo)
            }
            
          }
)