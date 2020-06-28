
.getLisa <- function(x,d1,d2,stat) {
  elsa::lisa(x,d1,d2,statistic = stat)
}

#--------
if (!isGeneric("stew")) {
  setGeneric("stew", function(x,stat,d,output,...)
    standardGeneric("stew"))
}




setMethod('stew', signature(x='RasterStackBrickTS'),
          function(x,stat,d,output,...) {
            ind <- index(x)

            .terra <- require(terra)

            k <- NULL

            if (missing(stat)) stat <- 'stg'
            else stat <- tolower(stat)

            xx <- x@raster

            if (missing(d)) d <- max(res(xx)) * sqrt(2)


            # if (require(terra)) {
            #   xx <- rast(x@raster)
            # } else {
            #   xx <- x@raster
            # }
            #if (ncell(xx) > 1e7)

            o <- raster(xx)

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
              if (.terra) {
                k <- app(rast(o),.getKendal)
              } else {
                k <- calc(o,.getKendal)
              }
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
