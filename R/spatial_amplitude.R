#' Locate the source of a seismic event by modelling amplutide attenuation
#'
#' The function fits a model of signal amplitude attenuation for all grid 
#' cells of the distance data sets and returns the residual sum as measure 
#' of the most likely source location of an event.
#'
#' @param data \code{Numeric} matrix or \code{eseis} object, seismic signals 
#' to work with. Since the function will calculate the maxima of the data it 
#' is usually the envolopes of the data that should be used here.
#' 
#' @param coupling \code{Numeric} vector, coupling efficiency factors for each
#' seismic station. The best coupled station (or the one with the highest 
#' amplification) must receive \code{1}, the others must be scaled relatively 
#' to this one.
#' 
#' @param d_map \code{List} object, distance maps for each station (i.e., 
#' \code{SpatialGridDataFrame} objects). Output of \code{distance_map}.
#' 
#' @param aoi \code{SpatialGridDataFrame} or \code{raster} object that 
#' defines which pixels are used to locate the source. If omitted, the entire 
#' distance map extent is used. \code{aoi} and \code{d_map} objects must have
#' the same extents, projections and pixel sizes. The \code{aoi} map must 
#' be of logical values.
#' 
#' @param v \code{Numeric} value, mean velocity of seismic waves (m/s).
#' 
#' @param q \code{Numeric} value, quality factor of the ground.
#' 
#' @param f \code{Numeric} value, frequency for which to model the 
#' attenuation.
#' 
#' @param a_0 \code{Logical} value, start parameter of the source amplitude,
#' if not provided, a best guess is made as 100 times the maximum amplitude 
#' value of the data set.
#' 
#' @param normalise \code{Logical} value, option to normalise sum of 
#' residuals between 0 and 1. Default is \code{TRUE}.
#' 
#' @return A raster object with the sums of squared model residuals for each 
#' grid cell.
#' 
#' @author Michael Dietze
#' 
#' @examples
#'
#' \dontrun{
#' 
#' L <- spatial_amplitude(data = s_f, 
#' coupling = c(1, 1, 1, 1),
#' d_map = D_50$maps, 
#' v = 900, 
#' q = 100, 
#' f = 7.5)
#' 
#' }
#'
#' @export spatial_amplitude

spatial_amplitude <- function (
  
  data, 
  coupling,
  d_map, 
  aoi,
  v,
  q, 
  f, 
  a_0,
  normalise = TRUE
) {
  
  ## check/set coupling factors
  if(missing(coupling) == TRUE) {
    
    coupling <- rep(1, length(data))
  }
  
  ## get maximum amplitude
  a_d <- do.call(c, lapply(X = data, FUN = function(data) {
    
    max(data$signal)
  }))
  
  ## normalise by coupling efficiency
  a_d <- a_d * coupling
  
  ## check/set source amplitude
  if(missing(a_0) == TRUE) {
    
    a_0 <- 100 * max(a_d)
  }

  ## convert distance data sets to matrix with distance values
  d <- do.call(cbind, lapply(X = d_map, 
                                  FUN = function(d_map) {d_map@data[,1]}))
  
  ## check if aoi is provided and create aoi index vector
  if(missing(aoi) == FALSE) {
    
    px_ok <- aoi@data[,1]
  } else {
    
    px_ok <- rep(1, nrow(d))
  }
  
  d <- cbind(px_ok, d)
  
  ## convert distance data to list
  d <- as.list(as.data.frame(t(d)))
  
  
  ## define amplitude function
  model_fun <- a_d ~ a_0 / sqrt(d) * exp(-((pi * f * d) / (q * v)))
  
  ## create model parameter list
  model_par <- list(a_d = a_d, 
                    d = rep(NA, length(a_d)),
                    f = f,
                    q = q,
                    v = v)
  
  ## create model start parameter list
  model_start <- list(a_0 = a_0)
  
  ## model event amplitude as function of distance and get sum of residuals
  r <- 
    lapply(X = d, FUN = function(d, model_fun, model_par, model_start) {
      
      process <- d[1]
      
      if(process == 1) {
        
        model_par$d <- d[-1]
        
        mod <- try(nls(formula = model_fun, 
                       data = model_par,
                       start = model_start))
        
        res <- try(sum(residuals(mod)^2))
        
        if(class(res)[1] != "try-error") {
          
          return(res)
        }
      } else {
        
        return(NA)
      }
      
      
    }, model_fun, model_par, model_start)
  
  ## convert list to vector
  r <- do.call(c, r)
  
  ## optionally normalise data
  if(normalise == TRUE) {
    
    r <- (r - min(r, na.rm = TRUE)) / 
      (max(r, na.rm = TRUE) - min(r, na.rm = TRUE))
  }
  
  ## convert data structure to raster
  l <- d_map[[1]]
  l@data[,1] <- r
  l <- raster::raster(l)
  
  ## return output
  return(l)
}
