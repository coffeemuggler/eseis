#' Clip values of spatial data.
#' 
#' The function replaces raster values based on different thresholds.
#' 
#' @param data \code{raster} object, spatial data set to be processed.
#' 
#' @param quantile \code{Numeric} value, quantile value below which raster 
#' values are clipped.
#' 
#' @param replace \code{Numeric} value, replacement value, default is
#' \code{NA}.
#' 
#' @param normalise \code{Logical} value, optionally normalise values above 
#' threshold quantile between 0 and 1. Default is \code{TRUE}. 
#' 
#' @return \code{raster} object,clipped spatial data set.
#' 
#' @author Michael Dietze
#' @keywords eseis
#' @examples
#' 
#' ## TO BE DONE
#'                      
#' @export spatial_clip
spatial_clip <- function(
  data,
  quantile,
  replace = NA,
  normalise = TRUE
) {
  
  ## check/set parameters
  if(missing(quantile) == TRUE) {
    
    quantile <- 1
  }
  
  ## replace values
  data@data@values[
    data@data@values < quantile(data@data@values,
                                quantile,
                                na.rm = TRUE)] <- replace
  
  ## optionally normaise data set
  if(normalise == TRUE) {
    
    data@data@values <- (
      data@data@values - min(data@data@values, na.rm = TRUE)) / 
      (max(data@data@values, na.rm = TRUE) - 
         min(data@data@values, na.rm = TRUE))
  }
  
  ## return data set
  return(data)
}