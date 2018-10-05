#' Get most likely source location
#' 
#' The function identifies the location of a seismic source with the 
#' heighest likelihood (P_max).
#' 
#' @param data \code{raster} object, spatial data set with source location
#' estimates.
#' 
#' @return \code{data.frame}, coordinates (x and y) of the most likely s
#' ource location(s).
#' 
#' @author Michael Dietze
#' @keywords eseis
#' @examples
#' 
#' ## create example source location likelihood raster
#' x <- raster::raster(nrows = 10, 
#'                     ncols = 10, 
#'                     xmn = 0, 
#'                     xmx = 10, 
#'                     ymn = 0, 
#'                     ymx = 10)
#' raster::values(x) <- runif(n = 100)
#' 
#' ## identify location of highest likelihood
#' p_max <- spatial_pmax(data = x)
#' 
#' ## show result
#' print(p_max)
#' 
#' @export spatial_pmax
spatial_pmax <- function(
  data
) {
  
  ## extract raster values
  p <- raster::values(data)
  
  ## extract raster coordinates
  xy <- sp::coordinates(data)
  
  ## get maximum likelihood indices
  i_max <- seq(1, length(p))[p == max(p, na.rm = TRUE)]
  
  ## collect coordinates
  data_out <- data.frame(x = xy[i_max,1],
                         y = xy[i_max,2])
  
  ## return data set
  return(data_out)
}