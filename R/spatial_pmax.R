#' Get most likely source location
#' 
#' The function identifies the location of a seismic source with the 
#' heighest likelihood (P_max).
#' 
#' @param data \code{SpatRaster} object, spatial data set with source location
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
#' x <- terra::rast(nrows = 100, ncols = 100, 
#'                  xmin = 0, xmax = 10, 
#'                  ymin = 0, ymax = 10)
#' terra::values(x) <- runif(n = 100)
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
  
  ## get maximum likelihood indices
  xy_max <- terra::crds(data)[values(data) == max(values(data))]
  
  ## return data set
  return(xy_max)
}