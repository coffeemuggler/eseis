#' Crop extent of spatial data.
#' 
#' The function crops the spatial extent of raster objects or other 
#' spatial objects based on bounding box coordinates.
#' 
#' @param data \code{raster} object, spatial data set to be processed.
#' 
#' @param bbox \code{Numeric} vector of length four, bounding box coordinates 
#' in the form \code{c(xmin, xmax, ymin, ymax)}
#' 
#' @return spatial object, cropped to bounding box
#' 
#' @author Michael Dietze
#' @keywords eseis
#' @examples
#' 
#' ## create example data set
#' x <- raster::raster(nrows = 100, 
#'                     ncols = 100, 
#'                     xmn = 0, 
#'                     xmx = 10, 
#'                     ymn = 0, 
#'                     ymx = 10)
#' raster::values(x) <- 1:10000
#' 
#' ## create crop extent vector
#' bbox <- c(3, 7, 3, 7)
#' 
#' ## crop spatial object
#' y <- spatial_crop(data = x, 
#'                   bbox = bbox)
#' 
#' ## plot both objects
#' raster::plot(x)
#' raster::plot(y, add = TRUE)
#' 
#' @export spatial_crop
spatial_crop <- function(
  data,
  bbox
) {
  
  ## convert bbox vector to extent object
  bbox_extent <- raster::extent(x = bbox)
  
  ## crop spatial object
  data_out <- raster::crop(x = data, 
                           y = bbox_extent)
  
  ## return data set
  return(data_out)
}