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
#' x <- terra::rast(nrows = 100, ncols = 100, 
#'                  xmin = 0, xmax = 10, 
#'                  ymin = 0, ymax = 10)
#' terra::values(x) <- 1:10000
#' 
#' ## create crop extent vector
#' bbox <- c(3, 7, 3, 7)
#' 
#' ## crop spatial object
#' y <- spatial_crop(data = x, 
#'                   bbox = bbox)
#' 
#' ## plot both objects
#' terra::plot(x)
#' terra::plot(y, add = TRUE)
#' 
#' @export spatial_crop
spatial_crop <- function(
  data,
  bbox
) {
  
  ## convert bbox vector to extent object
  bbox_extent <- terra::ext(x = bbox)
  
  ## crop spatial object
  data_out <- terra::crop(x = data, 
                          y = bbox_extent)
  
  ## return data set
  return(data_out)
}