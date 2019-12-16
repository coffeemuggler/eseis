#' Convert coordinates between reference systems
#' 
#' Coordinates are converted between reference systems.
#' 
#' @param data \code{Numeric} vector of length two or data frame, 
#' x-, y-coordinates to be converted.
#' 
#' @param to \code{Character} value, proj4 string of the output 
#' reference system. 
#' 
#' @return \code{Numeric} data frame with converted coordinates.
#' 
#' @author Michael Dietze
#' 
#' @keywords eseis
#' 
#' @examples
#' 
#' ## create lat lon coordinates
#' xy <- c(13, 55)
#'
#' ## define output coordinate system 
#' proj_out <- "+proj=utm +zone=32 +datum=WGS84"
#' 
#' ## convert coordinate pair
#' spatial_convert(data = xy, 
#'                 to = proj_out)
#'                 
#' ## define set of coordinates
#' xy <- data.frame(x = c(10, 11),
#'                  y = c(54, 55))
#'                  
#' ## convert set of coordinates
#' spatial_convert(data = xy, 
#'                 to = proj_out)
#'                      
#' @export spatial_convert
spatial_convert <- function(
  data,
  to
) {
  
  ## check input data
  if(class(data)[1] == "data.frame") {
    
    if(ncol(data) != 2) {
      
      stop("Coordinate data must contain only 2 columns, x and y!")
    } else {
      
      ## convert data to matrix
      data <- as.matrix(data)
    }
  } else if(class(data)[1] == "numeric" | class(data)[1] == "atomic") {
    
    if(length(data) != 2) {
      
      stop("Coordinate data must contain only 2 values, x and y!")
    } else {
      
      ## convert vector to data frame
      data <- cbind(x = data[1],
                    y = data[2])
    }
  } else {
    
    stop("Only data frames and vectors are supported as input data!")
  }
  
  ## project data set
  data <- rgdal::project(data, to)
  
  ## convert output to data frame
  data <- as.data.frame(data)
  
  ## return data frame
  return(data)
}