#' Convert coordinates between reference systems
#' 
#' Coordinates are converted between reference systems.
#' 
#' @param data \code{Numeric} matrix or data frame, x-, y-coordinates to 
#' be converted.
#' 
#' @param from \code{Character} value, proj4 string of the current 
#' reference system.
#' 
#' @param to \code{Character} value, proj4 string of the target 
#' reference system. 
#' 
#' @return \code{Numeric} data frame with converted coordinates.
#' @author Michael Dietze
#' @keywords eseis
#' @examples
#' 
#' xy <- cbind(13, 55)
#' 
#' proj_in <- "+proj=longlat + ellps=WGS84"
#' 
#' proj_out <- "+proj=utm +zone=32 +datum=WGS84"
#' 
#' spatial_convert(data = xy, 
#'                 from = proj_in, 
#'                 to = proj_out)
#'                      
#' @export spatial_convert
spatial_convert <- function(
  data,
  from,
  to
) {
  
  ## check input data
  if(ncol(data) != 2) {
    
    warning("Coordinate data must contain only 2 columns")
    return(c(NA, NA))
  }
  
  test_from <- try(sp::CRS(from), 
                   silent = TRUE)
  
  if(class(test_from) == "try-error") {
    
    warning("Input reference system incorrect!")
    return(data)
  }

  test_to <- try(sp::CRS(to), 
                   silent = TRUE)
  
  if(class(test_to) == "try-error") {
    
    warning("Output reference system incorrect!")
    return(data)
  }
  
  ## convert coordinates to SpatialPoints object
  data <- sp::SpatialPoints(data)
  
  ## assign CRS to SpatialPoints object
  sp::proj4string(data) <- sp::CRS(projargs = from)
  
  ## convert CRS
  data <-sp::spTransform(x = data, CRS = to)
  
  ## convert output to data frame
  data <- as.data.frame(sp::coordinates(data))
  
  ## return data frame
  return(data)
}

