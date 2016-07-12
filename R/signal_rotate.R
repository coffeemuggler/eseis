#' Rotate signal vectors using a 3-D rotation matrix.
#' 
#' The function rotates the horizontal components of the input data according 
#' to the specified rotation angle. 
#' 
#' @param data \code{Numeric} matrix, or list of matrices, input signal 
#' vectors. Matrix must contain either two columns (x- and y-component) or
#' three columns (x-, y-, and z-component), in exactly that order of the 
#' components.
#' 
#' @param angle \code{Numeric} scalar, rotation angle in degrees.
#' 
#' @return \code{Numeric} matrix, the 3-dimensional rotation matrix.
#' @author Michael Dietze
#' @keywords eseis
#' @examples
#' 
#' ## create artificial data set
#' data <- rbind(x = sin(seq(0, pi, length.out = 10)),
#' y = sin(seq(0, pi, length.out = 10)),
#' z = rep(0, 10))
#' 
#' ## rotate the data set
#' x_rot <- signal_rotate(data = data, 
#'                        angle = 15)
#'                       
#' ## plot the rotated data set 
#' plot(x_rot[1,], col = 1, ylim = c(-2, 2))
#' points(x_rot[2,], col = 2)
#' points(x_rot[3,], col = 3)
#' 
#' @export signal_rotate
signal_rotate <- function(
  data,
  angle
) {
  ## check data structure
  if(class(data) == "list") {
    
    ## apply function to list
    data_out <- lapply(X = data, 
                       FUN = eseis::signal_rotate, 
                       angle = angle)
    
    ## return output
    return(data_out)
  } else {
    
    ## check/set angle
    if(missing(angle) == TRUE) {
      
      print("No angle provided! Set to 0 by default.")
      angle <- 0
    }
    
    ## convert degrees to radians
    angle <- angle * pi / 180
    
    ## build rotation matrix
    rot <- rbind(c(0, -sin(angle), cos(angle)),
                 c(0, cos(angle), sin(angle)),
                 c(1, 0, 0))
    
    ## homogenise input data sets
    if(nrow(data) == 2) {
      data <- rbind(data, rep(0, ncol(data)))
    }
    
    ## rotate signal traces
    data_out <- t(t(data) %*% rot)
    
    ## prepare output data
    data_out <- data_out[3:1,]
  }

  ## return output
  return(data_out)
}