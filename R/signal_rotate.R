#' Rotate signal vectors using a 3-D rotation matrix.
#' 
#' The function rotates the horizontal components of the input data according 
#' to the specified angle. 
#' 
#' @param data \code{List}, \code{data frame} or \code{matrix}, seismic
#' componenents to be processed. If \code{data} is a matrix, the components 
#' must be organised as rows. Also, \code{data} can be a list of 
#' \code{eseis} objects. If a matrix, this matrix must contain either two 
#' columns (x- and y-component) or three columns (x-, y-, and z-component), 
#' in exactly that order of the components.
#' 
#' @param angle \code{Numeric} value, rotation angle in degrees.
#' 
#' @return \code{Numeric} matrix, the 3-dimensional rotation matrix.
#' @author Michael Dietze
#' @keywords eseis
#' @examples
#' 
#' ## create synthetic data set
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

    ## check/set angle
    if(missing(angle) == TRUE) {
      
      print("No angle provided! Set to 0 by default.")
      angle <- 0
    }
    
    ## get start time
    eseis_t_0 <- Sys.time()
    
    ## collect function arguments
    eseis_arguments <- list(data = "",
                            angle = angle)
    
    ## homogenise data structure
    if(class(data[[1]])[1] == "eseis") {
      
      ## set eseis flag
      eseis_class <- TRUE
      
      ## store initial object
      eseis_data <- data
      
      ## extract signal vectors
      data <- lapply(X = data, FUN = function(X) {
        
        X$signal
      })
      
      ## convert signal vector list to matrix
      data <- do.call(rbind, data)
      
    } else {
      
      ## set eseis flag
      eseis_class <- FALSE
    }
    
   ## convert degrees to radians
    angle <- angle * pi / 180
    
    ## build rotation matrix
    rot <- rbind(c(0, -sin(angle), cos(angle)),
                 c(0, cos(angle), sin(angle)),
                 c(1, 0, 0))
    
    ## homogenise input data sets
    if(nrow(data) == 2) {
      data <- rbind(data, 
                    rep(0, ncol(data)))
    }
    
    ## rotate signal traces
    data_out <- t(t(data) %*% rot)
    
    ## prepare output data
    data_out <- data_out[3:1,]

  ## optionally rebuild eseis object
  if(eseis_class == TRUE) {
    
    ## calculate function call duration
    eseis_duration <- as.numeric(difftime(time1 = Sys.time(), 
                                          time2 = eseis_t_0, 
                                          units = "secs"))
    
    
    ## ASSIGN ROTATED SIGNALS TO ORIGINAL ESEIS OBJECTS
    for(i in 1:length(eseis_data)) {
      
      eseis_data[[i]]$signal <- data_out[i,]
      
      ## update object history
      eseis_data[[i]]$history[[length(eseis_data[[i]]$history) + 1]] <- 
        list(time = Sys.time(),
             call = "signal_rotate()",
             arguments = eseis_arguments,
             duration = eseis_duration)
      names(eseis_data[[i]]$history)[length(eseis_data[[i]]$history)] <- 
        as.character(length(eseis_data[[i]]$history))
    }
    
    ## assign eseis object to output data set
    data_out <- eseis_data
  }

  ## return output
  return(data_out)
}