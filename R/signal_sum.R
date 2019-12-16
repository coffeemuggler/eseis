#' Calculate signal vector sum.
#' 
#' The function calculates the vector sum of the input signals.
#' 
#' @param \dots \code{Numeric} vectors or \code{eseis} objects, input 
#' signal, that must be of the same length.
#' 
#' @return \code{Numeric} vector, signal vector sum.
#' @author Michael Dietze
#' @keywords eseis
#' @examples
#' 
#' ## create random vectors
#' x <- runif(n = 1000, min = -1, max = 1)
#' y <- runif(n = 1000, min = -1, max = 1)
#' z <- runif(n = 1000, min = -1, max = 1)
#' 
#' ## calculate vector sums
#' xyz <- signal_sum(x, y, z)
#'                      
#' @export signal_sum
signal_sum <- function(
  ...
) {
  
  ## get start time
  eseis_t_0 <- Sys.time()
  
  ## collect function arguments
  eseis_arguments <- list(data = "")
  
  ## convert input data to list
  data <- list(...)
  
  ## check if input object is of class eseis
  if(class(data[[1]])[1] == "eseis") {
    
    ## store initial object
    eseis_data <- data[[1]]
    
    ## extract signal vectors
    data <- lapply(X = data, FUN = function(X) {
      
      X$signal
    })
    
    ## convert list to matrix
    data <- do.call(rbind, data)
    
    ## calculate vector sum
    data_sum <- sqrt(colSums(data^2))
    
    ## assign aggregated signal vector
    eseis_data$signal <- data_sum
    
    ## calculate function call duration
    eseis_duration <- as.numeric(difftime(time1 = Sys.time(), 
                                          time2 = eseis_t_0, 
                                          units = "secs"))
    
    ## update object history
    eseis_data$history[[length(eseis_data$history) + 1]] <- 
      list(time = Sys.time(),
           call = "signal_sum()",
           arguments = eseis_arguments,
           duration = eseis_duration)
    names(eseis_data$history)[length(eseis_data$history)] <- 
      as.character(length(eseis_data$history))

    ## assign eseis object to output data set
    data_sum <- eseis_data
    
  } else {
    
    ## homogenise input data
    data <- do.call(rbind, data)
    
    ## calculate vector sum
    data_sum <- sqrt(colSums(data^2))
  }
  
  ## return output
  return(data_sum)
}