#' Differentiate a signal vector
#' 
#' The function integrates a signal vector to, for example convert values from 
#' displacement to velocity.
#' 
#' @param data \code{eseis} object, \code{numeric} vector or list of 
#' objects, data set to be processed.
#' 
#' @return Derivative of the input data set.
#' 
#' @author Michael Dietze
#' @keywords eseis
#' @examples
#' 
#' ## load example data set
#' data(rockfall)
#' 
#' ## detrend and demean data set
#' s <- signal_demean(signal_detrend(data = rockfall_eseis))
#' 
#' ## differentiate
#' s_d = signal_differentiate(data = s)
#' 
#' ## plot result
#' plot(s_d)
#' 
#' @export signal_differentiate

signal_differentiate <- function( 
    
  data
  
) {
  
  ## check data structure
  if(class(data)[1] == "list") {
    
    ## apply function to list
    data_out <- lapply(X = data, 
                       FUN = eseis::signal_differentiate)
    
    ## return output
    return(data_out)
  } else {
    
    ## get start time
    eseis_t_0 <- Sys.time()
    
    ## collect function arguments
    eseis_arguments <- list(data = "")
    
    ## check if input object is of class eseis
    if(class(data)[1] == "eseis") {
      
      ## set eseis flag
      eseis_class <- TRUE
      
      ## store initial object
      eseis_data <- data
      
      ## extract signal vector
      data <- eseis_data$signal
    } else {
      
      ## set eseis flag
      eseis_class <- FALSE
    }
    
    ## get number of samples
    n <- length(data)

    ## calculate first derivative    
    data_out <- (data[c(2:n, 1)] - data[c(n, 1:(n-1))]) / 2
    
    ## assign first and last values
    data_out[1] = data[2] - data[1]
    data_out[n] = data[n] - data[n - 1]
    
    ## optionally rebuild eseis object
    if(eseis_class == TRUE) {
      
      ## assign aggregated signal vector
      eseis_data$signal <- data_out
      
      ## calculate function call duration
      eseis_duration <- as.numeric(difftime(time1 = Sys.time(), 
                                            time2 = eseis_t_0, 
                                            units = "secs"))
      
      ## update object history
      eseis_data$history[[length(eseis_data$history) + 1]] <- 
        list(time = Sys.time(),
             call = "signal_differentiate()",
             arguments = eseis_arguments,
             duration = eseis_duration)
      names(eseis_data$history)[length(eseis_data$history)] <- 
        as.character(length(eseis_data$history))
      
      ## assign eseis object to output data set
      data_out <- eseis_data
    }
    
    ## return output
    return(data_out)
  }
}
  