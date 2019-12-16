#' Pad signal with zeros.
#' 
#' The function adds zeros to the input vector to reach a length,
#' corresponding to the next higher power of two.
#' 
#' @param data \code{eseis} object, \code{numeric} vector or list of 
#' objects, data set to be processed.
#' 
#' @return \code{Numeric} vector or list of vectors, signal vector with 
#' added zeros.
#' 
#' @author Michael Dietze
#' 
#' @keywords eseis
#' 
#' @examples
#' 
#' ## load example data set
#' data(rockfall)
#' 
#' ## pad with zeros
#' rockfall_pad <- signal_pad(data = rockfall_eseis)
#' 
#' ## compare lengths
#' rockfall_eseis$meta$n
#' rockfall_pad$meta$n
#'                      
#' @export signal_pad
signal_pad <- function(
  
  data
) {
  
  ## check data structure
  if(class(data)[1] == "list") {
    
    ## apply function to list
    data_out <- lapply(X = data, 
                       FUN = eseis::signal_pad)
    
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
    
    ## get number of samples to reach next highest power two length 
    n <- stats::nextn(n = length(data), factors = 2)
    
    ## create output vector
    data_out <- rep(x = 0, times = n)
    
    ## add original values
    data_out[1:length(data)] <- data
    
    ## optionally rebuild eseis object
    if(eseis_class == TRUE) {
      
      ## assign aggregated signal vector
      eseis_data$signal <- data_out
      
      ## update sample value
      eseis_data$meta$n <- length(data_out)
      
      ## calculate function call duration
      eseis_duration <- as.numeric(difftime(time1 = Sys.time(), 
                                            time2 = eseis_t_0, 
                                            units = "secs"))
      
      ## update object history
      eseis_data$history[[length(eseis_data$history) + 1]] <- 
        list(time = Sys.time(),
             call = "signal_pad()",
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
