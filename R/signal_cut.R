#' Cut signal amplitude at standard deviation-defined level.
#'
#' This function cuts the amplitude of signal parts that exceede a user 
#' defined threshold set by k times the standard deviation of the signal.
#'
#' @param data \code{eseis} object, \code{numeric} vector or list of 
#' objects, data set to be processed.
#' 
#' @param k \code{Numeric} value, multiplier of the standard deviation
#' threshold used to cut the signal amplitude. Default is \code{1} (1 sd).
#' 
#' @return \code{Numeric} vector or list of vectors, cut signal.
#' 
#' @author Michael Dietze
#' 
#' @keywords eseis
#' 
#' @examples
#' 
#' ## load example data
#' data(rockfall)
#' 
#' ## cut signal
#' rockfall_cut <- signal_cut(data = rockfall_eseis)
#' 
#' @export signal_cut
#' 
signal_cut <- function(
  data,
  k = 1
) {
  
  ## check data structure
  if(class(data)[1] == "list") {
    
    ## apply function to list
    data_out <- lapply(X = data, 
                       FUN = eseis::signal_cut,
                       k = k)
    
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
    
    ## calculate standard deviation of the data
    threshold <- sd(data) * k
    
    ## cut signal
    data[data > threshold] <- threshold
    data[data < -threshold] <- -threshold
    
    ## get Hilbert transform
    data_out <- data
    
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
             call = "signal_cut()",
             arguments = eseis_arguments,
             duration = eseis_duration)
      names(eseis_data$history)[length(eseis_data$history)] <- 
        as.character(length(eseis_data$history))
      
      ## assign eseis object to output data set
      data_out <- eseis_data
    }
    
    ## return hilbert data set
    return(invisible(data_out)) 
  }
}


