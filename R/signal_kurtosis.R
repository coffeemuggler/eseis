#' Calculate signal kurtosis
#' 
#' This function calculates the running kurtosis of a seismic signal 
#' and returns that characteristic function.
#'
#' @param data \code{eseis} object, \code{numeric} vector or list of 
#' objects, data set to be processed.
#' 
#' @param window \code{Numeric} value, size of the running window, in 
#' number of samples
#' 
#' @return \code{Numeric} running kurtosis of the input signal.
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
#' ## calculate kurtosis
#' rockfall_kurtosis <- signal_kurtosis(data = rockfall_eseis,
#'                                      window = 200)
#' 
#' @export signal_kurtosis

signal_kurtosis <- function(
    data,
    window = 200
) {
  
  if(missing(window) == TRUE) {
    stop("No window provided!")
  }
  
  ## check data structure
  if(class(data)[1] == "list") {
    
    ## apply function to list
    data_out <- lapply(X = data, 
                       FUN = eseis::signal_kurtosis,
                       window = window)
    
    ## return output
    return(data_out)
    
  } else {
    
    ## get start time
    eseis_t_0 <- Sys.time()
    
    ## collect function arguments
    eseis_arguments <- list(data = "",
                            window = window)
    
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
    
    ## calculate running kurtosis vector
    data_out <- .kurtosis_event(x = data, k = window)
    
    ## remove edge effects
    n_smp <- length(data_out)
    data_out[1] <- data_out[2]
    data_out[(n_smp - window):n_smp] <- data_out[n_smp - window - 1]
    
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
             call = "signal_kurtosis()",
             arguments = eseis_arguments,
             duration = eseis_duration)
      names(eseis_data$history)[length(eseis_data$history)] <- 
        as.character(length(eseis_data$history))
      
      ## update data type
      eseis_data$meta$type = "characteristic"
      
      ## assign eseis object to output data set
      data_out <- eseis_data
    }
    
    ## return data set
    return(invisible(data_out)) 
  }
}