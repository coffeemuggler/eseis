#' Calculate signal envelope.
#' 
#' The function calculates envelopes of the input signals as 
#' cosine-tapered envelope of the Hilbert-transformed signal. The signal
#' should be detrended and/or the mean should be removed before processing.
#' 
#' @param data \code{eseis} object, \code{numeric} vector or list of 
#' objects, data set to be processed.
#' 
#' @param p \code{Numeric} value, proportion of the signal to be tapered,
#' default is \code{0}.
#' 
#' @return \code{Numeric} vector or list of vectors, signal envelope.
#' @author Michael Dietze
#' @keywords eseis
#' @examples
#' 
#' ## load example data set
#' data(rockfall)
#' 
#' ## detrend data set
#' rockfall_detrend <- signal_detrend(data = rockfall_eseis)
#' 
#' ## calculate envelope
#' rockfall_envelope <- signal_envelope(data = rockfall_detrend)
#' 
#' ## plot envelope
#' plot_signal(data = rockfall_envelope)
#'                      
#' @export signal_envelope
signal_envelope <- function(
  data,
  p = 0
) {
  
  ## check data structure
  if(class(data)[1] == "list") {
    
    ## apply function to list
    data_out <- lapply(X = data, 
                       FUN = eseis::signal_envelope, 
                       p = p)
    
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
    
    ## calculate Hilbert transform
    data_hilbert <- signal_hilbert(data = data)
    
    ## calculate absolute values
    data_out <- abs(data_hilbert)
    
    ## apply taper
    if(p > 0) {
      
      data_out <- signal_taper(data = data_out, 
                               p = p)
    }
    
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
             call = "signal_envelope()",
             arguments = eseis_arguments,
             duration = eseis_duration)
      names(eseis_data$history)[length(eseis_data$history)] <- 
        as.character(length(eseis_data$history))
      
      ## update data type
      eseis_data$meta$type = "envelope"
      
      ## assign eseis object to output data set
      data_out <- eseis_data
    }
    
    ## return output
    return(data_out) 
  }
}