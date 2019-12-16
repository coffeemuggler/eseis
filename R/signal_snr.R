#' Calculate signal-to-noise-ratio.
#' 
#' The function calculates the signal-to-noise ratio of an input signal 
#' vector as the ratio between mean and max.
#' 
#' @param data \code{eseis} object, \code{numeric} vector or list of 
#' objects, data set to be processed.
#' 
#' @param detrend \code{Logical} value, optionally detrend data set before
#' calcualting snr.
#' 
#' @return \code{Numeric} value, signal-to-noise ratio.
#' 
#' @author Michael Dietze
#' @keywords eseis
#' @examples
#' 
#' ## load example data set
#' data(rockfall)
#' 
#' ## calculate snr with detrend option off and on
#' snr <- signal_snr(data = rockfall_eseis)
#' print(snr$snr)
#' 
#' snr <- signal_snr(data = rockfall_eseis, 
#'                   detrend = TRUE)
#' print(snr$snr)
#'                      
#' @export signal_snr
signal_snr <- function(
  data,
  detrend = FALSE
) {

  ## check data structure
  if(class(data)[1] == "list") {
    
    ## apply function to list
    data_out <- lapply(X = data, 
                       FUN = eseis::signal_snr, 
                       detrend = detrend)
    
    ## return output
    return(data_out)
  } else {
    
    ## get start time
    eseis_t_0 <- Sys.time()
    
    ## collect function arguments
    eseis_arguments <- list(data = "",
                            detrend = detrend)
    
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
    
    ## optionally detrend data set
    if(detrend == TRUE) {
      
      data <- signal_detrend(data = data)
    }
    
    ## calculate SNR
    data_out <- abs(max(data, na.rm = TRUE) / mean(data, na.rm = TRUE))
    
    ## optionally rebuild eseis object
    if(eseis_class == TRUE) {
      
      ## assign aggregated signal vector
      eseis_data$signal <- data_out
      
      ## rename list element
      names(eseis_data)[1] <- "snr"
      
      ## calculate function call duration
      eseis_duration <- as.numeric(difftime(time1 = Sys.time(), 
                                            time2 = eseis_t_0, 
                                            units = "secs"))
      
      ## update object history
      eseis_data$history[[length(eseis_data$history) + 1]] <- 
        list(time = Sys.time(),
             call = "signal_snr()",
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