#' Calculate signal-to-noise-ratio (SNR).
#' 
#' The function calculates the signal-to-noise ratio of an input signal 
#' vector as the ratio between mean and max.
#' 
#' @param data \code{eseis} object, \code{numeric} vector or list of 
#' objects, data set to be processed.
#' 
#' @param scale \code{Character} value, scale of the output. One out of 
#' \code{"lin"} (linear) and \code{"dB"} (decibel). Default is \code{"lin"}.
#' 
#' @param detrend \code{Logical} value, optionally detrend data set before
#' calculating snr. Default is \code{FALSE}
#' 
#' @param envelope \code{Logical} value, optionally calculate the signal 
#' envelope before calculating the SNR. Default is \code{FALSE}.
#' 
#' @param method \code{Character} value, method used to calculate the SNR. 
#' Available methods are \code{"max-mean"} (Ratio of maximum and mean signal),
#' \code{"mean-sd"} (Ratio of mean and standard deviation). Default is 
#' \code{max-mean}.
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
#' ## remove mean and calculate envelope beforehand
#' x_prep <- signal_envelope(signal_detrend(rockfall_eseis))
#' 
#' ## calculate snr
#' snr <- signal_snr(data = x_prep)
#' print(snr$snr)
#' 
#' ## calculate snr with preprocessing during function call, and in dB scale
#' snr_dB <- signal_snr(data = rockfall_eseis, detrend = TRUE, 
#'                      envelope = TRUE, scale = "dB")
#' print(snr_dB$snr)
#' 
#' @export signal_snr

signal_snr <- function(
  data,
  scale = "lin",
  detrend = FALSE,
  envelope = FALSE,
  method = "max-mean"
) {

  ## check data structure
  if(class(data)[1] == "list") {
    
    ## apply function to list
    data_out <- lapply(X = data, 
                       FUN = eseis::signal_snr, 
                       scale = scale,
                       detrend = detrend,
                       envelope = envelope,
                       method = method)
    
    ## return output
    return(data_out)
  } else {
    
    ## get start time
    eseis_t_0 <- Sys.time()
    
    ## collect function arguments
    eseis_arguments <- list(data = "",
                            scale = scale,
                            detrend = detrend,
                            envelope = envelope,
                            method = method)
    
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
      
      data <- eseis::signal_detrend(data = data)
    }
    
    ## optionally calculate envelope
    if(envelope == TRUE) {
      
      data <- eseis::signal_envelope(data = data)
    }
    
    ## check method keywords
    if(method %in% c("max-mean", "mean-sd") == FALSE) {
      
      stop("Method keyword not supported!")
      
    } else if(method == "max-mean") {
     
      ## calculate SNR
      data_out <- abs(max(data, na.rm = TRUE) / mean(data, na.rm = TRUE)) 
      
    } else if(method == "mean-sd") {
      
      ## calculate SNR
      data_out <- abs(mean(data, na.rm = TRUE) / sd(data, na.rm = TRUE))
    }
    
    ## optionally scale data set
    if(scale %in% c("lin", "dB") == FALSE) {
      
      stop("Scale keyword not supported!")
      
    } else if(scale == "lin") {
      
      data_out <- data_out
      
    } else if(scale == "dB") {
      
      data_out <- 10 * log10(data_out)
      
    }
    
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