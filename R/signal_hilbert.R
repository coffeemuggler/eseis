#' Calculate Hilbert transform.
#' 
#' The function calculates the Hilbert transform of the input signal vector.
#' 
#' @param data \code{eseis} object, \code{numeric} vector or list of 
#' objects, data set to be processed.
#' 
#' @return \code{Numeric} vector or list of vectors, Hilbert transform.
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
#' ## calculate hilbert transform
#' rockfall_h <- signal_hilbert(data = rockfall_eseis)
#' 
#' @export signal_hilbert
#' 
signal_hilbert <- function(
  data
) {
  
  ## check data structure
  if(class(data)[1] == "list") {
    
    ## apply function to list
    data_out <- lapply(X = data, 
                       FUN = eseis::signal_hilbert)
    
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
    
    ## get length of the data set
    n <- length(data)

    ## create Hilbert vector
    h <- numeric(length = n)

    ## check odd vs. even data length
    if(n %% 2 == 0) {
      h[c(1, n / 2 + 1)] <- 1
      h[2:(n / 2)] <- 2
    }
    else {
      h[1] <- 1
      h[2:((n + 1) / 2)] <- 2
    }
    
    ## get Hilbert transform
    data_out <- fftw::IFFT(h * fftw::FFT(data))
    
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
             call = "signal_hilbert()",
             arguments = eseis_arguments,
             duration = eseis_duration)
      names(eseis_data$history)[length(eseis_data$history)] <- 
        as.character(length(eseis_data$history))
      
      ## update data type
      eseis_data$meta$type = "hilbert"
      
      ## assign eseis object to output data set
      data_out <- eseis_data
    }
    
    ## return hilbert data set
    return(invisible(data_out)) 
  }
}