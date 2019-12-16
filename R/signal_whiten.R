#' Perform spectral whitening of a signal vector
#' 
#' The function normalises the input signal within a given frequency 
#' window. If a time series is provided, it is converted to the spectral 
#' domain, whitening is performed, and it is transformed back to the time 
#' domain.
#' 
#' @param data \code{eseis} object, or \code{complex} vector, data set to 
#' be processed. 
#' 
#' @param f \code{Numeric} vector of length two, frequency window within 
#' which to normalise. If omitted, the entire bandwidth is normalised.
#' 
#' @param dt \code{Numeric} value, sampling period. Only needed if the input 
#' object is not an \code{eseis} object
#' 
#' @return \code{Numeric} vector or eseis object, whitened signal vector.
#' 
#' @author Michael Dietze
#' 
#' @keywords eseis
#' 
#' @examples
#' 
#' ## load example data set
#' data("rockfall")
#' 
#' ## whiten data set between 10 and 30 Hz
#' rockfall_2 <- signal_whiten(data = rockfall_eseis, 
#'                             f = c(10, 30))
#'                             
#' ## plot whitened data set
#' plot(rockfall_2)
#' 
#' @export signal_whiten
#' 
signal_whiten <- function(
  data,
  f,
  dt
) {
  
  ## check/set dt
  if(missing(dt) == TRUE && class(data)[1] != "eseis") {
    
    if(class(data[[1]])[1] != "eseis") {
      
      warning("Sampling frequency missing! Set to 1/200")
    }
    
    dt <- 1 / 200
    
  } else if(missing(dt) == TRUE){
    
    dt <- data$meta$dt
  }
  
  ## check/set frequency window
  if(missing(f) == TRUE) {
    
    f <- NA
  }
  
  ## check data structure
  if(class(data)[1] == "list") {
    
    ## apply function to list
    data_out <- lapply(X = data, 
                       FUN = eseis::signal_whiten,
                       dt = dt,
                       f = f)
    
    ## return output
    return(data_out)
  } else {
    
    ## get start time
    eseis_t_0 <- Sys.time()
    
    ## collect function arguments
    eseis_arguments <- list(data = "",
                            dt = dt,
                            f = f)
    
    ## check if input object is of class eseis
    if(class(data)[1] == "eseis") {
      
      ## set eseis flag
      eseis_class <- TRUE
      
      ## store initial object
      eseis_data <- data
      
      ## extract signal vector
      data <- eseis_data$signal
      
      ## extract number of samples
      n <- eseis_data$meta$n
      
      ## extract data type
      type <- eseis_data$meta$type
      
    } else {
      
      ## set eseis flag
      eseis_class <- FALSE
      
      ## get number of samples
      n <- length(data)
      
      ## set data type
      type <- "spectrum"
    }
    
    ## optionally go to frequency domain
    if(type == "waveform") {
      
      ## calculate FFT
      data_fft <- fftw::FFT(x = data)
      
    } else {
      
      ## maintain data set
      data_fft <- data
    }
    
    ## calculate normalisation vector
    if(is.na(f[1]) == TRUE) {
      
      norm <- rep(1, times = n)
    } else {
      
      ## create frequency vector
      f_data <- seq(from = 1/dt / length(data), 
                    to = 1/dt, 
                    by = 1/dt / length(data))
      
      ## get corner frequency boundaries
      boundaries <- (1:length(f_data))[f_data >= f[1] & f_data <= f[2]]
      boundaries <- c(boundaries[1], boundaries[length(boundaries)])
      
      ## calculate step size for rounding of corners
      stepsize <- max(c(3, round(abs(boundaries[1] - boundaries[2]) / 100)))
      
      ## calculate sinusoidal corner smoother
      smoother <- (sin(seq(from = -1, 
                           to = 1, 
                           by = 2 / (stepsize - 1)) * pi / 2) + 1) / 2
      
      ## initiate normalisation vector
      norm <- rep(0, length(f_data))
      
      ## set valid frequency range to 1
      norm[boundaries[1]:boundaries[2]] <- 1
      
      ## round corners
      norm[(boundaries[1] - stepsize + 1):boundaries[1]] <- smoother
      norm[boundaries[2]:(boundaries[2] + stepsize - 1)] <- rev(smoother)
      norm <- norm + rev(norm)
    }
    
    ## apply normalisation
    data_norm <- data_fft / abs(data_fft) * norm
    
    ## go back to time domain
    data_out <- Re(fftw::FFT(x = data_norm, 
                             inverse = TRUE)) / length(f_data)
    
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
             call = "signal_whiten()",
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