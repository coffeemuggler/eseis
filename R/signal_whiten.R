#' Spectral whitening of a signal
#' 
#' Thsi function performs spectral whitening of a signal. If the input signal 
#' is not already a FFT vector (or Fast Fourier transformed \code{eseis} 
#' object), i.e., \code{fft = TRUE} it is transformed and multiplied with a 
#' Hanning window to avoid spectral leakage. The magnitude of the FFT signal 
#' is set to 1 throughout and the pahse is extracted. Both vectors are 
#' combined (magnitude times exponential values of imaginary phase). If 
#' \code{fft == FALSE} the result is transformed back to the time domain and 
#' returned.
#' 
#' The function is a simplified version of the Matlab function whitening (see: 
#' https://de.mathworks.com/matlabcentral/fileexchange/65345-spectral-whitening).
#' 
#' @param data \code{eseis} object, \code{numeric} vector or list of 
#' objects, data set to be processed.
#' 
#' @param fft \code{Logical} value, optin to run the function with an already 
#' Fast Fourier transformed data set. Default is \code{FALSE}, i.e., a normal 
#' time series is FFT processed, manipulated and back transformed.
#' 
#' @return \code{Numeric} vector or list of vectors, Spectrally whitened data 
#' set.
#' 
#' @author Michael Dietze
#' 
#' @keywords eseis
#' 
#' @examples
#' 
#' ## load and detrend example data set
#' data(rockfall)
#' data <- signal_detrend(rockfall_eseis)
#' 
#' ## whiten data set
#' data_whitened <- signal_whiten(data = data)
#' 
#' ## plot spectra of the two data sets
#' par(mfcol = c(2, 1))
#' plot_spectrum(signal_spectrum(data))
#' plot_spectrum(signal_spectrum(data_whitened))
#' 
#' @export signal_whiten
#' 
signal_whiten <- function(
  
  data, 
  fft = FALSE
) {
  
  ## check data structure
  if(class(data) == "list") {
    
    ## apply function to list
    data_out <- lapply(X = data, 
                       FUN = eseis::signal_whiten)
    
    ## return output
    return(data_out)
    
  } else {
    
    ## get start time
    eseis_t_0 <- Sys.time()
    
    ## collect function arguments
    eseis_arguments <- list(data = "")
    
    ## check if input object is of class eseis
    if(class(data) == "eseis") {
      
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
    
    ## define Hanning window size
    n_hanning <- eseis_data$meta$n - 1
    
    ## build Hanning window vector
    hanning <- 0.5 - 0.5 * cos(2 * pi * (0:n_hanning) / n_hanning)
    
    ## if necessary FFT transform data set
    if(fft == FALSE) {
      
      data_fft <- fftw::FFT(data)
    } else {
      
      data_fft <- data
    }
    
    ## apply Hanning window
    data_fft <- data_fft * hanning
    
    ## Set signal magnitude to one
    magnitude <- rep(1, length(data_fft))
    
    ## get signal phase
    phase <- Arg(data_fft)
    
    ## whiten signal
    data_whitened <- magnitude * exp(complex(real = 0, 
                                             imaginary = 1) * phase)
    
    ## optionally, back transform data set
    if(fft == FALSE) {
      
      data_out <- Re(fftw::IFFT(data_whitened))
    } else {
      
      data_out <- data_whitened
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
             call = "signal_whiten()",
             arguments = eseis_arguments,
             duration = eseis_duration)
      names(eseis_data$history)[length(eseis_data$history)] <- 
        as.character(length(eseis_data$history))
      
      ## assign eseis object to output data set
      data_out <- eseis_data
    }
    
    ## return whitened data set
    return(invisible(data_out)) 
  }
}
