#' Interpolate a signal vector
#' 
#' The signal vector \code{data} is interpolated by an integer factor 
#' \code{n}. If an \code{eseis} object is provided, the meta data is updated. 
#' The function is a wrapper for the function \code{interp} of the package 
#' \code{signal}. Note that interpolation does not create new meaningful 
#' information but rather artefacts above the initial frequency range.
#' 
#' The function calls the function \code{signal::interp} and wraps the output
#' into the eseis object structure. The ...-argument may contain the passed 
#' argument \code{Wc} (FIR filter cutoff frequency). Note that by convention 
#' the argument \code{n} of \code{eseis::signal_interpolate} does not equal the
#' argument \code{n} of \code{signal::interp}. Rather this is the argument 
#' \code{l} that is passed as \code{n}.
#' 
#' @param data \code{eseis} object, \code{numeric} vector or list of 
#' objects, data set to be processed.
#' 
#' @param n \code{Numeric} value, number of samples to be interpolated by. 
#' Must be an integer value greater than 1. Default is  \code{2}.
#' 
#' @param l \code{Character} value, FIR filter length. For  details see 
#' documentation of \code{signal::decimate}. Default is \code{4}.
#' 
#' @param \dots, further arguments passed to \code{signal::interp}. See 
#' details.
#' 
#' @return Interpolated data set.
#' 
#' @author Michael Dietze
#' @keywords eseis
#' @examples
#' 
#' ## load example data set
#' data(rockfall)
#' 
#' ## detrend data set
#' s <- signal_detrend(data = rockfall_eseis)
#' 
#' ## interpolate by factor 2
#' s_int = signal_interpolate(data = s, n = 2)
#' 
#' ## calculate and plot spectrogram
#' p_int = signal_spectrogram(data = s_int)
#' plot(p_int)
#' 
#' @export signal_interpolate

signal_interpolate <- function( 
    
  data,
  n,
  l = 4,
  ...
) {
  
  ## extract ...-arguments
  arg <- list(...)
  
  ## check if FIR filter cutoff frequency is provided
  if("Wc" %in% names(arg) == FALSE) {
    
    Wc <- 0.5
  } else {
    
    Wc <- arg$Wc
  }
  
  ## check data structure
  if(class(data)[1] == "list") {
    
    ## apply function to list
    data_out <- lapply(X = data, 
                       FUN = eseis::signal_interpolate,
                       n = n,
                       l = l,
                       Wc = Wc)
    
    ## return output
    return(data_out)
  } else {
    
    ## get start time
    eseis_t_0 <- Sys.time()
    
    ## collect function arguments
    eseis_arguments <- list(data = "",
                            n = n,
                            l = l,
                            Wc = Wc)
    
    ## check aggregation factor
    if(signif(n) != n) {
      
      n <- round(x = n, 
                 digits = 0)
      warning("Interpolation factor rounded to integer value!")
    }
    
    if(n < 1) {
      
      n <- 1
      warning("Interpolation factor smaller than 1, set to 1 automatically!")
    }
    
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
    
    ## use decimate function from signal package
    data_int <- signal::interp(x = data, 
                               q = n, 
                               n = l, 
                               Wc = Wc)
    
    ## optionally rebuild eseis object
    if(eseis_class == TRUE) {
      
      ## assign aggregated signal vector
      eseis_data$signal <- data_int
      
      ## update number of samples
      eseis_data$meta$n <- length(data_int)
      
      ## update sampling interval
      eseis_data$meta$dt <- eseis_data$meta$dt / n
      
      ## calculate function call duration
      eseis_duration <- as.numeric(difftime(time1 = Sys.time(), 
                                            time2 = eseis_t_0, 
                                            units = "secs"))
      
      ## update object history
      eseis_data$history[[length(eseis_data$history) + 1]] <- 
        list(time = Sys.time(),
             call = "signal_interpolate()",
             arguments = eseis_arguments,
             duration = eseis_duration)
      names(eseis_data$history)[length(eseis_data$history)] <- 
        as.character(length(eseis_data$history))
      
      ## assign eseis object to output data set
      data_int <- eseis_data
    }
    
    ## return output
    return(data_int)
  }
}
  