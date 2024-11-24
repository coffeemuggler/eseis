#' Calculate the spectrum of a time series
#' 
#' The power spectral density estimate of the time series is calculated using 
#' different approaches.
#' 
#' If the \code{res} option is used, the frequency and power vectors will be 
#' interpolated using a spline interpolator, using equally spaced frequency 
#' values. If desired, the additional option \code{log = TRUE} can be used
#' to interpolate with log spaced frequency values.
#' 
#' @param data \code{eseis} object, \code{numeric} vector or list of 
#' objects, data set to be processed.
#' 
#' @param dt \code{Numeric} value, sampling period. If omitted, \code{dt} 
#' is set to 1/200. Only needed if \code{data} is no \code{eseis} object.
#' 
#' @param method \code{Character} value, calculation method. One out of 
#' \code{"periodogram"} and \code{"autoregressive"}. 
#' default is \code{"periodogram"}.
#' 
#' @param n \code{Numeric} value, optional number of samples in 
#' running window used for smoothing the spectrogram. Only applied if a 
#' number is provided. Smoothing is performed as running mean.
#' 
#' @param res \code{Numeric} value, optional resolution of the spectrum, 
#' i.e. the number of power and frequency values. If omitted, the full 
#' resolution is returned. If used, a spline interpolation is performed.
#' 
#' @param log \code{Logical} value, option to interpolate the spectrum with 
#' log spaced frequency values. Default is \code{FALSE}.
#' 
#' @param \dots Additional arguments passed to the function. 
#' 
#' @return \code{Data frame} with frequency and power vector 
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
#' ## calculate spectrum with standard setup
#' s <- signal_spectrum(data = rockfall_eseis)
#' 
#' ## plot spectrum
#' plot_spectrum(data = s)
#' 
#' @export signal_spectrum
#' 
signal_spectrum <- function(
  data,
  dt,
  method = "periodogram",
  n,
  res,
  log = FALSE,
  ...
) {

  ## check/set dt
  if(missing(dt) == TRUE && class(data)[1] != "eseis") {

    ## try to get object class
    class_data <- try(class(data[[1]])[1] == "eseis", 
                      silent = TRUE)
    
    if(class(class_data)[1] == "try-error " | class_data == FALSE) {
      
      warning("Sampling frequency missing! Set to 1/200")
      
      dt <- 1 / 200
    } 
    
  } else if(missing(dt) == TRUE){
    
    dt <- NULL
  }
  
  ## check/set dt
  if(missing(n) == TRUE) {
    
    n <- NULL
  }
  
  ## check/set res
  if(missing(res) == TRUE) {
    
    res <- NULL
  }

  ## check data structure
  if(class(data)[1] == "list") {
    
    ## apply function to list
    data_out <- lapply(X = data, 
                       FUN = signal_spectrum,
                       dt = dt, 
                       method = method,
                       n = n,
                       res = res,
                       ...)
    
    ## return output
    return(data_out)
    
  } else {
    
    ## get start time
    eseis_t_0 <- Sys.time()
    
    ## collect function arguments
    eseis_arguments <- list(data = "",
                            dt = dt,
                            n = n, 
                            res = res,
                            method = method)
    
    ## check if input object is of class eseis
    if(class(data)[1] == "eseis") {
      
      ## set eseis flag
      eseis_class <- TRUE
      
      ## store initial object
      eseis_data <- data
      
      ## extract signal vector
      data <- eseis_data$signal
      
      ## extract sampling period
      dt <- eseis_data$meta$dt
      
    } else {
      
      ## set eseis flag
      eseis_class <- FALSE
    }
    
    if(method == "periodogram") {
      
      ## calculate spectrum
      s <- spec.pgram(x = data, 
                      plot = FALSE, 
                      ...)
      
      ## recalculate frequency vector
      s$freq <- seq(from = 0, 
                    to = 1 / dt / 2, 
                    length.out = length(s$freq))
      
      ## recompose data set
      data_out <- data.frame(frequency = s$freq,
                             power = s$spec)
      
    } else if(method == "autoregressive") {
      
      ## calculate spectrum
      s <- spec.ar(x = data, 
                   plot = FALSE, 
                   ...)
      
      ## recalculate frequency vector
      s$freq <- seq(from = 0, 
                    to = 1 / dt / 2, 
                    length.out = length(s$freq))
      
      ## recompose data set
      data_out <- data.frame(frequency = s$freq,
                             power = s$spec)
      
    } else {
      
      stop("Keyword for method not supported!")
    }
    
    ## optionally perform smoothing
    if(is.null(n) == FALSE) {
      
      data_out$power <- caTools::runmean(x = data_out$power, k = n)
    }
    
    ## optionally perform interpolation
    if(is.null(res) == FALSE) {
      
      ## define interpolation frequency vector
      if(log == TRUE) {
        
        f_int <- exp(seq(from = log(abs(min(data_out$frequency[-1]))), 
                         to = log(abs(max(data_out$frequency))),
                         length.out = res))
      } else {
        
        f_int <- seq(from = min(data_out$frequency), 
                     to = max(data_out$frequency),
                     length.out = res)
      }
      
      if(is.null(res) == FALSE) {
        
        data_out <- data.frame(
          frequency = stats::spline(x = data_out$frequency, 
                                    y = data_out$frequency,
                                    xout = f_int)$y,
          
          power = stats::spline(x = data_out$frequency, 
                                y = data_out$power, 
                                xout = f_int)$y
        )
      }
    }
    
    ## optionally rebuild eseis object
    if(eseis_class == TRUE) {
      
      ## assign aggregated signal vector
      eseis_data$signal <- data_out
      
      ## rename output list element
      names(eseis_data)[1] <- "spectrum"
      
      ## calculate function call duration
      eseis_duration <- as.numeric(difftime(time1 = Sys.time(), 
                                            time2 = eseis_t_0, 
                                            units = "secs"))
      
      ## update object history
      eseis_data$history[[length(eseis_data$history) + 1]] <- 
        list(time = Sys.time(),
             call = "signal_spectrum()",
             arguments = eseis_arguments,
             duration = eseis_duration)
      names(eseis_data$history)[length(eseis_data$history)] <- 
        as.character(length(eseis_data$history))
      
      ## update data type
      eseis_data$meta$type = "spectrum"
      
      ## assign eseis object to output data set
      data_out <- eseis_data
    }
    
    ## return output
    return(data_out)
  }
}
