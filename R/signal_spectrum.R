#' Calculate the spectrum of a time series
#' 
#' The power spectral density estimate of the time series is calculated using 
#' different approaches.
#' 
#' @param data \code{eseis} object, \code{numeric} vector or list of 
#' objects, data set to be processed.
#' 
#' @param dt \code{Numeric} value, sampling period. If omitted, \code{dt} 
#' is set to 1/200. Only needed if \code{data} is no \code{eseis} object.
#' 
#' @param method \code{Character} value, calculation method. One out of 
#' \code{"periodogram"} , \code{"autoregressive"} and \code{"multitaper"}, 
#' default is \code{"periodogram"}.
#' 
#' @param \dots Additional arguments passed to the function. See 
#' \code{\link{spec.pgram}}, \code{\link{spec.ar}}, \code{\link{spec.mtm}}.
#' 
#' @return \code{Data frame} with spectrum and frequency vector 
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

  ## check data structure
  if(class(data)[1] == "list") {
    
    ## apply function to list
    data_out <- lapply(X = data, 
                       FUN = signal_spectrum,
                       dt = dt, 
                       method = method,
                       ...)
    
    ## return output
    return(data_out)
    
  } else {
    
    ## get start time
    eseis_t_0 <- Sys.time()
    
    ## collect function arguments
    eseis_arguments <- list(data = "",
                            dt = dt,
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
                             spectrum = s$spec)
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
                             spectrum = s$spec)
    } else if(method == "multitaper") {
      
      ## calculate spectrum
      s <- multitaper::spec.mtm(timeSeries = data, 
                                deltat = dt, 
                                plot = FALSE, 
                                ...)
      
      ## recalculate frequency vector
      s$freq <- seq(from = 0, 
                    to = 1 / dt / 2, 
                    length.out = length(s$freq))
      
      ## recompose data set
      data_out <- data.frame(frequency = s$freq,
                             spectrum = s$spec)
    } else {
      
      stop("Keyword for method not supported!")
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