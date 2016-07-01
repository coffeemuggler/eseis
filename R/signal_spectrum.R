#' Calculate the spectrum of a time series
#' 
#' The spectral density estimate of the time series is calculated using 
#' different approaches.
#' 
#' @param data \code{Numeric} vector or list of vectors, data set to be 
#' processed.
#' 
#' @param dt \code{Numeric} scalar, sampling period. If omitted, \code{dt} 
#' is set to 1/200.
#' 
#' @param method \code{Character} scalar, calculation method. One out of 
#' \code{"periodogram"} , \code{"autoregressive"} and \code{"multitaper"}, 
#' default is \code{"periodogram"}.
#' 
#' @param \dots Additional arguments passed to the function. See 
#' \code{\link{spec.pgram}}, \code{\link{spec.ar}}, \code{\link{spec.mtm}}.
#' 
#' @return \code{Data frame} with spectrum and frequency vector 
#' @author Michael Dietze
#' @keywords eseis
#' @examples
#' 
#' ## load example data set
#' data(rockfall, envir = environment())
#' 
#' ## calculate spectrum with standard setup
#' s <- signal_spectrum(data = rockfall, dt = 1/200)
#' 
#' ## plot spectrum
#' plot(s, type = "l", log = "y")
#' 
#' @export signal_spectrum
signal_spectrum <- function(
  data,
  dt,
  method = "periodogram",
  ...
) {
  
  ## check data structure
  if(class(data) == "list") {
    
    ## apply function to list
    data_out <- lapply(X = data, 
                       FUN = signal_spectrum,
                       dt = dt, 
                       method = method,
                       ...)
    
    ## return output
    return(data_out)
  } else {
    
    ## check/set dt
    if(missing(dt) == TRUE) {
      
      warning("Sampling frequency missing! Set to 1/200")
      
      dt <- 1/200
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
    }
    
    ## return output
    return(data_out)
  }
}