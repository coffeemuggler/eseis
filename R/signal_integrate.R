#' Integrate a signal vector.
#' 
#' The function integrates a signal vector to convert values from velocity to 
#' displacement. Two methods are available 
#' 
#' @param data \code{Numeric} vector or list of vectors, input signal vector.
#' 
#' @param dt \code{Numeric} scalar, sampling rate.
#' 
#' @param method \code{Character} scalar, method used for integration. One out 
#' of \code{"fft"} (convert in the frequency domain) and \code{"trapezoid"} 
#' (integrate using the trapezoidal rule). Default is \code{"fft"}.
#' 
#' @param waterlevel \code{Numeric} scalar, waterlevel value for frequency
#' division, default is \code{10^-6}. Only used when \code{method = "fft"}.
#' 
#' @return \code{Numeric} vector or list of vectors, integrated signal.
#' @author Michael Dietze
#' @keywords eseis
#' @examples
#' 
#' ## load example data set
#' data(rockfall)
#' 
#' ## deconvolve signal
#' rockfall_decon <- signal_deconvolve(data = rockfall,
#'                                     dt = 1/200, 
#'                                     sensor = "TC120s", 
#'                                     logger = "Cube3ext")
#'                                     
#' ## integrate signal
#' rockfall_int <- signal_integrate(data = rockfall_decon,
#'                                  dt = 1/200)
#'                                  
#' ## Note that usually the signal should be filtered prior to integration.
#'                      
#' @export signal_integrate
signal_integrate <- function(
  data,
  dt,
  method = "fft",
  waterlevel = 10^-6
) {
  
  if(missing(dt) == TRUE) {
    
    dt <- 0.01
    warning("No dt provided. Set to 0.01 s by default!")
  }
  
  ## check data structure
  if(class(data) == "list") {
    
    ## apply function to list
    data_out <- lapply(X = data, 
                       FUN = eseis::signal_integrate, 
                       dt = dt,
                       method = method)
    
    ## return output
    return(data_out)
  } else {
    
    if(method == "fft") {
      
      ## remove mean from data set
      data_demean <- signal_demean(data = data)
      
      ## add zeros to reach even power of two number
      data_padd <- signal_padd(data = data_demean)
      
      ## define frequency vector
      if ((length(data_padd)%%2) == 1) {
        f <- c(seq(0, (length(data_padd) - 1) / 2), 
               seq(-(length(data_padd) - 1) / 2, -1)) / (length(data_padd) * dt)
      } else {
        f = c(seq(0, length(data_padd) / 2), 
              seq(-length(data_padd) / 2 + 1, -1)) / (length(data_padd) * dt)
      }
      
      ## calculate fast Fourier transform
      x_fft <- stats::fft(z = data_padd)
      
      ## calculate complex respone vector
      f_comp <- 2 * pi * f * dt * complex(real = 0, imaginary = 1)
      
      ## replace zeros by waterlevel value
      f_comp[Im(f_comp) == 0] <- 2 * pi * dt * complex(real = 0, 
                                                       imaginary = waterlevel)
      
      ## define helper vector
      f_comp_inverse <- complex(real = 1, imaginary = 0) / f_comp
      
      ## calculate inverse fast Fourier transform
      data_integrate <- Re(stats::fft(x_fft * f_comp_inverse, 
                                      inverse = TRUE) / length(data_padd))
      
      ## truncate data set to original length
      data_out <- data_integrate[1:length(data)]
      
    } else if(method == "trapezoid") {
      
      ## remove mean from data set
      data_demean <- signal_demean(data = data)
      
      ## check/adjust NA-values
      if(sum(is.na(data_demean)) > 0) {
        
        warning("Data contains NA-values. NA-values are set to 0.")
        
        data_demean[is.na(data_demean)] <- 0
      }
      
      ## define data set length
      n <- length(data)
      
      data_out <- c(0, 
                    cumsum(x = dt * 1 / 2 * 
                             data_demean[1:(n - 1)] + data_demean[2:n]))
    } else {
      
      warning("Method keyword not support! Initial data set is returned.")
      data_out <- data
    }
    
    ## return output
    return(data_out)
  }
 
}