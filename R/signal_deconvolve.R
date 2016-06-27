#' Deconvolve a signal vector.
#' 
#' The function removes the instrument response from a signal vector.
#' 
#' @param data \code{Numeric} vector or list of vectors, input signal vector.
#' 
#' @param dt \code{Numeric} scalar, sampling rate.
#' 
#' @param output \code{Character} scalar, output data type. One out 
#' of \code{"velocity"}, \code{"displacement"} or \code{"acceleration"}, 
#' default is \code{"velocity"}. The other keywords are not yet supported.
#' 
#' @param sensor \code{Character} scalar, seismic sensor name. Must be 
#' present in the sensor library (\code{list_sensor}) or parameters must be 
#' added manually. Default is \code{"TC120s"}.
#' 
#' @param logger \code{Character} scalar, seismic logger name. Must be 
#' present in the logger library (\code{list_logger}) or parameters must be
#' added manually. Default is \code{"Cube"}.
#' 
#' @param p \code{Numeric} scalar, proportion of signal to be tapered. Default
#' is\code{10^-6}.
#' 
#' @param waterlevel \code{Numeric} scalar, waterlevel value for frequency
#' division, default is \code{10^-6}.
#' 
#' @return \code{Numeric} vector or list of vectors, deconvolved signal.
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
#' ## plot time series
#' plot(x = t, 
#'      y = rockfall_decon, 
#'      main = "Rockfall, deconvolved signal", 
#'      ylab = "m/s", 
#'      type = "l")
#'                      
#' @export signal_deconvolve
signal_deconvolve <- function(
  data,
  dt,
  output = "velocity",
  sensor = "TC120s",
  logger = "Cube3ext",
  p = 10^-6,
  waterlevel = 10^-6
) {
  
  ## check data structure
  if(class(data) == "list") {
    
    ## apply function to list
    data_out <- lapply(X = data, 
                       FUN = eseis::signal_deconvolve, 
                       dt = dt,
                       output = output,
                       sensor = sensor,
                       p = p,
                       waterlevel = waterlevel)
    
    ## return output
    return(data_out)
  } else {
    
    ## get sensor information
    sensor <- try(list_sensor()[[sensor]], silent = TRUE)
    
    if(class(sensor) == "try-error") {
      stop("Sensor keyword not in library! Consider adding manually.")
    }
    
    poles <- sensor$poles
    zeros <- sensor$zeros
    s <- sensor$s
    k <- sensor$k
    
    ## get sensor information
    logger <- try(list_logger()[[logger]], silent = TRUE)
    
    if(class(logger) == "try-error") {
      stop("Logger keyword not in library! Consider adding manually.")
    }
    
    AD <- logger$AD
    
    ## detrend data set
    data_detrend <- signal_detrend(data = data)
    
    ## remove mean from data set
    data_demean <- signal_demean(data = data_detrend)
    
    ## taper data set
    data_taper <- signal_taper(data = data_demean, 
                               p = p)
    
    ## add zeros to reach even power of two number
    data_padd <- signal_padd(data = data_taper)
    
    ## make frequency vector
    if(output == "velocity") {
      
      if ((length(data_padd)%%2) == 1) {
        f <- c(seq(0, (length(data_padd) - 1) / 2), 
               seq(-(length(data_padd) - 1) / 2, -1)) / (length(data_padd) * dt)
      } else {
        f = c(seq(0, length(data_padd) / 2), 
              seq(-length(data_padd) / 2 + 1, -1)) / (length(data_padd) * dt)
      }  
    } else {
      
      f <- seq(from = 1, to = length(data_padd)) * 1 / (length(data_padd) * dt)  
    }
    
    ## calculate Fourier transform
    x_fft <- stats::fft(z = data_padd)
    
    ## get polynomial form of poles and zeros
    poles_poly <- Re(signal::poly(x = poles))
    zeros_poly <- Re(signal::poly(x = zeros))
    
    ## calculate complex circular frequency vector
    w_comp <- complex(real = 0, imaginary = 1) * (2 * pi * f)
    
    ## calculate transfer function
    h <- outer(w_comp, (length(zeros_poly) - 1):0, "^") %*% zeros_poly / 
      outer(w_comp, (length(poles_poly) - 1):0, "^") %*% poles_poly * s
    
    ## calculate waterlevel factor
    gamma <- max(Re(h * Conj(h))) * waterlevel
    
    ## deconvolve signal  
    data_decon <- x_fft * Conj(h) / Re(h * Conj(h) + gamma)
    
    ## invert deconvolved signal
    data_inverse <- Re(stats::fft(data_decon, 
                                  inverse = TRUE) / length(data_decon))
    
    ## correct for A/D-ratio and sensitivity factor
    data_inverse <- (data_inverse * AD) / s
    
    ## truncate signal to original length
    data_inverse <- data_inverse[1:length(data)]
    
    ## remove mean from output signal
    data_out <- signal_demean(data_inverse)
    
    ## return output signal
    return(data_out)    
  }
 }