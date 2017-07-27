#' Deconvolve a signal vector.
#' 
#' The function removes the instrument response from a signal vector.
#' 
#' The function requires a set of input parameters, apart from the signal 
#' vector. These parameters are contained in and read from the function 
#' \code{list_sensor()} and \code{list_logger()}. Poles and zeros are used 
#' to build the transfer function. The value s is the generator constant in 
#' Vs/m. The value k is the normalisation factor. AD is the analogue-digital 
#' conversion factor. If the signal was recorded with a gain value other than 
#' 1, the resulting signal needs to be corrected for this, as well.
#' 
#' @param data \code{Numeric} vector or list of vectors, input signal vector.
#' 
#' @param dt \code{Numeric} scalar, sampling rate.
#' 
#' @param sensor \code{Character} scalar or \code{list} object, 
#' seismic sensor name. Must be present in the sensor library 
#' (\code{list_sensor}) or parameters must be added manually (see 
#' examples). Default is \code{"TC120s"}.
#' 
#' @param logger \code{Character} scalar, seismic logger name. Must be 
#' present in the logger library (\code{list_logger}) or parameters must be
#' added manually. Default is \code{"Cube"}.
#' 
#' @param gain \code{Numeric} value, signal gain level of the logger. Default 
#' is \code{1}.
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
#' ## add new logger manually
#' logger_new <- list_logger()[[1]]
#' 
#' ## add logger data
#' logger_new$ID <- "logger_new"
#' logger_new$name <- "logger_new"
#' logger_new$AD <- 2.4414e-07
#' 
#' ## deconvolve signal with new logger
#' rockfall_decon <- signal_deconvolve(data = rockfall,
#'                                     dt = 1/200, 
#'                                     sensor = "TC120s", 
#'                                     logger = logger_new)
#'                                     
#' ## Change the setup of a logger, here: Centaur AD is changed due to 
#' ## other than default Vpp value, according to AD = V / (2^24).
#' 
#' ## extract default Centaur logger
#' Centaur_10V <- list_logger()[[2]]
#' 
#' ## replace AD value
#' Centaur_10V$AD <- 20/(2^24)
#'                                     
#' @export signal_deconvolve
signal_deconvolve <- function(
  data,
  dt,
  sensor = "TC120s",
  logger = "Cube3extBOB",
  gain = 1,
  p = 10^-6,
  waterlevel = 10^-6
) {
  
  ## check data structure
  if(class(data) == "list") {
    
    ## apply function to list
    data_out <- lapply(X = data, 
                       FUN = eseis::signal_deconvolve, 
                       dt = dt,
                       sensor = sensor,
                       logger = logger,
                       gain = gain,
                       p = p,
                       waterlevel = waterlevel)
    
    ## return output
    return(data_out)
  } else {
    
    ## get sensor information
    if(class(sensor) == "character") {
      
      sensor <- try(list_sensor()[[sensor]], silent = TRUE)
      
      if(class(sensor) == "try-error") {
        stop("Sensor keyword not in library! Consider adding manually.")
      }
    } else if(class(sensor) == "list") {
      
      if(length(match(x = names(sensor), 
                      table = names(list_sensor()[[1]]))) != 10) {
        
        stop("Sensor specification incorrect! See examples.")
      }
    } else {
      
      stop("Sensor specification incorrect! See examples.")
    }
    
    poles <- sensor$poles
    zeros <- sensor$zeros
    s <- sensor$s
    k <- sensor$k
    
    ## get sensor information
    if(class(logger) == "character") {
      
      logger <- try(list_logger()[[logger]], silent = TRUE)
      
      if(class(logger) == "try-error") {
        
        stop("Logger keyword not in library! Consider adding manually.")
      }      
    } else if(class(logger) == "list") {
      
      if(length(match(x = names(logger), 
                      table = names(list_logger()[[1]]))) != 7) {
        
        stop("Logger specification incorrect! See examples.")
      }
    } else {
      
      stop("Logger specification incorrect! See examples.")
    }
    
    AD <- logger$AD
    
    ## apply gain correction
    data <- data / gain
    
    ## detrend data set
    data_detrend <- signal_detrend(data = data)
    
    ## remove mean from data set
    data_demean <- signal_demean(data = data_detrend)
    rm(data_detrend)
    
    ## taper data set
    data_taper <- signal_taper(data = data_demean, 
                               p = p)
    rm(data_demean)
    
    ## add zeros to reach even power of two number
    data_padd <- signal_padd(data = data_taper)
    rm(data_taper)
    
    ## make frequency vector
    if ((length(data_padd)%%2) == 1) {
      f <- c(seq(0, (length(data_padd) - 1) / 2), 
             seq(-(length(data_padd) - 1) / 2, -1)) / (length(data_padd) * dt)
    } else {
      f = c(seq(0, length(data_padd) / 2), 
            seq(-length(data_padd) / 2 + 1, -1)) / (length(data_padd) * dt)
    }  
    
    ## calculate Fourier transform
    x_fft <- fftw::FFT(x = data_padd)
    
    
    ## get polynomial form of poles and zeros
    poles_poly <- Re(signal::poly(x = poles))
    zeros_poly <- Re(signal::poly(x = zeros))
    
    ## calculate complex circular frequency vector
    w_comp <- complex(real = 0, imaginary = 1) * (2 * pi * f)
    
    ## calculate transfer function
    h <- outer(w_comp, (length(zeros_poly) - 1):0, "^") %*% zeros_poly / 
      outer(w_comp, (length(poles_poly) - 1):0, "^") %*% poles_poly * k
    rm(w_comp)
    
    ## calculate waterlevel factor
    gamma <- max(Re(h * Conj(h))) * waterlevel
    
    ## deconvolve signal  
    data_decon <- x_fft * Conj(h) / Re(h * Conj(h) + gamma)
    rm(x_fft)
    
    ## invert deconvolved signal
    data_inverse <- Re(fftw::IFFT(x = data_decon, 
                                  inverse = TRUE))
    rm(data_decon)
    
    ## correct for A/D-ratio and sensitivity factor
    data_inverse <- (data_inverse * AD) / s
    
    ## truncate signal to original length
    data_inverse <- data_inverse[1:length(data)]
    
    ## remove mean from output signal
    data_out <- signal_demean(data_inverse)
    rm(data_inverse)
    
    ## remove garbage
    gc()
    
    ## return output signal
    return(data_out)    
  }
 }