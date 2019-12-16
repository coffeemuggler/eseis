#' Filter a seismic signal in the time or frequency domain
#' 
#' The function filters the input signal vector in the time or
#' frequency domain.
#' 
#' @param data \code{eseis} object, \code{numeric} vector or list of 
#' objects, data set to be processed.
#' 
#' @param f \code{Numeric} value or vector of length two, lower and/or 
#' upper cutoff frequencies (Hz).
#' 
#' @param fft \code{Logical} value, option to filter in the time domain 
#' (\code{fft = FALSE}) or the frequency domain (\code{fft = TRUE}). Default 
#' is (\code{fft = FALSE}).
#' 
#' @param dt \code{Numeric} value, sampling period. If omitted, \code{dt} 
#' is set to 1/200.
#' 
#' @param type \code{Character} value, type of filter, one out of 
#' \code{"LP"} (low pass), \code{"HP"} (high pass), \code{"BP"} (band 
#' pass) and \code{"BR"} (band rejection). If omitted, the type is interpreted 
#' from \code{f}. If \code{f} is of length two, \code{type} is set to 
#' \code{"BP"}. If \code{f} is of length one, \code{type} is set to 
#' \code{"HP"}.
#' 
#' @param shape \code{Character} value, one out of \code{"butter"} 
#' (Butterworth), default is \code{"butter"}.
#' 
#' @param order \code{Numeric} value, order of the filter, default 
#' is \code{2}. Only needed if \code{data} is no \code{eseis} object.
#' 
#' @param p \code{Numeric} value, fraction of the signal to be tapered.
#' 
#' @return \code{Numeric} vector or list of vectors, filtered signal vector.
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
#' ## filter data set by bandpass filter between 1 and 90 Hz
#' rockfall_bp <- signal_filter(data = rockfall_eseis, 
#'                              f = c(1, 90))
#'                              
#' ## taper signal to account for edge effects
#' rockfall_bp <- signal_taper(data = rockfall_bp, 
#'                             n = 2000)
#' 
#' ## plot filtered signal
#' plot_signal(data = rockfall_bp)
#' 
#' ## compare time domain versus frequency domain filtering
#' rockfall_td <- signal_filter(data = rockfall_eseis, 
#'                              f = c(10, 40), 
#'                              fft = FALSE)
#'                              
#' rockfall_td_sp <- signal_spectrum(data = rockfall_td)
#' 
#' rockfall_fd <- signal_filter(data = rockfall_eseis, 
#'                              f = c(10, 40), 
#'                              fft = TRUE)
#'                              
#' rockfall_fd_sp <- signal_spectrum(data = rockfall_fd)
#'
#' plot_spectrum(data = rockfall_td_sp)
#' plot_spectrum(data = rockfall_fd_sp)
#'                      
#' @export signal_filter
signal_filter <- function(
  data,
  f,
  fft = FALSE,
  dt,
  type,
  shape = "butter",
  order = 2,
  p = 0
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
  
  ## check data structure
  if(class(data)[1] == "list") {
    
    ## apply function to list
    data_out <- lapply(X = data, 
                       FUN = eseis::signal_filter, 
                       fft = fft,
                       dt = dt,
                       f = f,
                       type = type,
                       shape = shape,
                       order = order,
                       p = p)
    
    ## return output
    return(data_out)
    
  } else {
    
    ## check f
    if(missing(f) == TRUE) {
      
      stop("No frequencies provided!")
    }
    
    ## check/set type
    if(missing(type) == TRUE) {
      
      if(length(f) == 1) {
        
        type <- "HP"
      } else {
        
        type <- "BP"
      }
    }
    
    ## get start time
    eseis_t_0 <- Sys.time()
    
    ## collect function arguments
    eseis_arguments <- list(data = "",
                            dt = dt,
                            f = f,
                            type = type,
                            shape = shape,
                            order = order,
                            p = p)
    
    ## check if input object is of class eseis
    if(class(data)[1] == "eseis") {
      
      ## set eseis flag
      eseis_class <- TRUE
      
      ## store initial object
      eseis_data <- data
      
      ## extract signal vector
      data <- eseis_data$signal
      
      ## update dt
      dt <- eseis_data$meta$dt
      
      ## get number of samples
      n <- eseis_data$meta$n
      
    } else {
      
      ## set eseis flag
      eseis_class <- FALSE
      
      ## get number of samples
      n <- length(data)
    }
    
    ## filter signal with Butterworth filter
    if(shape == "butter") {
      
      ## translate filter type for function "butter()"
      if(type == "LP") {
        type <- "low"
      } else if(type == "HP") {
        type <- "high"
      } else if(type == "BP") {
        type <- "pass"
      } else if(type == "BR") {
        type <- "stop"
      } 
      
      ## translate filter frequencies for function "butter()"
      f_filter <- f * 2 * dt
      
      ## decide on filter mode
      if(fft == FALSE) {
        
        ## filter in time domain
        data_out <- signal::filter(x = data, 
                                   signal::butter(n = order, 
                                                  W = f_filter,
                                                  type = type))
      } else {
        
        ## filter in frequency domain
        
        ## pad with zeros
        data_pad <- eseis::signal_pad(data = data)
        
        ## create frequency vector
        f_0 <- seq(from = 0, 
                   to = 1, 
                   length.out = length(data_pad))
        
        ## calculate fast Fourrier transform
        data_fft <- fft(data_pad)
        
        ## adjust filter corner frequencies and inversion option
        if(type == "low") {
          
          f_fft <- c(0, f)
          f_inv <- FALSE
          
        } else if(type == "high") {
          
          f_fft <- c(f, 1 / (2 * dt))
          f_inv <- FALSE
          
        } else if(type == "pass") {
          
          f_fft <- f
          f_inv <- FALSE
          
        } else if(type == "stop") {
          
          f_fft <- f
          f_inv <- TRUE
          
        }
        
        ## build filter
        a <- 1 / abs(((f_fft[2] - f_fft[1]) / 2 * dt)^order)
        
        ## assign weights
        w <- 1 - abs(a * abs(f_0 - (mean(f_fft) * dt))^order)
        
        ## set negative values to zero
        w[w < 0] <- 0
        
        ## optionally convert to band stop filter
        if(f_inv == TRUE) {
          
          w <-  1 - w
        }
        
        ## apply weights to data set
        data_fft <- w * data_fft * 2
        
        ## convert data back to time domain
        data_out <- Re(fft(data_fft, inverse = TRUE) / length(data_fft))
        
        ## remove padded zeros
        data_out <- data_out[1:n]
      }
    } else {
      
      stop("Filter shape not supported, yet!")
    }
    
    ## optionally apply taper
    if(p > 0) {
      
      data_out = eseis::signal_taper(data = data_out,
                                     p = p)
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
             call = "signal_filter()",
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
