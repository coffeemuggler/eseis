#' Filter a seismic signal.
#' 
#' The function filters the input signal vector.
#' 
#' @param data \code{Numeric} vector or list of vectors, input signal vector.
#' 
#' @param dt \code{Numeric} scalar, sampling period. If omitted, \code{dt} 
#' is set to 1/200.
#' 
#' @param f \code{Numeric} scalar or vector of length two, lower and/or 
#' upper cutoff frequencies (Hz).
#' 
#' @param type \code{Character} scalar, type of filter, one out of 
#' \code{"LP"} (low pass), \code{"HP"} (high pass), \code{"BP"} (band 
#' pass) and \code{"BR"} (band rejection). If omitted, the type is interpreted 
#' from \code{f}. If \code{f} is of length two, \code{type} is set to 
#' \code{"BP"}. If \code{f} is of length one, \code{type} is set to 
#' \code{"HP"}.
#' 
#' @param shape \code{Character} scalar, one out of \code{"butter"} 
#' (Butterworth), default is \code{"butter"}.
#' 
#' @param order \code{Numeric} scalar, order of the filter, default 
#' is \code{2}.
#' 
#' @param p \code{Numeric} scalar, fraction of the signal to be tapered.
#' 
#' @return \code{Numeric} vector or list of vectors, filtered signal vector.
#' @author Michael Dietze
#' @keywords eseis
#' @examples
#' 
#' ## load example data set
#' data(rockfall)
#' 
#' ## filter data set by bandpass filter between 1 and 90 Hz
#' rockfall_bp <- signal_filter(data = rockfall_z, 
#'                              f = c(1, 90), 
#'                              dt = 1/200)
#'                              
#' ## taper signal to account for edge effects
#' rockfall_bp <- signal_taper(data = rockfall_bp, n = 2000)
#' 
#' ## plot filtered signal
#' plot(x = rockfall_t[68000:78000], 
#'      y = rockfall_bp[68000:78000], 
#'      type = "l")
#'                      
#' @export signal_filter
signal_filter <- function(
  data,
  dt,
  f,
  type,
  shape = "butter",
  order = 2,
  p = 0
) {
  
  ## check data structure
  if(class(data) == "list") {
    
    ## apply function to list
    data_out <- lapply(X = data, 
                       FUN = eseis::signal_filter, 
                       dt = dt,
                       f = f,
                       type = type,
                       shape = shape,
                       order = order,
                       p = p)
    
    ## return output
    return(data_out)
  } else {
    
    ## check/set dt
    if(missing(dt) == TRUE) {
      
      warning("Sampling frequency missing! Set to 1/200")
      
      dt <- 1/200
    }
    
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
      
      ## translate filter frequencis for function "butter()"
      f_filter <- f * 2 * dt
      
      data_filter <- signal::filter(x = data, signal::butter(n = order, 
                                                             W = f_filter,
                                                             type = type))
    }
    
    ## apply taper
    #data_out <- stats::spec.taper(x = data_filter, p = p)
    data_out = data_filter
    ## return output
    return(data_out) 
  }
}
