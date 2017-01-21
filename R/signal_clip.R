#' Clip signal based on time vector.
#' 
#' The function clips signals based on the correpsonding time vector.
#' 
#' @param data \code{Numeric} vector or list of vectors, input signal vector.
#' 
#' @param time \code{POSIXct} vector, corresponding time vector.
#' 
#' @param limits \code{POSIXct} vector of length two, time limits for 
#' clipping.
#' 
#' @return \code{Numeric} data set clipped to provided time interval. 
#' 
#' @author Michael Dietze
#' @keywords eseis
#' @examples
#' 
#' ## TO BE DONE
#'                      
#' @export signal_clip
signal_clip <- function(
  data,
  time,
  limits
) {
  
  ## check data structure
  if(class(data) == "list") {
    
    ## apply function to list
    data_out <- lapply(X = data, 
                       FUN = eseis::signal_clip,
                       time = time,
                       limits = limits)
    
    ## return output
    return(data_out)
    
  } else {
    
    ## clip signal
    data_out <- data[time >= limits[1] & time <= limits[2]]
    
    ## return output
    return(data_out) 
  }
}
