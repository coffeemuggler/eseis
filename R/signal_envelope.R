#' Calculate signal envelope.
#' 
#' The function calculates envelopes of the input signals as 
#' cosine-tapered envelope of the Hilbert-transformed signal. The signal
#' should be detrended and/or the mean should be removed before processing.
#' 
#' @param data \code{Numeric} vector or list of vectors, input signal vector.
#' 
#' @param p \code{Numeric} scalar, proportion of the signal to be tapered,
#' default is \code{10^-6}.
#' 
#' @return \code{Numeric} vector or list of vectors, signal envelope.
#' @author Michael Dietze
#' @keywords eseis
#' @examples
#' 
#' ## load example data set
#' data(rockfall)
#' 
#' ## detrend data set
#' rockfall_detrend <- signal_detrend(data = rockfall)
#' 
#' ## calculate envelope
#' rockfall_envelope <- signal_envelope(data = rockfall_detrend)
#' 
#' ## plot envelope
#' plot(x = t[68000:78000], y = rockfall_envelope[68000:78000], type = "l")
#'                      
#' @export signal_envelope
signal_envelope <- function(
  data,
  p = 10^-6
) {
  
  ## check data structure
  if(class(data) == "list") {
    
    ## apply function to list
    data_out <- lapply(X = data, 
                       FUN = eseis::signal_envelope, 
                       p = p)
    
    ## return output
    return(data_out)
  } else {
    
    ## calculate Hilbert transform
    data_hilbert <- signal_hilbert(data = data)
    
    ## calculate absolute values
    data_abs <- abs(data_hilbert)
    
    ## apply taper
    data_taper <- spec.taper(x = data_abs, 
                             p = p)
    
    ## return output
    return(data_taper) 
  }
}