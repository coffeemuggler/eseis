#' Calculate signal-to-noise-ratio.
#' 
#' The function calculates the signal-to-noise ratio of an input signal 
#' vector as the ratio between mean and max.
#' 
#' @param data \code{Numeric} vector or list of vectors , input signal 
#' vector.
#' 
#' @param detrend \code{Logical} scalar, optionally detrend data set before
#' calcualting snr.
#' 
#' @return \code{Numeric} scalar, signal-to-noise ratio.
#' @author Michael Dietze
#' @keywords eseis
#' @examples
#' 
#' ## load example data set
#' data(rockfall)
#' 
#' ## calculate snr with detrend option off and on
#' signal_snr(data = rockfall)
#' signal_snr(data = rockfall, detrend = TRUE)
#'                      
#' @export signal_snr
signal_snr <- function(
  data,
  detrend = FALSE
) {

  ## check data structure
  if(class(data) == "list") {
    
    ## apply function to list
    data_out <- lapply(X = data, 
                       FUN = eseis::signal_snr, 
                       detrend = detrend)
    
    ## return output
    return(data_out)
  } else {
    
    ## optionally detrend data set
    if(detrend == TRUE) {
      
      data <- signal_detrend(data = data)
    }
    
    ## calculate SNR
    data_out <- abs(max(data, na.rm = TRUE) / mean(data, na.rm = TRUE))

    ## return output
    return(data_out)
  }
}