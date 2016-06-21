#' Remove mean of signal vector.
#' 
#' The function removes the mean from a signal vector.
#' 
#' @param data \code{Numeric} vector, input signal vector
#' 
#' @return \code{Numeric} vector, data set with mean subtracted.
#' @author Michael Dietze
#' @keywords eseis
#' @examples
#' 
#' ## load example data set
#' data(rockfall)
#' 
#' ## remove mean from data set
#' rockfall_demean <- signal_demean(data = rockfall)
#' 
#' ## compare data ranges
#' range(rockfall)
#' range(rockfall_demean)
#' 
#' ## chow mean of initial signal
#' mean(rockfall)
#'                      
#' @export signal_demean
signal_demean <- function(
  data
) {
  
  ## remove mean
  data_out <- data - mean(data, na.rm = TRUE)

  ## return output
  return(data_out)
}