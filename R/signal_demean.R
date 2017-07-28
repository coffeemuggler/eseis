#' Remove mean of signal vector.
#' 
#' The function removes the mean from a signal vector.
#' 
#' @param data \code{Numeric} vector or list of vectors, input signal vector.
#' 
#' @return \code{Numeric} vector or list of vectors, data set with mean 
#' subtracted.
#' @author Michael Dietze
#' @keywords eseis
#' @examples
#' 
#' ## load example data set
#' data(rockfall)
#' 
#' ## remove mean from data set
#' rockfall_demean <- signal_demean(data = rockfall_z)
#' 
#' ## compare data ranges
#' range(rockfall_z)
#' range(rockfall_demean)
#' 
#' ## show mean of initial signal
#' mean(rockfall_z)
#'                      
#' @export signal_demean
signal_demean <- function(
  data
) {
  
  ## check data structure
  if(class(data) == "list") {
    
    ## apply function to list
    data_out <- lapply(X = data, 
                       FUN = eseis::signal_demean)
    
    ## return output
    return(data_out)
  } else {
    
    ## remove mean
    data_out <- data - mean(data, na.rm = TRUE)
    
    ## return output
    return(data_out) 
  }
}