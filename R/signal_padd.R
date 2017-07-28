#' Padd signal with zeros.
#' 
#' The function adds zeros to the input vector to reach a length,
#' corresponding to the next higher power of two.
#' 
#' @param data \code{Numeric} vector or list of vectors, input signal vector
#' 
#' @return \code{Numeric} vector or list of vectors, signal vector with 
#' added zeros.
#' @author Michael Dietze
#' @keywords eseis
#' @examples
#' 
#' ## load example data set
#' data(rockfall)
#' 
#' ## padd with zeros
#' rockfall_padd <- signal_padd(data = rockfall_z)
#' 
#' ## compare lengths
#' length(rockfall_z)
#' length(rockfall_padd)
#'                      
#' @export signal_padd
signal_padd <- function(
  data
) {
  
  ## check data structure
  if(class(data) == "list") {
    
    ## apply function to list
    data_out <- lapply(X = data, 
                       FUN = eseis::signal_padd)
    
    ## return output
    return(data_out)
  } else {
    
    ## get number of samples to reach next highest power two length 
    #n <- 2^ceiling(x = log(x = length(data), base = 2))
    n <- stats::nextn(n = length(data), factors = 2)
    
    ## create output vector
    data_out <- rep(x = 0, times = n)
    
    ## add original values
    data_out[1:length(data)] <- data
    
    ## return output
    return(data_out) 
  }
}
