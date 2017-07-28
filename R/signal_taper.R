#' Taper a signal vector.
#' 
#' The function tapers a signal vector with a cosine bell taper, either of a
#' given proportion or a discrete number of samples.
#' 
#' @param data \code{Numeric} vector, input signal vector
#' 
#' @param p \code{Numeric} scalar, proportion of the signal vector to be 
#' tapered. Alternative to \code{n}.
#' 
#' @param n \code{Numeric} scalar, number of samples to be tapered at each
#' end of the signal vector.
#' 
#' @return \code{Numeric} scalar, tapered signal vector.
#' @author Michael Dietze
#' @keywords eseis
#' @examples
#' 
#' ## load example data set
#' data(rockfall)
#' 
#' ## remove mean from data set
#' rockfall <- signal_demean(data = rockfall_z)
#' 
#' ## create artefact at the beginning
#' rockfall[1:100] <- runif(n = 100, min = -5000, max = 5000)
#' 
#' ## taper signal
#' rockfall_taper <- signal_taper(data = rockfall, n = 1000)
#' 
#' ## plot both data sets
#' plot(rockfall[1:5000], type = "l")
#' plot(rockfall_taper[1:5000], type = "l")
#'                      
#' @export signal_taper
signal_taper <- function(
  data,
  p = 0,
  n
) {
  
  ## check/set taper width
  if(missing(n) == FALSE) {
    p <- (n/2) / length(data)
  }
  
  ## check data structure
  if(class(data) == "list") {
    
    ## apply function to list
    data_out <- lapply(X = data, 
                       FUN = eseis::signal_taper, 
                       p = p,
                       n = n)
    
    ## return output
    return(data_out)
  } else {
    
    ## apply taper
    data_out <- spec.taper(x = data, p = p)
    
    ## return output
    return(data_out) 
  }
}
