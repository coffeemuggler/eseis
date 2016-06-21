#' Calculate Hilbert transform.
#' 
#' The function calculates the Hilbert transform of the input signal vector.
#' 
#' @param data \code{Numeric} vector, input signal vector.
#' 
#' @return \code{Numeric} vector, Hilbert transform.
#' @author Michael Dietze
#' @keywords eseis
#' @examples
#' 
#' ## NOT DONE YET
#'                      
#' @export signal_hilbert
signal_hilbert <- function(
  data
) {
  
  ## create Hilbert vector
  h <- numeric(length = length(data))
  
  ## ceck odd vs. even data length
  if(length(data) %% 2 == 0) {
    h[c(1, length(data) / 2 + 1)] <- 1
    h[2:(length(data) / 2)] <- 2
  }
  else {
    h[1] <- 1
    h[2:((length(data) + 1) / 2)] <- 2
  }
  
  ## get Hilbert transform
  data_out <- stats::fft(h * stats::fft(data), inverse = TRUE) / length(data)    

  ## return hilbert data set
  return(invisible(data_out))
}