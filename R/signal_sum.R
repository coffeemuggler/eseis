#' Calculate signal vector sum.
#' 
#' The function calculates the vector sum of the input signals.
#' 
#' @param \dots \code{Numeric} vectors, input signal vectors, that 
#' must be of the same length.
#' 
#' @return \code{Numeric} vector, signal vector sum.
#' @author Michael Dietze
#' @keywords eseis
#' @examples
#' 
#' ## create random vectors
#' x <- runif(n = 1000, min = -1, max = 1)
#' y <- runif(n = 1000, min = -1, max = 1)
#' z <- runif(n = 1000, min = -1, max = 1)
#' 
#' ## calculate vector sums
#' xyz <- signal_sum(x, y, z)
#'                      
#' @export signal_sum
signal_sum <- function(
  ...
) {
  
  ## homogenise input data
  data <- do.call(rbind, list(...))
  
  ## calculate vector sum
  data_sum <- sqrt(colSums(data^2))
 
  ## return output
  return(data_sum)
}