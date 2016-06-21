#' Detrend a signal vector.
#' 
#' The function removes a linear trend from a signal vector.
#' 
#' @param data \code{Numeric} vector, input signal vector
#' 
#' @return \code{Numeric} vector, detrended data set.
#' @author Michael Dietze
#' @keywords eseis
#' @examples
#' 
#' ## load example data set
#' data(rockfall)
#' 
#' ## remove linear trend from data set
#' rockfall_detrend <- signal_detrend(data = rockfall)
#' 
#' ## compare data ranges
#' range(rockfall)
#' range(rockfall_detrend)
#'                      
#' @export signal_detrend
signal_detrend <- function(
  data
) {
  
  ## convert vector to matrix
  data <- rbind(data)

  ## detrend data set  
  data_t <- t(data)
  A <- cbind(rep(0, nrow(data_t)), 
             rep(1, nrow(data_t)))
  A[(1:nrow(data_t)), 1] <- as.matrix(1:nrow(data_t)) / nrow(data_t)
  X <- t(data_t - A %*% qr.solve(A, data_t))

  ## convert matrix to vector
  data_out <- as.numeric(X)
  
  ## return output
  return(data_out)
}