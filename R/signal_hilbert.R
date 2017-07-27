#' Calculate Hilbert transform.
#' 
#' The function calculates the Hilbert transform of the input signal vector.
#' 
#' @param data \code{Numeric} vector or list of vectors, input signal vector.
#' 
#' @return \code{Numeric} vector or list of vectors, Hilbert transform.
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
  
  ## check data structure
  if(class(data) == "list") {
    
    ## apply function to list
    data_out <- lapply(X = data, 
                       FUN = eseis::signal_hilbert)
    
    ## return output
    return(data_out)
  } else {
    
    ## get length of the data set
    n <- length(data)

    ## create Hilbert vector
    h <- numeric(length = n)

    ## check odd vs. even data length
    if(n %% 2 == 0) {
      h[c(1, n / 2 + 1)] <- 1
      h[2:(n / 2)] <- 2
    }
    else {
      h[1] <- 1
      h[2:((n + 1) / 2)] <- 2
    }
    
    ## get Hilbert transform
    data_out <- fftw::IFFT(h * fftw::FFT(data))
    
    ## return hilbert data set
    return(invisible(data_out)) 
  }
}