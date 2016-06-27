#' Calculate stal-lta-ratio.
#' 
#' The function calculates the ratio of the short-term-average and 
#' long-term-average of the input signal.
#' 
#' @param data \code{Numeric} vector or list , input signal envelope vector(s)
#' 
#' @param sta \code{Numeric} scalar, number of samples for short-term window.
#' 
#' @param lta \code{Numeric} scalar, number of samples for long-term window.
#' 
#' @return \code{Numeric} vector, sta-lta-ratio.
#' @author Michael Dietze
#' @keywords eseis
#' @examples
#' 
#' ## load example data set
#' data(rockfall)
#' 
#' ## TO BE DONE, LATER
#'                      
#' @export signal_stalta
signal_stalta <- function(
  data,
  sta,
  lta
) {
  
  ## check data structure
  if(class(data) == "list") {
    
    ## apply function to list
    data_out <- lapply(X = data, 
                       FUN = eseis::signal_stalta)
    
    ## return output
    return(data_out)
  } else {

    ## calculate sta vector
    data_sta <- caTools::runmean(data, 
                                 k = round(x = sta / 2, digits = 0), 
                                 alg = "fast", 
                                 endrule = "NA",
                                 align = "right")
    
    ## calculate lta vector
    data_lta <- caTools::runmean(data, 
                                 k = round(x = lta / 2, digits = 0), 
                                 alg = "fast", 
                                 endrule = "NA",
                                 align = "right")
    
    ## build ratio
    data_out <- data_sta / data_lta
    
    ## return output
    return(data_out)
  }
}