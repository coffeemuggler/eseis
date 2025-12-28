#' Taper a signal vector.
#' 
#' The function tapers a signal vector with a cosine bell taper, either of a
#' given proportion or a discrete number of samples.
#' 
#' @param data \code{eseis} object, \code{numeric} vector or list of 
#' objects, data set to be processed.
#' 
#' @param p \code{Numeric} value, proportion of the signal vector to be 
#' tapered. Alternative to \code{n}.
#' 
#' @param n \code{Numeric} value, number of samples to be tapered at each
#' end of the signal vector.
#' 
#' @return \code{Data frame}, tapered signal vector.
#' 
#' @author Michael Dietze
#' 
#' @keywords eseis
#' 
#' @examples
#' 
#' ## load example data set
#' data(rockfall)
#' 
#' ## remove mean from data set
#' rockfall <- signal_demean(data = rockfall_eseis)
#' 
#' ## create artefact at the beginning
#' rockfall_eseis$signal[1:100] <- runif(n = 100, min = -5000, max = 5000)
#' 
#' ## taper signal
#' rockfall_taper <- signal_taper(data = rockfall, n = 1000)
#' 
#' ## plot both data sets
#' plot_signal(data = rockfall_eseis)
#' plot_signal(rockfall_taper)
#'                      
#' @export signal_taper
signal_taper <- function(
  data,
  p = 0,
  n
) {
  
  ## check/set taper width
  if(missing(n) == TRUE) {
    
    n <- NULL
  }
  
  ## check data structure
  if(class(data)[1] == "list") {
    
    ## apply function to list
    data_out <- lapply(X = data, 
                       FUN = eseis::signal_taper, 
                       p = p,
                       n = n)
    
    ## return output
    return(data_out)
  } else {
    
    ## get start time
    eseis_t_0 <- Sys.time()
    
    ## collect function arguments
    eseis_arguments <- list(data = "",
                            p = p,
                            n = n)
    
    ## check if input object is of class eseis
    if(class(data)[1] == "eseis") {
      
      ## set eseis flag
      eseis_class <- TRUE
      
      ## store initial object
      eseis_data <- data
      
      ## extract signal vector
      data <- eseis_data$signal
      
    } else {
      
      ## set eseis flag
      eseis_class <- FALSE
    }
    
    ## optionally convert n tp p
    if(is.null(n) == FALSE) {
      
      p <- (n / 2) / length(data)
    }
    
    ## apply taper
    data_out <- spec.taper(x = data,
                           p = p)
    
    ## optionally rebuild eseis object
    if(eseis_class == TRUE) {
      
      ## assign aggregated signal vector
      eseis_data$signal <- data_out
      
      ## calculate function call duration
      eseis_duration <- as.numeric(difftime(time1 = Sys.time(), 
                                            time2 = eseis_t_0, 
                                            units = "secs"))
      
      ## update object history
      eseis_data$history[[length(eseis_data$history) + 1]] <- 
        list(time = Sys.time(),
             call = "signal_taper()",
             arguments = eseis_arguments,
             duration = eseis_duration)
      names(eseis_data$history)[length(eseis_data$history)] <- 
        as.character(length(eseis_data$history))
      
      ## assign eseis object to output data set
      data_out <- eseis_data
    }
    
    ## return output
    return(data_out) 
  }
}
