#' Detrend a signal vector.
#' 
#' The function removes a trend from a signal vector.
#' 
#' The method \code{"simple"} subtracts a linear trend built from 
#' the first and last sample of the data set. The method \code{"linear"} 
#' uses the linear function as implemented in pracma::detrend.
#' 
#' @param data \code{eseis} object, \code{numeric} vector or list of 
#' objects, data set to be processed.
#' 
#' @param method \code{Character} value, method used for detrending. 
#' One out of \code{"simple"} and \code{"linear"}. Default is 
#' \code{"linear"}.
#' 
#' @return \code{Numeric} vector or list of vectors, detrended data set.
#' @author Michael Dietze
#' @keywords eseis
#' @examples
#' 
#' ## load example data set
#' data(rockfall)
#' 
#' ## remove linear trend from data set
#' rockfall_detrend <- signal_detrend(data = rockfall_eseis)
#' 
#' ## compare data ranges
#' range(rockfall_eseis$signal)
#' range(rockfall_detrend$signal)
#'                      
#' @export signal_detrend
signal_detrend <- function(
  data,
  method = "linear"
) {
  
  ## check data structure
  if(class(data)[1] == "list") {
    
    ## apply function to list
    data_out <- lapply(X = data, 
                       FUN = eseis::signal_detrend)
    
    ## return output
    return(data_out)
  } else {
    
    ## get start time
    eseis_t_0 <- Sys.time()
    
    ## collect function arguments
    eseis_arguments <- list(data = "",
                            method = method)
    
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
    
    ## detrend data set  
    if(method == "linear") {
      
      ## convert vector to matrix
      data <- rbind(data)
      
      data_t <- t(data)
      A <- cbind(rep(0, nrow(data_t)), 
                 rep(1, nrow(data_t)))
      A[(1:nrow(data_t)), 1] <- as.matrix(1:nrow(data_t)) / nrow(data_t)
      X <- t(data_t - A %*% qr.solve(A, data_t))
      
      ## convert matrix to vector
      data_out <- as.numeric(X)
    } else if(method == "simple") {
      
      trend_linear <- seq(from = data[1], 
                          to = data[length(data)],
                          length.out = length(data))
      
      data_out <- data - trend_linear
    } else {
      
      stop("Method not supported!")
    }

    
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
             call = "signal_detrend()",
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