#' Fill NA-gaps of a signal
#' 
#' This function performs linear interpolation of NA values.
#' 
#' Note that the procedure will contaminate the signal by artefacts as 
#' increasingly larger data gaps are filled with interpolated values.
#' 
#' @param data \code{eseis} object, numeric vector or list of objects, 
#' data set to be processed. 
#' 
#' @return \code{eseis} object, numeric vector or list of objects, 
#' interpolated data set(s).
#' 
#' @author Michael Dietze
#' 
#' @keywords eseis
#' 
#' @examples
#' 
#' ## create synthetic data set and add NA-gaps
#' x <- eseis::signal_detrend(data = runif(1000))
#' x_gap <- x
#' x_gap[100:102] <- NA
#' x_gap[500:530] <- NA
#' 
#' ## fill gaps
#' y <- signal_fill(data = x_gap)
#' 
#' ## filter both data sets
#' x <- signal_filter(data = x, f = c(1, 3), dt = 1/200)
#' y <- signal_filter(data = y, f = c(1, 3), dt = 1/200)
#' 
#' ## plot both data sets
#' plot(y, type = "l", col = "grey", lwd = 3)
#' lines(x, col = "red")
#' 
#' @export signal_fill
signal_fill <- function(
  
  data
) {
  
  
  ## check data structure
  if(class(data)[1] == "list") {
    
    ## apply function to list
    data_out <- lapply(X = data, 
                       FUN = eseis::signal_fill)
    
    ## return output
    return(data_out)
  } else {
    
    ## get start time
    eseis_t_0 <- Sys.time()
    
    ## collect function arguments
    eseis_arguments <- list(data = "")
    
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
    
    ## find gaps
    na_lims <- diff(is.na(data))
    
    ## account for gaps at beginning of data set
    if(is.na(data[1]) == TRUE) {
      
      data[1] <- data[which(na_lims == -1)[1] + 1]
    }
    
    ## account for gaps at beginning of data set
    if(is.na(data[length(data)]) == TRUE) {
      
      data[length(data)] <- data[utils::tail(which(na_lims == 1), n = 1)]
    }
    
    ## find upper and lower limits of gaps
    na_lims <- diff(is.na(data))
    l <- c(which(na_lims == 1))
    u <- c(which(na_lims == -1) + 1)
    
    ## create output data set
    data_out <- data
    
    ## fill gaps
    if(length(l) > 0) {
      
      for(i in 1:length(l)) {
        
        data_out[(l[i] + 1):(u[i] - 1)] <- seq(from = data[l[i]], 
                                               to = data[u[i]], 
                                               length.out = u[i] - l[i] - 1)
      }    
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
             call = "signal_fill()",
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