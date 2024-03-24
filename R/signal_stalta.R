#' Calculate the short-time-average to long time average ratio
#'
#' This function calculates the short-time-average to long time average 
#' (STA-LTA) ratio of a seismic signal and returns that ratio time series.
#'
#' @param data \code{eseis} object, \code{numeric} vector or list of 
#' objects, data set to be processed.
#' 
#' @param sta \code{Numeric} value, length of the short time average window
#' size, in number of samples.
#' 
#' @param lta \code{Numeric} value, length of the long time average window
#' size, in number of samples.
#' 
#' @param lazy \code{Logical} value, option to preprocess data, including 
#' demeaning, detrending and envelope calculation. Default is \code{FALSE}.
#' 
#' @return \code{Numeric} vector or list of vectors, STA-LTA ratio signal.
#' 
#' @author Michael Dietze
#' 
#' @keywords eseis
#' 
#' @examples
#' 
#' ## load example data
#' data(rockfall)
#' 
#' ## calculate STA-LTA ratio
#' rockfall_stalta <- signal_stalta(data = rockfall_eseis, 
#'                                  sta = 0.5 * 200, 
#'                                  lta = 10 * 200, 
#'                                  lazy = TRUE)
#' 
#' @export signal_stalta

signal_stalta <- function(
    data,
    sta,
    lta,
    lazy = FALSE
) {
  
  if(missing(sta) == TRUE) {
    stop("No sta value provided!")
  }
  
  if(missing(lta) == TRUE) {
    stop("No lta value provided!")
  }
  
  if(is.numeric(sta) == FALSE | is.numeric(lta) == FALSE) {
    stop("Either sta or lta are no numeric values!")
  }
  
  if(sta >= lta) {
    stop("The sta value is not smaller than lta value!")
  }
  
  ## check data structure
  if(class(data)[1] == "list") {
    
    ## apply function to list
    data_out <- lapply(X = data, 
                       FUN = eseis::signal_stalta,
                       sta = sta,
                       lta = lta,
                       lazy = lazy)
    
    ## return output
    return(data_out)
    
  } else {
    
    ## get start time
    eseis_t_0 <- Sys.time()
    
    ## collect function arguments
    eseis_arguments <- list(data = "",
                            sta = sta,
                            lta = lta,
                            lazy = lazy)
    
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
    
    ## optionally calculate signal envelope
    if(lazy == TRUE) {
      
      data <- eseis::signal_demean(data = data)
      data <- eseis::signal_detrend(data = data)
      data <- eseis::signal_envelope(data = data)
    }
    
    ## calculate sta vector
    data_sta <- caTools::runmean(x = data, 
                                 k = sta, 
                                 alg = "fast", 
                                 endrule = "NA",
                                 align = "right")
    
    ## calculate lta vector
    data_lta <- caTools::runmean(x = data, 
                                 k = lta, 
                                 alg = "fast", 
                                 endrule = "NA",
                                 align = "right")
    
    ## calculate stalta vector
    data_out <- data_sta / data_lta
    
    ## set NA value to zero
    data_out[is.na(data_out)] <- mean(data_out, na.rm = TRUE)
    
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
             call = "signal_stalta()",
             arguments = eseis_arguments,
             duration = eseis_duration)
      names(eseis_data$history)[length(eseis_data$history)] <- 
        as.character(length(eseis_data$history))
      
      ## update data type
      eseis_data$meta$type = "characteristic"
      
      ## assign eseis object to output data set
      data_out <- eseis_data
    }
    
    ## return data set
    return(invisible(data_out)) 
  }
}