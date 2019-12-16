#' Aggregate a signal vector
#' 
#' The signal vector \code{data} is aggregated by an integer factor \code{n}.
#' If an \code{eseis} object is provided, the meta data is updated. The 
#' function is a wrapper for the funcion \code{decimate} of the package
#' \code{signal}. 
#' 
#' @param data \code{eseis} object, \code{numeric} vector or list of 
#' objects, data set to be processed.
#' 
#' @param n \code{Numeric} value, number of samples to be aggregated to one
#' new data value. Must be an integer value greater than 1. Default is 
#' \code{2}.
#' 
#' @param type \code{Character} value, filter type used for aggregation. For 
#' details see documentation of \code{signal::decimate}. Default is 
#' \code{"iir"}.
#' 
#' @return Aggregated data set.
#' @author Michael Dietze
#' @keywords eseis
#' @examples
#' 
#' ## load example data set
#' data(rockfall)
#' 
#' ## aggregate signal by factor 4 (i.e., dt goes from 1/200 to 1/50)
#' rockfall_agg <- signal_aggregate(data = rockfall_z, 
#'                                  n = 4)
#' 
#' ## create example data set
#' s <- 1:10
#'   
#' ## aggregate x by factor 2
#' s_agg_2 <- signal_aggregate(data = s,
#'                             n = 2)
#'                               
#' ## aggregate x by factor 3
#' s_agg_3 <- signal_aggregate(data = s, 
#'                             n = 3)
#'                               
#' ## plot results
#' plot(x = s,
#'      y = rep(x = 1, times = length(s)),
#'      ylim = c(1, 3))
#'      
#' points(x = s_agg_2, 
#'        y = rep(x = 2, times = length(s_agg_2)), 
#'        col = 2)
#'
#' points(x = s_agg_3, 
#'        y = rep(x = 3, times = length(s_agg_3)), 
#'        col = 3)
#'        
#' abline(v = s_agg_2,
#'        col = 2)
#'
#' abline(v = s_agg_3, 
#'        col = 3)
#'        
#' ## create signal matrix
#' X <- rbind(1:100, 1001:1100, 10001:10100)
#' 
#' ## aggregate signal matrix by factor 4
#' X_agg <- signal_aggregate(data = X, 
#' n = 4)
#' 
#' @export signal_aggregate
signal_aggregate <- function(
  data,
  n = 2,
  type = "iir"
) {
  
  ## check data structure
  if(class(data)[1] == "list") {
    
    ## apply function to list
    data_out <- lapply(X = data, 
                       FUN = eseis::signal_aggregate,
                       n = n)
    
    ## return output
    return(data_out)
  } else {
    
    ## get start time
    eseis_t_0 <- Sys.time()
    
    ## collect function arguments
    eseis_arguments <- list(data = "",
                            n = n,
                            type = type)
    
    ## check aggregation factor
    if(signif(n) != n) {
      
      n <- round(x = n, 
                 digits = 0)
      warning("Aggregation factor rounded to integer value!")
    }
    
    if(n < 1) {
      
      n <- 1
      warning("Aggregation factor smaller than 1, set to 1 automatically!")
    }
    
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
    
    ## use decimate function from signal package
    data_agg <- signal::decimate(x = data, 
                                 q = n, 
                                 ftype = type)
    
    ## optionally rebuild eseis object
    if(eseis_class == TRUE) {
      
      ## assign aggregated signal vector
      eseis_data$signal <- data_agg
      
      ## update number of samples
      eseis_data$meta$n <- length(data_agg)
      
      ## update sampling interval
      eseis_data$meta$dt <- eseis_data$meta$dt * n
      
      ## calculate function call duration
      eseis_duration <- as.numeric(difftime(time1 = Sys.time(), 
                                            time2 = eseis_t_0, 
                                            units = "secs"))
      
      ## update object history
      eseis_data$history[[length(eseis_data$history) + 1]] <- 
        list(time = Sys.time(),
             call = "signal_aggregate()",
             arguments = eseis_arguments,
             duration = eseis_duration)
      names(eseis_data$history)[length(eseis_data$history)] <- 
        as.character(length(eseis_data$history))
      
      ## assign eseis object to output data set
      data_agg <- eseis_data
    }
    
    ## return output
    return(data_agg)
  }
}