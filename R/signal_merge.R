#' Merge several signal streams into one
#' 
#' This function merges two or more single signals into one common. Only 
#' \code{eseis} objects are supported. Gaps will be filled with \code{NA}
#' values unless the argument \code{fill = TRUE}. The resulting data length
#' will correspond to the combined length of the input data. The sampling 
#' frequency must be the same for all input data sets. The meta data of the  
#' first stream will be used for the output data.
#' 
#' @param \dots \code{eseis} objects that will be merged into one output
#' \code{eseis} object. Can also be one list that contains \code{eseis}
#' objects.
#' 
#' @param fill \code{Logical} value, option to fill data gaps between 
#' merged streams. Default is \code{FALSE}.
#' 
#' @return \code{eseis} object with merged input data sets
#' 
#' @author Michael Dietze
#' 
#' @keywords eseis
#' 
#' @examples
#' 
#' ## load rockfall data set
#' data("rockfall")
#' s_1 <- rockfall_eseis
#' 
#' ## duplicate data set and shift start time (incl. gap)
#' s_2 <- s_1
#' s_2$meta$starttime <- s_2$meta$starttime + 500
#' 
#' ## merge data sets
#' s_merged <- signal_merge(s_1, s_2)
#' 
#' ## plot merged data set
#' plot(s_merged)
#' 
#' ## merge and fill gap
#' s_merged_filled <- signal_merge(s_1, s_2, fill = TRUE)
#' 
#' ## plot merged data set
#' plot(s_merged_filled)
#' 
#' @export signal_merge

signal_merge <- function(
    
  ...,
  fill = FALSE
) {
  
  ## get start time
  eseis_t_0 <- Sys.time()
  
  ## collect function arguments
  eseis_arguments <- list(data = "",
                          fill = fill)
  
  ## convert input data to list
  data <- list(...)
  
  ## check if a list of eseis objects had been provided
  if(length(data) == 1) {
    
    if(class(data[[1]][[1]])[1] == "eseis") {
      
      data <- data[[1]]
    }
  }
  
  ## check if all elements are eseis objects
  if(any(sapply(X = data, FUN = function(x) {
    
    class(x)[1] == "eseis"
  })) == FALSE) {
    
    stop("At least one object if not of class eseis!")
  }
  
  ## check that dt is equal for all eseis objects
  dt_all <- sapply(X = data, FUN = function(x) {
    x$meta$dt
  })
  
  if(any(dt_all - dt_all[1] == 0) == FALSE) {
    
    stop("At least one object has a different dt value")
  }  
  
  ## extract start times and number of samples
  t_n_dt_data <- do.call(rbind, lapply(X = data, FUN = function(x) {
    
    return(data.frame(starttime = x$meta$starttime,
                      stoptime = x$meta$starttime + x$meta$n * x$meta$dt,
                      n = x$meta$n,
                      dt = x$meta$dt))
  }))
  
  ## sort info and data by start time
  data <- data[order(t_n_dt_data$starttime)]
  t_n_dt_data <- t_n_dt_data[order(t_n_dt_data$starttime),]
  
  ## create time and empty signal vectors
  t_merge <- seq(from = t_n_dt_data$starttime[1],
                 by = t_n_dt_data$dt[1],
                 to = tail(t_n_dt_data$stoptime, 1) + 
                   t_n_dt_data$dt[1])
  s_merge <- rep(NA, length(t_merge))
  
  ## fill signal vector
  for(i in 1:length(data)) {
    
    d_t_0 <- abs(t_merge - t_n_dt_data$starttime[i])
    d_t_1 <- abs(t_merge - t_n_dt_data$stoptime[i])
    
    i_t_0 <- which(d_t_0 == min(d_t_0))
    i_t_1 <- which(d_t_1 == min(d_t_1)) - 1
    
    s_merge[i_t_0:i_t_1] <- data[[i]]$signal
  }
  
  ## modify first data set
  data_out <- data[[1]]
  data_out$signal <- s_merge
  data_out$meta$n <- length(s_merge)
  
  ## optionally fill gaps
  if(fill == TRUE) {
    
    data_out <- eseis::signal_fill(data = data_out)
  }
  
  ## calculate function call duration
  eseis_duration <- as.numeric(difftime(time1 = Sys.time(), 
                                        time2 = eseis_t_0, 
                                        units = "secs"))
  
  ## update object history
  data_out$history[[length(data_out$history) + 1]] <- 
    list(time = Sys.time(),
         call = "signal_merge()",
         arguments = eseis_arguments,
         duration = eseis_duration)
  names(data_out$history)[length(data_out$history)] <- 
    as.character(length(data_out$history))
  
  ## return output
  return(data_out) 
}