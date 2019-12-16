#' Clip signal based on time vector.
#' 
#' The function clips a seismic signal based on the corresponding time vector.
#' 
#' @param data \code{eseis} object, \code{numeric} vector or list of 
#' objects, data set to be processed.
#' 
#' @param time \code{POSIXct} vector, corresponding time vector. Only needed 
#' if \code{data} is no \code{eseis} object.
#' 
#' @param limits \code{POSIXct} vector of length two, time limits for 
#' clipping.
#' 
#' @return \code{Numeric} data set clipped to provided time interval. 
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
#' ## define limits (second 10 to 20 of the signal)
#' limits <- c(rockfall_t[1] + 10, rockfall_t[1] + 20)
#' 
#' ## clip signal 
#' rockfall_clip <- signal_clip(data = rockfall_z, 
#'                              time = rockfall_t, 
#'                              limits = limits)
#'                      
#' ## clip signal using the eseis object
#' rockfall_clip <- signal_clip(data = rockfall_eseis, 
#'                              limits = limits)
#'                              
#' @export signal_clip
signal_clip <- function(
  data,
  time,
  limits
) {
  
  ## check/set time vector
  if(missing(time) == TRUE) {
    
    time <- NULL
  }
  
  ## check data structure
  if(class(data)[1] == "list") {
    
    ## apply function to list
    data_out <- lapply(X = data, 
                       FUN = eseis::signal_clip,
                       time = time,
                       limits = limits)
    
    ## return output
    return(data_out)
    
  } else {
    
    ## get start time
    t_0 <- Sys.time()
    
    ## collect function arguments
    eseis_arguments <- list(data = "",
                            time = time,
                            limits = limits)
    
    if(class(data)[1] == "eseis") {
      
      ## get start index
      i_start <- as.numeric(difftime(time1 = limits[1], 
                                     time2 = data$meta$starttime, 
                                     units = "secs")) / data$meta$dt + 1
      
      ## get stop index
      i_stop <- as.numeric(difftime(time1 = limits[2], 
                                    time2 = data$meta$starttime, 
                                    units = "secs")) / data$meta$dt
      

      ## correct for inappropriate indices
      if(i_start < 1) {
        
        warning("Start time smaller than signal start. Set to start time.")
        
        limits[1] <- limits[1] + (1 - i_start) * data$meta$dt
        
        i_start <- 1
      }
      
      if(i_stop > data$meta$n) {
        
        warning("Stop time greater than signal length. Set to end time.")
        
        limits[2] <- limits[1] + data$meta$n * data$meta$dt
        
        i_stop <- data$meta$n
      }
      
      ## clip signal
      data$signal <- data$signal[i_start:i_stop]
      
      ## update meta data start time
      data$meta$starttime <- limits[1]
      
      ## update meta data number of samples
      data$meta$n <- length(data$signal)
      
      ## calculate function call duration
      eseis_duration <- as.numeric(difftime(time1 = Sys.time(), 
                                            time2 = t_0, 
                                            units = "secs"))
      
      ## update object history
      data$history[[length(data$history) + 1]] <- 
        list(time = Sys.time(),
             call = "signal_clip()",
             arguments = eseis_arguments,
             duration = eseis_duration)
      names(data$history)[length(data$history)] <- 
        as.character(length(data$history))
      
    } else {
      
      ## clip signal
      data <- data[time >= limits[1] & time <= limits[2]]
    }
    
    ## return output
    return(data) 
  }
}
