#' Calculate stal-lta-ratio.
#' 
#' The function calculates the ratio of the short-term-average and 
#' long-term-average of the input signal.
#' 
#' @param data \code{eseis} object, \code{numeric} vector or list of 
#' objects, data set to be processed.
#' 
#' @param time \code{POSIXct} vector, time vector of the signal(s). If not 
#' provided, a synthetic time vector will be created.
#' 
#' @param dt \code{Numeric} value, sampling period. If omitted, either 
#' estimated from \code{time} or set to 0.01 s (i.e., f = 100 Hz). 
#' 
#' @param sta \code{Numeric} value, number of samples for short-term window.
#' 
#' @param lta \code{Numeric} value, number of samples for long-term window.
#' 
#' @param freeze \code{Logical} value, option to freeze lta value at start of 
#' an event. Useful to avoid self-adjustment of lta for long-duration events. 
#' 
#' @param on \code{Numeric} value, threshold value for event onset.
#' 
#' @param off \code{Numeric} value, threshold value for event end.
#' 
#' @return \code{data frame}, detected events (ID, start time, duration in 
#' seconds).
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
#' ## filter signal
#' rockfall_f <- signal_filter(data = rockfall_eseis,
#'                             f = c(1, 90), 
#'                             p = 0.05)
#'                     
#' ## calculate signal envelope
#' rockfall_e <- signal_envelope(data = rockfall_f)
#' 
#' ## pick earthquake and rockfall event
#' signal_stalta(data = rockfall_e,
#'               sta = 100, 
#'               lta = 18000, 
#'               freeze = TRUE, 
#'               on = 5, 
#'               off = 3)
#'               
#'                      
#' @export signal_stalta
signal_stalta <- function(
  data,
  time,
  dt,
  sta,
  lta,
  freeze = FALSE,
  on,
  off
) {
  
  ## check/store presence of time vector
  if(missing(time) == TRUE) {
    
    time_in <- NULL
  } else {
    
    time_in <- time
  }
  
  if(missing(dt) == TRUE) {
    
    dt <- NULL
  } else {
    
    dt <- dt
  }

  ## check data structure
  if(class(data)[1] == "list") {
    
    ## apply function to list
    data_out <- lapply(X = data, 
                       FUN = eseis::signal_stalta,
                       time = time_in,
                       dt = dt,
                       sta = sta,
                       lta = lta,
                       freeze = freeze,
                       on = on,
                       off = off)
    
    ## return output
    return(data_out)
    
  } else {
    
    ## get start time
    eseis_t_0 <- Sys.time()
    
    ## collect function arguments
    eseis_arguments <- list(data = "",
                            time = time_in,
                            dt = dt,
                            sta = sta,
                            lta = lta,
                            freeze = freeze,
                            on = on,
                            off = off)
    
    ## homogenise data structure
    if(class(data)[1] == "eseis") {
      
      ## set eseis flag
      eseis_class <- TRUE
      
      ## store initial object
      eseis_data <- data
      
      ## extract signal vector
      data <- eseis_data$signal
      
      ## extract sampling period
      dt <- eseis_data$meta$dt
      
      ## generate time vector
      time <- seq(from = eseis_data$meta$starttime, 
                  by = dt, 
                  length.out = eseis_data$meta$n)
      
    } else {
      
      ## set eseis flag
      eseis_class <- FALSE
      
      ## assign dt and time
      if(is.null(dt) == TRUE) {
        
        if(is.null(time) == TRUE) {
          
          dt <- 1 / 200
          
          warning("No dt provided. Set to 1 / 200 s by default!")
        } else {
          
          if(length(time) > 1000) {
            
            time_dt <- time[1:1000]
            
          } else {
            
            time_dt <- time
            
          }
          
          dt_estimate <- mean(x = diff(x = as.numeric(time)), 
                              na.rm = TRUE)
          print(paste("dt estimated from time vector (", 
                      signif(x = dt_estimate),
                      " s)",
                      sep = ""))
          
          dt <- dt_estimate
        }
        
        ## handle missing time vector
        if(is.null(time) == TRUE) {
          
          time <- seq(from = 
                        as.POSIXct(x = strptime(x = "0000-01-01 00:00:00",
                                                format = "%Y-%m-%d %H:%M:%S", 
                                                tz = "UTC"), tz = "UTC"),
                      by = dt,
                      length.out = length(data))
          
          print("No or non-POSIXct time data provided. Default data created!")
          
        }
        
      }
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
    
    ## create output data set
    event <- rep(x = 0, 
                 times = length(data))
    
    if(freeze == TRUE) {
      
      event <- .stalta_event_freeze(event_length = length(event),
                                    data_sta = data_sta,
                                    data_lta = data_lta,
                                    on = on,
                                    off = off)
    } else {
      
      ## calculate ratio
      ratio <- data_sta / data_lta
      ratio[is.na(ratio)] <- 0
      
      ## assign event trigger
      event <- .stalta_event_nofreeze(event_length = length(event),
                                      ratio = ratio,
                                      on = on,
                                      off = off)
    }

    ## calculate event state switches
    event_diff <- diff(x = event)

    ## asign on and off states
    event_on <- time[event_diff == 1]
    event_off <- time[event_diff == -1]

    ## calculate event duration
    event_duration <- as.numeric(difftime(time1 = event_off, 
                                          time2 = event_on, 
                                          units = "sec"))
    
    ## create ID vector
    ID <- seq(from = 1, 
              along.with = event_duration)
    
    ## build output data set
    if(length(ID) > 0) {
      
      data_out <- data.frame(ID = ID,
                             start = event_on,
                             duration = event_duration)
      
    } else {
      
      data_out <- data.frame(ID = NA,
                             start = NA,
                             duration = NA)[-1,]
    }
    
    ## optionally rebuild eseis object
    if(eseis_class == TRUE) {
      
      ## assign aggregated signal vector
      eseis_data <- list(picks = data_out,
                         history = eseis_data$history)
      
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
      
      ## set S3 class name
      class(eseis_data)[1] <- "eseis"
      
      ## assign eseis object to output data set
      data_out <- eseis_data
    }

    ## return output
    return(data_out)
  }
}