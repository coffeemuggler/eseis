#' Signal correlation based event picking
#' 
#' The function picks (identifies) events from continuous data by comparing 
#' the data patterns against a template signal using Pearson's correlation 
#' coefficient, defining an event when that coefficient is above a threshold 
#' value.
#' 
#' @param data \code{eseis} object, \code{numeric} vector or list of 
#' objects, data set to be processed.
#' 
#' @param on \code{Numeric} value, minimum correlation coefficient to define 
#' event occurence.
#' 
#' @param template \code{eseis} object or signal vector, template event 
#' with which the data set is correlated.
#' 
#' @param dur_min \code{Numeric} value, minimum duration of the event. This 
#' is required as the routine tends to identify multipe picks with similarly 
#' high correlation coefficients due to autocorrelation effects. If omitted, 
#' \code{dur_min} is set to \code{0}, i.e., all picks are returned.
#' 
#' @param time \code{POSIXct} vector, time vector of the signal(s). If not 
#' provided, a synthetic time vector will be created.
#' 
#' @param dt \code{Numeric} value, sampling period. If omitted, either 
#' estimated from \code{time} or set to 0.01 s (i.e., f = 100 Hz). 
#' 
#' @return \code{data.frame}, picked events.
#' 
#' @author Michael Dietze
#' 
#' @keywords eseis
#' 
#' @examples
#' 
#' ## create synthetic event signal
#' p <- sin(seq(0, 10 * pi, by = 0.35)) * 0.2 * 
#'   (1 + sin(seq(0, pi, length.out = 90)))^5
#' 
#' ## show event signal
#' plot(p, type = "l")
#' 
#' ## create synthetic noise signal
#' x <- runif(n = 1000, min = -1, max = 1)
#' t <- seq(from = Sys.time(), length.out = length(x), by = 1/200)
#' ii <- floor(runif(n = 3, min = 100, max = 900))
#' 
#' ## add events to noise
#' for(k in 1:length(ii)) {
#'   
#'   nn <- ii[k]:(ii[k] + 89)
#'   x[nn] <- x[nn] + p
#' }
#' 
#' ## show resulting time series
#' plot(x = t, y = x, type = "l")
#' 
#' ## pick events based on template
#' picks <- eseis::pick_correlation(data = x, 
#'                                  on = 0.8, 
#'                                  template = p, 
#'                                  time = t, 
#'                                  dt = 1/200)
#'                                  
#' ## show result
#' print(picks)
#'                      
#' @export pick_correlation
#' 
pick_correlation <- function(
  data,
  on,
  template,
  dur_min,
  time,
  dt
) {
  
  ## check/set values
  if(missing(on) == TRUE) {
    
    stop("No on-threshold value provided!")
  }

  if(missing(template) == TRUE) {
    
    stop("No template event provided!")
  }
  
  if(missing(dur_min) == TRUE) {
    
    dur_min <- 0
  }
    
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
                       FUN = eseis::pick_correlation, 
                       on = on,
                       template = template,
                       dur_min = dur_min,
                       time = time_in, 
                       dt = dt)
    
    ## return output
    return(data_out)
    
  } else {
    
    ## get start time
    eseis_t_0 <- Sys.time()
    
    ## collect function arguments
    eseis_arguments <- list(data = "",
                            on = on,
                            template = template,
                            dur_min = dur_min,
                            time = time_in, 
                            dt = dt)
    
    ## check template event structure
    if(class(template) == "eseis") {
      
      template <- template$signal
    }
    
    ## check if input object is of class eseis
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
    
    ## calculate running correlation vector
    corr <- .correlation_event(x = data, y = template)
    
    ## classify events based on threshold
    corr_class <- corr >= on
    
    ## identify event onset
    corr_onset <- which(c(0, diff(corr_class)) == 1)
    
    ## remove all picks within minimum duration of the first pick
    corr_onset_diff <- c(dur_min + 1, diff(corr_onset))
    corr_onset <- corr_onset[which(corr_onset_diff >= dur_min / dt)]

    ## calculate duration and build output data set
    if(length(corr_onset) > 0) {

      ## create event ID vector
      ID <- seq(from = 1, to = length(corr_onset))
      
      data_out <- data.frame(ID = ID,
                             start = time[corr_onset],
                             duration = rep(NA, length(corr_onset)),
                             cor = corr[corr_onset])

    } else {

      data_out <- data.frame(ID = NA,
                             start = NA,
                             duration = NA,
                             cor = NA)[-1,]
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
             call = "pick_correlation()",
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