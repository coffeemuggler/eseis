#' Kutosis based event picking
#' 
#' The function picks (identifies) events from continuous data using the 
#' kurtosis of the signal, and when it reaches beyond a defined threshold 
#' value. The end of an event is determined by the signal-to-noise ratio 
#' (SNR) 
#' 
#' Further reading: 
#' 
#' Baillard, C., Crawford, W.C., Ballu, V., Hibert, C., Mangeney, A., 2014. 
#' An automatic kurtosis-based p- and s-phase picker designed for local 
#' seismic networks. Bull. Seismol. Soc. Am. 104 (1), 394–409.
#' 
#' Hibert, C., Mangeney, A., Grandjean, G., Baillard, C., Rivet, D., 
#' Shapiro, N.M., Satriano, C., Maggi, A., Boissier, P., Ferrazzini, V., 
#' Crawford, W., 2014. Automated identification, location, and volume 
#' estimation of rockfalls at Piton de la Fournaise Volcano. J. Geophys. 
#' Res. Earth Surf. 119 (5), 
#' 1082–1105. http://dx.doi.org/10.1002/2013JF002970.
#' 
#' @param data \code{eseis} object, \code{numeric} vector or list of 
#' objects, data set to be processed.
#' 
#' @param on \code{Numeric} value, kurtosis threshold that defines the 
#' onset of an event.
#' 
#' @param off \code{Numeric} value, ratio of average post and pre event
#' signal amplitude inside a running window. Default is \code{1}. 
#' 
#' @param dur_min \code{Numeric} value, minimum duration of the event. This 
#' is required as the kurtosis routine tends to identify multipe picks in 
#' the beginning of an event.
#' 
#' @param dur_max \code{Numeric} value, maximum duration of the event. This
#' value can be omitted but would increase computational speed as it 
#' determines the length of samples to look for the amplitude ratio that 
#' signals the end of an event 
#' 
#' @param window_kurt \code{Numeric} value, size of the running window (in  
#' number of samples) in which the kurtosis is calculated.
#' 
#' @param window_amp \code{Numeric} value, size of the running window (in  
#' number of samples) in which the running mean is calculated.
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
#' ## load example data set
#' data(rockfall)
#' 
#' ## preprocess signal (aggregate to increase speed, filter, envelope)
#' s <- signal_aggregate(data = rockfall_eseis, n = 4)
#' s <- signal_filter(data = s, f = c(5, 20), lazy = TRUE)
#' e <- signal_envelope(data = s)
#' 
#' ## pick events based on signal kurtosis
#' p <- eseis::pick_kurtosis(data = e, 
#'                           window_kurt = 200, 
#'                           on = 15, 
#'                           off = 5, 
#'                           dur_min = 10, 
#'                           dur_max = 90, 
#'                           window_amp = 300)
#'
#' p$picks
#'                      
#' @export pick_kurtosis
#' 
pick_kurtosis <- function(
  data,
  on,
  off = 1,
  dur_min = 0,
  dur_max,
  window_kurt,
  window_amp,
  time,
  dt
) {
  
  ## check/set values
  if(missing(on) == TRUE) {
    
    stop("No on-threshold value provided!")
  }
  
  if(missing(off) == TRUE) {
    
    off <- 1
  }
  
  if(missing(dur_min) == TRUE) {
    
    dur_min <- 0
  }
  
  if(missing(dur_max) == TRUE) {
    
    dur_max <- NA
  }
  
  if(missing(window_kurt) == TRUE) {
    
    stop("No kurtosis window size provided!")
  }
  
  if(missing(window_amp) == TRUE) {
    
    warning("No amplitude window size provided, durations will be NA!")
    window_amp <- NA
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
                       FUN = eseis::pick_kurtosis, 
                       on = on,
                       off = off, 
                       dur_min = dur_min,
                       dur_max = dur_max,
                       window_kurt = window_kurt,
                       window_amp = window_amp,
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
                            off = off, 
                            dur_min = dur_min,
                            dur_max = dur_max,
                            window_kurt = window_kurt,
                            window_amp = window_amp,
                            time = time_in, 
                            dt = dt)
    
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
    
    ## calculate running kurtosis vector
    kurt <- .kurtosis_event(x = data, k = window_kurt)
    
    ## correct raw data set
    kurt <- c(utils::tail(x = kurt, n = window_kurt), 
              kurt[1:(length(kurt) - window_kurt)])
    
    ## classify by threshold value
    kurt_ok <- kurt >= on
    
    ## identify onset indices
    i_onset <- which(diff(c(kurt_ok[1], kurt_ok)) == 1)
    
    ## assign onset times
    t_start <- time[i_onset]
    
    ## remove spurious picks
    t_diff <- c(dur_min + 1, diff(t_start))
    i_ok <- which(t_diff >= dur_min)
    t_start <- t_start[i_ok]
    i_onset <- i_onset[i_ok]

    ## calculate duration and build output data set
    if(length(i_onset) > 0) {
  
      ## create ID vector
      ID <- seq(from = 1, to = length(i_onset))
      
      ## calculate running mean
      amp <- caTools::runmean(x = data, 
                              k = window_amp, 
                              align = "right")
      
      ## get amplitude ratios per event
      t_dur <- lapply(X = i_onset, FUN = function(i, amp, off, dur_max, dur_min) {
        
        ## get signal amplitude before event
        amp_pre <- amp[i]
        
        ## get number of samples to work with      
        if(is.na(dur_max) == TRUE) {
          
          n_max <- (length(data) - i)
        } else {
          
          n_max <- dur_max / dt
          if(i + n_max > length(amp)) {
            
            n_max <- length(amp) - i
          }
        }
        
        ## calculate amplitude ratio
        amp_rec <- amp[i:(i + n_max)] / amp_pre <= off
        amp_rec[1:(dur_min / dt)] <- FALSE
        
        ## get first occurence of threshold crossing
        i_off <- which(c(0, diff(amp_rec)) == 1)[1]
        
        ## return duration time
        return(i_off * dt)
        
      }, amp, off, dur_max, dur_min)
      
      t_dur <- do.call(c, t_dur)

      data_out <- data.frame(ID = ID,
                             start = t_start,
                             duration = t_dur,
                             kurtosis = kurt[i_onset])
      
    } else {
      
      data_out <- data.frame(ID = NA,
                             start = NA,
                             duration = NA,
                             kurtosis = NA)[-1,]
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
             call = "pick_kurtosis()",
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