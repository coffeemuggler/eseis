#' Pick seismic events with a sensor network approach, parallel version
#' 
#' The function allows detection of discrete seismic events using the STA-LTA
#' picker approach and including several stations of a seismic network. It 
#' will pick events at all input stations and identify events picked by more 
#' than a user defined minimum number of stations, within a given time window.
#' It further allows rejecting events that are too short or too long, or occur 
#' to short after a previous event. The function can be used to process long 
#' data archives organised in a consistent file structure. This is the parallel 
#' computation version of the routine. For the single CPU version, allowing to 
#' show verbose progress information, use \code{aux_picknetwork()}.
#' 
#' The data will be imported time slice by time slice. Optionally, a signal 
#' deconvolution can be done. The data will be filtered to isolate the 
#' frequency range of interest, and tapered to account for edge effects. The 
#' taper size is automatically set be the two-sided buffer (see below). 
#' Optionally, a signal envelope can be calculated as part of the preprocessing
#' work. For all successfully imported and preprocessed signal time snippets, 
#' events will be detected by the STL-LTA approach. Events from all input 
#' signals will be checked for a minium and maximum allowed event duration, set 
#' as thresholds by the user. All remaining events are checked for the number 
#' of stations of the network that did consistently detect an event. That means
#' an event must be found in each signal within a user defined time window, a 
#' window that corresponds to the maximum travel time of a seismic signal 
#' through the network. The minimum number of stations that detect the event 
#' can be set by the user. In addition, events can be rejected if they occur 
#' during a user defined ban time after the onset of a previous event, which 
#' assumes that no subsequent events are allowed during the implementation of 
#' long duration events happening.   
#' 
#' The time period to process is defined by \code{start}, \code{end} and the 
#' length of time snippets \code{res} (s). In addition, a left sided and 
#' right sided buffer can and should be added \code{buffer = c(60, 60)} (s), 
#' to account for artefacts during signal preprocessing and events that 
#' occur across the time snippets. The \code{start} and \code{end} time should
#' be provided in \code{POSIXct} format. If it is provided as text string, the 
#' function will try to convert to POSIXct assuming the time zone is UTC.
#' 
#' There must be at least two seismic stations. The seismic components 
#' \code{component} must be a vector of the same length as \code{station} and 
#' contain the corresponding information for each station. 
#' 
#' Deconvolution of the input signals can be done, although usually this is
#' not essential for just picking events if the filter frequencies are in the 
#' flat response range of the sensors. If wanted, one needs to provide the 
#' additional vectors (all of the same length as \code{station}) \code{sensor}, 
#' \code{logger} and optionally \code{gain} (otherwise assumed to be \code{1}). 
#' See \code{signal_deconvolve()} for further information on the deconvolution 
#' routine and the supported keywords for sensors and loggers. 
#' 
#' STA-LTA picking is usually done on signal envelopes \code{enevelope = TRUE}. 
#' However, for array seismology or other specific cases it might be useful to 
#' work with the waveforms directly, instead of with their envelopes. 
#' 
#' The STA-LTA routine requires information on the length of the STA window, 
#' the LTA window, the on-threshold to define the start of an event, the 
#' off-threshold to define the end of an event, and the option to freeze the 
#' STA-LTA ratio at the onset of and event. See the documentation of the 
#' function \code{pick_stalta()} for further information. 
#' 
#' Events that last shorter than \code{dur_min} or longer than \code{dur_max} 
#' are rejected. This criterion is applied before the test of how many stations
#' have commonly detected an event. Hence, the minimum and maximum duration 
#' criteria need to be fulfilled by an event for all seismic stations, or at 
#' least as many as defined as minimum number of stations \code{n_common}.
#' 
#' A valid event should be detected by more than one station. At least two 
#' stations (\code{n_common = 2}) should detect it to confirm it is not just a
#' local effect like rain drop impacts or animal activity. At least three 
#' stations (\code{n_common = 3}) are needed to locate an event. Detection at
#' more stations increases the validity of an event as proper signal. However, 
#' the likelihood of stations not operating correctly or that some stations 
#' may be too far to detect a smaller event limits the number of stations that 
#' are be set as (\code{n_common}).
#' 
#' An event that happens just outside a seismic network will generate seismic 
#' waves that travel through the network and will require a given time for 
#' that. The time is set by the longest inter-station distance and the apparent 
#' seismic wave velocity. For example, an event who's signal propagated with 
#' 1000 m/s through a network with a maximum aperture of 1000 m will need up to 
#' 1 s. Hence, the parameter  \code{t_common} should be set to \code{1}, in 
#' this case. This parameter is good to reject events that show a longer travel
#' time, e.g. because they are pressure waves that travel through the 
#' atmosphere, like helicopter signals or lightning strikes.
#' 
#' @param start \code{POSIXct} value, start time for event detection analysis. 
#' If a character string (or vector) is provided, the function will try to 
#' convert it to POSIXct.
#' 
#' @param stop \code{POSIXct} value, start time for event detection analysis. 
#' If a character string (or vector) is provided, the function will try to 
#' convert it to POSIXct.
#' 
#' @param res \code{Numeric} value, time resolution of the snippets used to 
#' detect events, in seconds. A useful value would be one hour 
#' (\code{res = 3600}).
#' 
#' @param buffer \code{Numeric} vector, of length two. Size of the two-sided 
#' buffer to account for signal pre-processing effects, in seconds. The first 
#' value is used for the left side, the second for the right side. Events that 
#' happen to be detected during the buffer time will be removed from the output
#' data set.
#' 
#' @param station \code{Character} vector, seismic station IDs to be processed. 
#' 
#' @param component \code{Character} value or vector, seismic component to 
#' be used. Information can be restricted to a single character, denoting the 
#' last character of the station component code, i.e. \code{"Z"} would use the 
#' first and last component of \code{c("BHZ", "HHE", "HHZ")}. If omitted, 
#' the function will use \code{"BHZ"} by default.
#'
#' @param dir \code{Character} value, path to directory that contains the 
#' seismic files. See \code{read_data} for details and constraints on the 
#' data structure.
#'
#' @param f \code{Numeric} Numeric vector of length two, cutoff frequencies 
#' (Hz) for the bandpass filter to be applied. If omitted, no filtering will 
#' be performed.
#' 
#' @param envelope \code{Logical} value, option to calculate signal envelopes
#' before detecting events. Default is \code{TRUE}.
#' 
#' @param sta \code{Numeric} value, duration of short-term window. Attention, 
#' this definition differs from the one in \code{pick_stalta}!
#' 
#' @param lta \code{Numeric} value, duration of long-term window. Attention, 
#' this definition differs from the one in \code{pick_stalta}!
#' 
#' @param on \code{Numeric} value, threshold value for event onset.
#' 
#' @param off \code{Numeric} value, threshold value for event end.
#' 
#' @param freeze \code{Logical} value, option to freeze lta value at start of 
#' an event. Useful to avoid self-adjustment of lta for long-duration events. 
#' 
#' @param dur_min \code{Numeric} value, minimum duration of an event (s). Picks 
#' with a STA-LTA duration below this threshold will be discarded.
#' 
#' @param dur_max \code{Numeric} value, maximum duration of an event (s). Picks 
#' with a STA-LTA duration longer than this threshold will be discarded.
#' 
#' @param n_common \code{Numeric} value, minimum number of stations at which 
#' an event has to be registered.
#' 
#' @param t_common \code{Numeric} value, time (s) within which the picks of an 
#' event need to be registered by seismic stations.
#' 
#' @param t_pause \code{Numeric} value, time period after the onset of an event 
#' during which no other event is allowed to happen.
#' 
#' @param cpu \code{Numeric} value, fraction of CPUs to use. If omitted, 
#' only one CPU will be used.
#' 
#' @param \dots Further arguments passed to the function, i.e. keywords for 
#' the signal deconvolution part. See details for further information.
#' 
#' @return \code{data frame}, detected events (start time, duration in 
#' seconds)
#' 
#' @author Michael Dietze
#' 
#' @keywords eseis
#' 
#' @examples
#' 
#' 
#' picks <- aux_picknetwork(start = "2017-04-09 01:00:00 UTC",
#'                          stop = "2017-04-09 02:00:00 UTC",
#'                          res = 1800,
#'                          buffer = c(90, 90),
#'                          station = c("RUEG1","RUEG2"),
#'                          component = rep("BHZ", 2),
#'                          dir = paste0(path.package("eseis"), "/extdata/"),
#'                          f = c(0.1, 0.2),
#'                          envelope = TRUE,
#'                          sta = 0.5,
#'                          lta = 300,
#'                          on = 3,
#'                          off = 1,
#'                          freeze = TRUE,
#'                          dur_min = 2,
#'                          dur_max = 90,
#'                          n_common = 2,
#'                          t_common = 5,
#'                          t_pause = 30)
#'                          
#' print(picks)
#'
#' @export aux_picknetworkparallel

aux_picknetworkparallel <- function(
    
  start,
  stop,
  res,
  buffer,
  station,
  component,
  dir,
  f,
  envelope = TRUE,
  sta,
  lta,
  on,
  off,
  freeze,
  dur_min,
  dur_max,
  n_common,
  t_common,
  t_pause,
  cpu,
  ...
  
) {
  
  ## CONSISTENCY CHECKS -------------------------------------------------------
  
  ## extract ...-arguments
  arg <- list(...)
  
  ## check/convert start time
  if(inherits(start, "POSIXct") == FALSE) {
    
    start <- try(as.POSIXct(start, tz = "UTC"))
    
    if(inherits(start, "try-error") == TRUE) {
      
      stop("Start date is not a POSIXct format!")
    }
  }
  
  ## check/convert stop time
  if(inherits(stop, "POSIXct") == FALSE) {
    
    stop <- try(as.POSIXct(stop, tz = "UTC"))
    
    if(inherits(stop, "try-error") == TRUE) {
      
      stop("Stop date is not a POSIXct format!")
    }
  }
  
  ## check if start time with time step is smaller than stop time
  if(start + res >= stop) {
    
    stop("Start time with time step is larger than stop time!")
  }
  
  ## check or set component
  if(missing(component) == TRUE) {
    
    component <- rep("Z", length(station))
  }
  
  if(length(component) != length(station)) {
    
    warning("n components does not match n stations, will be recycled!")
    
    component <- rep(x = component, length.out = length(station))
  }
  
  ## check or set filter frequencies
  if(missing(f) == TRUE) {
    
    f <- c(NA)
    
  } else if(length(f) != 2) {
    
    stop("f must contain two values!")
    
  } else if(f[1] >= f[2]) {
    
    stop("Filter frequencies flipped!")
  }
  
  ## check if file pattern is provided
  if("pattern" %in% names(arg) == FALSE) {
    
    arg$pattern <- "eseis"
  }
  
  ## check if interpolation option is provided
  if("interpolate" %in% names(arg) == FALSE) {
    
    arg$interpolate <- FALSE
  }
  
  ## check if logger, sensor, gain information is present, set decon. flag
  if("sensor" %in% names(arg) & "logger" %in% names(arg)) {
    
    if("gain" %in% names(arg) == FALSE) {
      
      arg$gain <- 1
    }
    
    deconvolve <- TRUE
    
  } else {
    
    deconvolve <- FALSE
  }
  
  ## PREPARATION --------------------------------------------------------------
  
  ## define time steps
  t <- seq(from = start, to = stop, by = res)
  
  ## compose parameter list
  par <- list(res = res,
              buffer = buffer,
              station = station,
              component = component,
              dir = dir, 
              arg = arg,
              deconvolve = deconvolve,
              f = f,
              envelope = envelope,
              sta = sta,
              lta = lta,
              on = on,
              off = off, 
              freeze = freeze,
              dur_min = dur_min,
              dur_max = dur_max,
              n_common = n_common,
              t_common = t_common,
              t_pause = t_pause)
  
  ## initiate cluster
  cores <- parallel::detectCores()
  
  if(missing(cpu) == FALSE) {
    
    n_cpu <- floor(cores * cpu)
    cores <- ifelse(cores < n_cpu, cores, n_cpu)
  } else {
    
    cores <- 1
  }
  
  ## start cluster
  cl <- parallel::makeCluster(getOption("mc.cores", cores))
  
  ## PROCESS TIME STEPS -------------------------------------------------------
  
  ## create output data set
  picks <- parallel::parLapply(cl = cl, X = t, fun = function(t, par) {
    
    ## read and preprocess station data
    s <- lapply(X = 1:length(par$station), FUN = function(j, t_i, par) {
      
      ## read data
      s <- try(eseis::read_data(start = t_i - par$buffer[1], 
                                duration = par$res + par$buffer[1] + 
                                  par$buffer[2],
                                station = par$station[j], 
                                component = par$component[j], 
                                dir = par$dir, 
                                pattern = par$arg$pattern, 
                                interpolate = par$arg$interpolate), 
               silent = TRUE)
      
      ## optionally deconvolve
      if(par$deconvolve == TRUE) {
        
        if(inherits(s, "eseis")) {
          
          s <- try(eseis::signal_deconvolve(data = s, 
                                            sensor = par$arg$sensor, 
                                            logger = par$arg$logger, 
                                            gain = par$arg$gain), 
                   silent = TRUE)
        }
        
      }
      
      ## filter signal
      if(inherits(s, "eseis") & !is.na(par$f[1])) {
        
        s <- try(eseis::signal_filter(data = s,
                                      f = par$f), 
                 silent = TRUE) 
      }
      
      ## taper signal
      if(inherits(s, "eseis")) {
        
        s <- try(eseis::signal_taper(data = s, 
                                     n = floor(min(par$buffer) / s$meta$dt))) 
      }
      
      ## optionally calculate envelope
      if(par$envelope == TRUE) {
        
        if(inherits(s, "eseis")) {
          
          s <- try(eseis::signal_envelope(data = s), 
                   silent = TRUE)
        }
      }
      
      ## return successfully preprocessed data set    
      if(inherits(s, "eseis")) {
        
        return(s)
      }
    }, t_i = t[1], par = par)
    
    ## remove erroneous data sets
    i_ok <- sapply(X = s, FUN = function(s) {is.null(s) == FALSE})
    s <- s[i_ok]
    
    ## check if minimum number of stations remains
    if(length(s) >= par$n_common) {
      
      ## pick events at all stations
      p <- lapply(X = s, FUN = function(s, par) {
        
        p <- try(eseis::pick_stalta(data = s, 
                                    sta = round(par$sta / s$meta$dt), 
                                    lta = round(par$lta / s$meta$dt), 
                                    freeze = par$freeze, 
                                    on = par$on, 
                                    off = par$off))
        
        if(inherits(p, "eseis")) {
          
          return(p$picks[,c(2, 3, 5)])
        }
      }, par = par)
      
      ## remove too long and too short events
      p <- lapply(X = p, FUN = function(p, par) {
        
        p <- p[p$duration >= par$dur_min & 
                 p$duration <= par$dur_max,]
        
        return(p)
        
      }, par)
      
      ## convert all pick start times to seconds
      t_all <- try(as.numeric(do.call(c, lapply(p, function(p) {p$start}))))
      
      ## find mutually common events
      p_ok <- lapply(X = p, FUN = function(p, par, t_all) {
        
        if(length(p$start) > 0) {
          
          ## create output data set
          p_ok_i <- rep(NA, length(p$start))
          n_ok_i <- rep(NA, length(p$start))
          
          ## process all picks per station
          for(i in 1:length(p$start)) {
            
            ## calculate time difference to all other picks
            dt <- as.numeric(p$start[i]) - t_all
            
            ## count picks within common time window
            n_ok <- sum(abs(dt) <= par$t_common)
            
            ## decide if pick is valid
            n_ok_i[i] <- n_ok
            p_ok_i[i] <- n_ok >= par$n_common
          }
          
          ## convert list to data frame and append to initial picks        
          p_ok_i <- cbind(p, p_ok_i, n_ok_i)
          p_ok_i <- p_ok_i[p_ok_i[,4] == TRUE,]
          
        } else {
          
          p_ok_i <- NULL
        }
        
        ## return commonly identified picks
        return(p_ok_i)
        
      }, par, t_all)
      
      ## collect all remaining picks
      p_ok <- do.call(rbind, p_ok)
      
      if(inherits(x = p_ok, what = "data.frame")) {
        
        ## sort picks by time
        p_ok <- p_ok[order(p_ok$start),]
        
        ## calculate time between events
        p_ok_lag <- p_ok$start[-1] - p_ok$start[-length(p_ok$start)]
        p_ok_lag <- c(max(par$t_common + 1, par$t_pause + 1), p_ok_lag)
        
        ## remove events within common time window size and pause range
        p_ok <- p_ok[p_ok_lag >= par$t_common &
                       p_ok_lag >= par$t_pause,]
      }
      
      if(inherits(x = p_ok, what = "data.frame")) {
        
        ## remove events beyond buffer range
        p_ok <- p_ok[p_ok$start >= t & p_ok$start <= t + par$res,]
        
      }
      
    } else {
      
      ## create empty output
      p_ok <- data.frame(start = NULL, duration = NULL, max = NULL,
                         p_ok_i = NULL, n_ok_i = NULL)
    }
    
    ## remove NA entries
    p_ok <- p_ok[!is.na(p_ok$start),]

    ## assign output
    return(p_ok)
  }, par = par)
  
  ## stop cluster
  try(parallel::stopCluster(cl = cl))
  
  ## convert list to data frame
  picks <- do.call(rbind, picks)
  
  ## account for empty object
  if(is.null(picks)) {
    
    ## create empty data frame
    picks <- data.frame(start = Sys.time(), duration = NA, 
                        max = NA, stations = NA)
    picks <- picks[-1,]
  }
  
  ## prepare output
  if(nrow(picks) == 0) {
    
    ## create empty data frame
    picks <- data.frame(start = Sys.time(), duration = NA, 
                        max = NA, stations = NA)
    picks <- picks[-1,]
    
  } else {
    
    ## remove temporary variables
    picks <- picks[,c(1,2,3,5)]
    
    ## rename variable names
    names(picks) <- c("start", "duration", "max", "stations")
    
    ## assign consistent row numbers
    row.names(picks) <- seq(from = 1, to = nrow(picks))
  }
  
  ## return output
  return(picks)
}