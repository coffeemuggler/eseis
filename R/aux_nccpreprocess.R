#' Preprocess and correlate seismic signals for ambient noise cross correlation
#' 
#' The function processes snippets of two seismic signals to perform ambient
#' noise cross correlation analysis as pre-step for estimating changes in the 
#' relative seismic velocity. It reads snippets of a given window length and 
#' fraction of overlap as well as sub windows and respective overlaps therein, 
#' preprocesses it (see details), calculates the cross correlation function 
#' within a given lag time window, stacks the sub window results and returns 
#' a matrics of correlation function steps for further analysis (see
#' \code{aux_nccprocess}).
#' 
#' The workflow is based on the Python modules miic and MSnoise with 
#' modifications due to the R environment constraints. 
#' 
#' Signals can either come from one station or a pair of stations. In the 
#' former case the components must be different. In the latter case, and if 
#' the components are identical horizontal ones (e.g., BHE and BHE or BHN and 
#' BHN), it is useful to set the option \code{rotate = TRUE}. This will align 
#' the two components to avoid spurious phase offsets. 
#' 
#' @param data \code{list} object of length two, containing two \code{eseis} 
#' objects to be preprocessed. Alternatively, the arguments \code{period}, 
#' \code{station}, \code{component} and \code{dir} can be used to process an 
#' organised repository of seismic data files.
#' 
#' @param period \code{POSIXct} vector of length two, time period for which  
#' the analysis shall be performed. 
#' 
#' @param station \code{Character} vector, station ID, either one station or 
#' two stations are allowed. If one station is used, the components should be 
#' different ones. Station IDs must correspond to the ID in the file name of 
#' the data directory structure (cf. \code{aux_organisecubefiles}).
#' 
#' @param component \code{Character} vector of length two, seismic components, 
#' which must correspond to the component name in the file name of the data   
#' directory structure (cf. \code{aux_organisecubefiles}).
#' 
#' @param dir \code{Character} value, path to the seismic data directory. See
#' \code{aux_organisecubefiles} for details about the required file and  
#' directory structure.
#' 
#' @param window \code{Numeric} value, time window length in hours, used to 
#' calculate individual correlations. Default is \code{24} (one day).
#' 
#' @param overlap \code{Numeric} value, fraction of window overlap. Default 
#' is \code{0} (no overlap, but seamless succession). It is possible 
#' to specify negative values, which will result in spaced windows. A value of
#' for example \code{-1} will cause gaps as large as the analysis windows.
#' 
#' @param window_sub \code{Numeric} value, length of the sub-window in hours,  
#' used to calculate correlations before stacking. Default is \code{1} (i.e.,
#' 24 snippets to be stacked per day).
#' 
#' @param overlap_sub \code{Numeric} value, fraction of sub-window overlap. 
#' Default is \code{0} (no overlap, but seamless succession). It is possible 
#' to specify negative values, which will result in spaced windows. A value of
#' for example \code{-1} will cause gaps as large as the analysis windows.
#' 
#' @param lag \code{Numeric} value, cross correlation lag time of interest, 
#' in seconds. Default is \code{120} (2 minutes). This value determines the 
#' length of the output data set.
#' 
#' @param dt \code{Numeric} value, target sampling period of the output 
#' signal. Option can be used for aggregating (downsampling) the input signals
#' to save computation time and reach necessary consistency of sampling 
#' periods of signal pairs. If omitted, the signals are automatically 
#' aggregated to reach a common sampling period. See 
#' \code{aux_homogeniseperiod} for details.
#' 
#' @param deconvolve \code{logical} value, option to deconvolve signals. 
#' Default is \code{FALSE}. If set to \code{TRUE}, arguments \code{"sensor"},
#' \code{"logger"} and \code{"gain"} must be provided, each as vector of 
#' length two, representing the two signals, each.
#' 
#' @param taper \code{Numeric} value, fraction of the signals to be tapered 
#' to avoid edge effects. Default is \code{0.01} (1 percent).
#' 
#' @param f \code{Numeric} vector, filter frequencies. Either of length
#' one or two. If of length one, the filter type must be specified, 
#' additionally (\code{type = "HP"} or \code{type = "LP"}). Otherwise it is 
#' assumed that the filter is a bandpass one (\code{type = "BP"}). See
#' \code{signal_filter} for details. If not specified, no filtering is done.
#' 
#' @param cut \code{Numeric} value, option to cut the signal vectors above a 
#' provided threshold denoting n times to standard deviations of the signals. 
#' If omitted, no cutting is performed.
#' 
#' @param sign \code{Logical} value, option to convert the signals to signed 
#' one bit vectors. Default is \code{FALSE}. Will override any effect of the 
#' argument \code{cut}.
#' 
#' @param normalise \code{Logical} value, option to normalise correlation 
#' function by signal range. Default is \code{TRUE}.
#' 
#' @param cpu \code{Numeric} value, fraction of CPUs to use for parallel 
#' processing. If omitted, one CPU is used.
#' 
#' @param \dots Further arguments passed to the function.
#' 
#' @return A \code{list} object containing the stacked correlation results and 
#' meta information. The element \code{correlation} contains a \code{matrix} 
#' with correlation data organised in rows. The element \code{meta} holds the 
#' respective meta information.
#' 
#' @author Michael Dietze
#' 
#' @keywords eseis
#' 
#' @examples
#' 
#' ## preprocess data and calculate window-wise cross-correlation functions
#' cc <- aux_nccpreprocess(period = as.POSIXct(x = c("2017-04-09 00:00:00",
#'                                                   "2017-04-09 03:00:00"),
#'                                             tz = "UTC"),
#'                         station = c("RUEG1", "RUEG2"),
#'                         component = c("BHZ", "BHZ"),
#'                         dir = paste0(system.file("extdata", 
#'                                                  package="eseis"), "/"),
#'                         window = 1.5 * 3600,
#'                         overlap = 0.5,
#'                         window_sub = 1800,
#'                         overlap_sub = 0,
#'                         lag = 20,
#'                         deconvolve = TRUE,
#'                         sensor = "TC120s",
#'                         logger = "Cube3ext",
#'                         gain = 1,
#'                         f = c(0.1, 0.5),
#'                         sign = TRUE)
#' 
#' ## define colour scheme
#' cp <- colorRampPalette(colors = c("red", "grey", "blue"))
#' 
#' ## plot image of cross-correlation functions
#' image(x = cc$time, 
#'       y = cc$lag, 
#'       z = cc$correlation, 
#'       col = cp(200))
#'                      
#' @export aux_nccpreprocess
aux_nccpreprocess <- function(
  data,
  period,
  station,
  component, 
  dir,
  window,
  overlap = 0,
  window_sub,
  overlap_sub = 0,
  lag = 120,
  dt, 
  deconvolve,
  taper = 0.01,
  f, 
  cut,
  sign,
  normalise,
  cpu,
  ...
) {
  
  ## check/set arguments ------------------------------------------------------
  
  ## check if either data or alternative data import information is provided
  if(missing(data) == TRUE) {
    
    if(missing(period) | 
       missing(station) | 
       missing(component) |
       missing(dir)) {
      
      stop("Neither data set nor data import information provided!")
    }
    
    ## check if period is of length two, or make it length two
    if(length(period) < 2) {
      
      stop("Argument period must contain two values!")
    } else if(length(period) > 2) {
      
      warning("Argument period contains > 2 values. Only two first are used.")
      
      period <- period[1:2]
    }
    
    ## check if station is of length one or two, or make it length two
    if(length(station) < 1) {
      
      stop("Argument station must contain one or two values!")
    } else if(length(station) > 2) {
      
      warning("Period contains > 2 values. Only two first are used.")
      
      station <- station[1:2]
    }
    
    ## check if period is of right format
    if(class(period[1])[1] != "POSIXct") {
      
      stop("Processing period must be of POSIXct format!")
    } else {
      
      if(period[2] <= period[1]) {
        
        stop("No valid processing period limits!")
      }
    }
    
    ## check if input directory exists
    if(dir.exists(paths = dir) == FALSE) {
      
      stop("Data input directory does not exist!")
    } else {
      
      ## check if JD directory of period start is present
      if(dir.exists(paths = paste(dir, 
                                  format(period[1], format = "%Y"),
                                  format(period[1], format = "%j"),
                                  sep = "/")) == FALSE) {
        
        stop("Input directory does not contain data of processing period!")
      }
      
      ## check if JD directory of period end is present
      if(dir.exists(paths = paste(dir, 
                                  format(period[2], format = "%Y"),
                                  format(period[2], format = "%j"),
                                  sep = "/")) == FALSE) {
        
        warning("Input directory does not contain data of processing period!")
      }
      
      ## check if station 1 is present for period start
      if(length(list.files(path = paste(dir, 
                                        format(period[1], format = "%Y"),
                                        format(period[1], format = "%j"),
                                        sep = "/"),
                           pattern = station[1])) < 1) {
        
        stop(paste("Station", station[1], "not found in input directory!"))
      }
      
      ## check if station 2 is present for period start
      if(length(list.files(path = paste(dir, 
                                        format(period[1], format = "%Y"),
                                        format(period[1], format = "%j"),
                                        sep = "/"),
                           pattern = station[2])) < 1) {
        
        warning(paste("Station", station[2], "not found in input directory!"))
      }
    }
  }
  
  ## optionally set arguments if data is present
  if(missing(data) == FALSE) {
    
    ## get start time of data sets
    data_start <- do.call(c, lapply(X = data, FUN = function(data) {
      
      data$meta$starttime
    }))
    
    ## get length of data sets
    data_end <- do.call(c, lapply(X = data, FUN = function(data) {
      
      data$meta$starttime + data$meta$n * data$meta$dt
    }))
    
    ## get period fully covered by data sets
    period <- c(max(data_start), min(data_end))
    
    ## get station IDs
    station <- do.call(c, lapply(X = data, FUN = function(data) {
      
      trimws(data$meta$station)
    }))
    
    ## get components
    component <- do.call(c, lapply(X = data, FUN = function(data) {
      
      trimws(data$meta$component)
    }))
  } else {
    
    data <- list(period = period,
                 station = station,
                 component = component,
                 dir = dir)
  }
  
  ## check/set window length
  if(missing(window) == TRUE) {
    window <- 24 * 3600
  }
  
  ## check/set sub-window length
  if(missing(window_sub) == TRUE) {
    window_sub <- 3600
  }
  
  ## account for negative overlap values
  if(overlap < 0) {
    
    spacing <- -overlap + 1
  } else {
    
    spacing <- 1 - overlap
  }
  
  ## account for negative overlap values
  if(overlap_sub < 0) {
    
    spacing_sub <- -overlap_sub + 1
  } else {
    
    spacing_sub <- 1 - overlap_sub
  }
  
  ## check/set common dt
  if(missing(dt) == TRUE) {
    
    dt_flag <- FALSE
    dt <- NA
  } else {
    
    dt_flag <- TRUE
  }
  
  ## check/set deconvolve argument
  if(missing(deconvolve) == TRUE) {
    
    deconvolve <- NA
  }
  
  ## check/set filter frequency argument
  if(missing(f) == TRUE) {
    
    f <- NA
  }
  
  ## check/set cut argument
  if(missing(cut) == TRUE) {
    
    cut <- NA
  }
  
  ## check/set sign argument
  if(missing(sign) == TRUE) {
    
    sign <- FALSE
  }
  
  ## check/set normalise argument
  if(missing(normalise) == TRUE) {
    
    normalise <- TRUE
  }
  ## extract additional arguments
  args <- list(...)
  
  ## check/set sensor argument
  if ("sensor" %in% names(args)) {
    
    sensor <- args$sensor
  }
  else {
    
    sensor = NA
  }
  
  ## check/set logger argument
  if ("logger" %in% names(args)) {
    
    logger <- args$logger
  }
  else {
    
    logger = NA
  }
  
  ## check/set gain argument
  if ("gain" %in% names(args)) {
    
    gain <- args$gain
  }
  else {
    
    gain = NA
  }
  
  ## check/set filter type arguments
  if ("type" %in% names(args)) {
    
    type <- args$type
  }
  else {
    
    if(length(f) == 2) {
      
      type <- "BP"
    } else {
      
      type <- NA
      f <- NA
    }
  }
  
  ## check/set fraction of CPUs to use
  if(missing(cpu) == TRUE) {
    
    cpu <- NA
  }
  
  ## build processing argument list
  arguments <- list(period = period,
                    station = station,
                    component = component,
                    dir = dir,
                    window = window,
                    window_sub = window_sub, 
                    overlap = overlap,
                    spacing = spacing,
                    overlap_sub = overlap_sub,
                    spacing_sub = spacing_sub,
                    lag = lag,
                    dt_flag = dt_flag,
                    dt = dt, 
                    deconvolve = deconvolve,
                    sensor = sensor,
                    logger = logger,
                    gain = gain,
                    taper = taper,
                    f = f, 
                    type = type,
                    cut = cut,
                    sign = sign,
                    normalise = normalise)
  
  ## Generation of time slices and processing ---------------------------------
  
  ## make start time vector of time slices (0.0001 is arbitrary to avoid
  ## start time sequence reaching the actual end of the period)
  window_seq <- seq(from = arguments$period[1], 
                    to = arguments$period[2] - 0.0001, 
                    by = arguments$window * arguments$spacing)
  
  ## create sub window vector
  window_sub_seq <- 
    lapply(X = as.list(window_seq), 
           FUN = function(window_seq,
                          arguments) {
             
             window_sub <- seq(from = window_seq, 
                               to = window_seq + arguments$window * 
                                 arguments$spacing - 0.0001, 
                               by = arguments$window_sub * 
                                 arguments$spacing_sub)
             
             window_top <- rep(x = window_seq, 
                               times = length(window_sub))
             
             return(data.frame(window = window_top,
                               window_sub = window_sub))
             
           }, arguments)
  
  ## convert list to data frame
  window_sub_seq <- do.call(rbind, 
                            window_sub_seq)
  
  ## convert data frame to list
  windows <- vector(mode = "list", 
                    length = nrow(window_sub_seq))
  
  for(i in 1:length(windows)) {
    
    windows[[i]] <- window_sub_seq[i,]
  }
  
  ## detect and adjust number of cores to use
  cores <- parallel::detectCores()
  
  if(is.na(cpu) == FALSE) {
    
    n_cpu <- floor(cores * cpu)
    cores <- ifelse(cores < n_cpu, cores, n_cpu)
  } else {
    
    cores <- 1
  }
  
  ## initiate cluster
  cl <- parallel::makeCluster(getOption("mc.cores", cores))
  
  ## process all time snippets
  correlation <- parallel::parLapply(
    cl = cl, 
    X = windows, 
    fun = function(windows, data, arguments) {
      
      ## read/get time snippets of input data
      if(names(data)[1] == "period") {
        
        ## read new data
        s <- try(
          list(x = eseis::aux_getevent(start = windows$window_sub, 
                                          duration = arguments$window_sub, 
                                          station = arguments$station[1], 
                                          component = arguments$component[1], 
                                          dir = arguments$dir),
                  y = eseis::aux_getevent(start = windows$window_sub, 
                                          duration = arguments$window_sub, 
                                          station = arguments$station[2], 
                                          component = arguments$component[2], 
                                          dir = arguments$dir)))
      } else {
        
        ## clip existing data
        s <- try(
          list(x = eseis::signal_clip(data = data[[1]], 
                                      limits = c(windows$window_sub, 
                                                 windows$window_sub + 
                                                   arguments$window_sub)),
               y = eseis::signal_clip(data = data[[2]], 
                                      limits = c(windows$window_sub, 
                                                 windows$window_sub + 
                                                   arguments$window_sub))))
      }
      
      if(class(s) == "try-error") {
        
        return(rep(NA, times = 2 * arguments$lag * (1 / arguments$dt) + 1))
      }
      
      ## get common dt of the two signals
      if(arguments$dt_flag == FALSE) {

        dt_common <- eseis::aux_commondt(data = s)
        dt_out <- dt_common$dt
        agg_out <- dt_common$agg

        print(paste("Common sampling interval is", dt_out))
      } else {

        dt_common <- eseis::aux_commondt(data = s,
                                         dt = arguments$dt)
        dt_out <- dt_common$dt
        agg_out <- dt_common$agg
      }
      
      ## optionally, aggregate signals
      if(sum(agg_out != 1) > 0) {
        
        s <- list(x = eseis::signal_aggregate(data = s[[1]],
                                              n = agg_out[1]),
                  y = eseis::signal_aggregate(data = s[[2]],
                                              n = agg_out[2]))
      }
      
      ## remove mean from signals
      s <- eseis::signal_demean(data = s)
      
      ## detrend data
      s <- eseis::signal_detrend(data = s)

      ## optionally taper signals
      if(arguments$taper > 0) {
        
        s <- eseis::signal_taper(data = s, 
                                 p = arguments$taper)
      }
      
      ## optionally filter signals
      if(!is.na(arguments$f[1]) == TRUE) {
        
        s <- eseis::signal_filter(data = s, 
                                  f = arguments$f,
                                  type = arguments$type)
      }
      
      ## optionally deconvolve signals
      if(!is.na(arguments$deconvolve) == TRUE) {
        
        s <- eseis::signal_deconvolve(data = s, 
                                      sensor = arguments$sensor, 
                                      logger = arguments$logger, 
                                      gain = arguments$gain)
      }
      
      ## detrend data
      s <- eseis::signal_detrend(data = s)
      
      ## optionally sd-cut signals
      if(is.na(arguments$cut) == FALSE) {
        
        s <- eseis::signal_cut(data = s, 
                               k = arguments$cut)
      }
      
      ## optionally sign-cut signals
      if(arguments$sign == TRUE) {

        s$x$signal <- sign(s$x$signal)
        s$y$signal <- sign(s$y$signal)
      }
      
      ## get number of samples before padding
      n_samples <- lapply(X = s, FUN = function(s) {
        
        s$meta$n
      })
      n_samples <- mean(unlist(n_samples))
      
      ## pad signals with zeros
      s <- eseis::signal_pad(data = s)
      
      ## Fourier transform signals
      s_fft <- lapply(X = s, FUN = function(s) {
        
        s$signal <- fftw::FFT(x = s$signal)
        
        return(s)
      })
      
      ## calculate cross-correlation function
      corr <- Conj(s_fft$x$signal) * s_fft$y$signal
      corr <- Re(fftw::IFFT(x = corr)) / length(corr)
      corr <- c(corr[(length(corr) - n_samples + 1):length(corr)], 
                corr[1:(n_samples)])
      
      ## optionally normalise correlation function
      if(arguments$normalise == TRUE) {
        
        e <- lapply(X = s_fft, FUN = function(s_fft) {
          
          Re(sqrt(mean(fftw::IFFT(s_fft$signal)^2)))
        })
        e <- prod(unlist(e))
        
        corr <- corr / e
      }
      
      ## clip correlation function to lag time of interest
      lag_samples <- arguments$lag * (1 / dt_out)
      
      i_lag <- abs(seq(from = -n_samples + 1, to = n_samples))
      
      corr_out <- corr[i_lag <= lag_samples]

      ## return output
      return(list(corr_out = corr_out,
                  dt_out = dt_out))
      
    }, data = data, arguments = arguments)
  
  ## stop cluster
  parallel::stopCluster(cl = cl)
  
  ## extract output sampling period
  dt_out <- median(do.call(c, lapply(X = correlation, 
                                     FUN = function(correlation) {
                                       correlation$dt_out
                                     })))
  
  ## extract correlation data
  correlation <- do.call(rbind, lapply(X = correlation, 
                                   FUN = function(correlation) {
                                     correlation$corr_out
                                   }))
  
  ## stack data within sub windows
  correlation_stack <- matrix(nrow = length(window_seq), 
                              ncol = ncol(correlation))
  
  for(i in 1:nrow(correlation_stack)) {
    
    correlation_stack[i,] <- 
      colMeans(x = correlation[window_sub_seq[,1] == window_seq[i],])
  }
  
  ## create lag time vector
  lag_t <- seq(from = 1, 
               to = ncol(correlation_stack)) * dt_out - arguments$lag - dt_out
  
  ## create output
  data_out <- list(time = window_seq,
                   lag = lag_t,
                   correlation = correlation_stack)
  
  ## return output
  return(data_out)
}
