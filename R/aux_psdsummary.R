#' Calculate aggregated PSDs over long time periods
#' 
#' The function generates a long time PSD with aggregated time and frequency
#' resolution. 
#' 
#' The function will calculate PSDs using the Welch method (see 
#' \code{signal_spectrogram}), with no overlap of the main time windows. The 
#' sub-windows will be automatically set to 10 % of the main window size and 
#' the overlap of sub-windows to 0.5.
#' 
#' @param start \code{POSIXct} value, start time for PSD 
#' calculation. If a character string (or vector) is provided, the function 
#' will try to convert it to POSIXct.
#' 
#' @param stop \code{POSIXct} value, stop time for PSD calculation. 
#' If a character string (or vector) is provided, the function will try to 
#' convert it to POSIXct.
#' 
#' @param ID \code{Character} vector, station ID to be processed
#' 
#' @param component \code{Character} value or vector, seismic component to 
#' be used. If omitted, the function will use \code{"BHZ"} by default.
#' 
#' @param dir \code{Character} value, path to directory that contains the 
#' seismic files. See \code{read_data} for details and constraints on the 
#' data structure.
#' 
#' @param window \code{Numeric} value, time window size for the PSD 
#' calculation. Should be appropriately large to avoid extensive calculation
#' times, usually at the order of 0.1 percent of the total PSD duration.
#' 
#' @param sensor \code{Character} value, sensor keyword for the deconvolution 
#' step (see \code{signal_deconvolve} for details and keywords). If omitted, 
#' no deconvolution will be attempted.
#' 
#' @param logger \code{Character} value, logger keyword for the deconvolution 
#' step (see \code{signal_deconvolve} for details and keywords). If omitted, 
#' no deconvolution will be attempted.
#' 
#' @param gain \code{Numeric} value, signal preamplification factor used for 
#' the deconvolution step (see \code{signal_deconvolve} for details. Default
#' value is \code{1}.
#' 
#' @param hours_skip \code{Integer} vector, one or more full hours that will
#' be excluded from the PSD generation. This optional value is useful if one 
#' wants to omit noisy daytime hours. 
#' 
#' @param res \code{Numeric} value, frequency resolution of the PSD, by 
#' default set to \code{1000}. This is used to reduce the resulting data 
#' size.
#' 
#' @param n \code{Numeric} value, number of times to try to find a time 
#' snippet with data to estimate the frequency vector length. By
#' default set to \code{100}. 
#' 
#' @param cpu \code{Numeric} value, fraction of CPUs to use. If omitted, 
#' only one CPU will be used.
#' 
#' @param verbose \code{Logical} value, option to show extended function 
#' information as the function is running. Default is \code{FALSE}.
#' 
#' @return \code{eseis} object, a spectrogram
#' 
#' @author Michael Dietze
#' 
#' @keywords eseis
#' 
#' @examples
#' 
#' \dontrun{
#' 
#' p <- aux_psdsummary(start = "2017-04-15 19:00:00 UTC",
#'                     stop = "2017-04-15 22:00:00 UTC",
#'                     ID = "RUEG1",
#'                     component = "BHE",
#'                     dir = "~/data/sac/",
#'                     sensor = "TC120s", 
#'                     logger = "Cube3ext",
#'                     window = 600,
#'                     res = 1000,
#'                     verbose = TRUE)
#'                     
#' plot(p)
#' 
#' }
#' 
#' @export aux_psdsummary
#' 
aux_psdsummary <- function(
    start,
    stop,
    ID,
    component = "BHZ",
    dir,
    window,
    sensor,
    logger,
    gain = 1,
    hours_skip,
    res = 1000,
    n = 100,
    cpu,
    verbose = FALSE
) {
  
  ## INPUT DATA CHECKS -------------------------------------------------------

  ## check/set time limits
  if(class(start)[1] != "POSIXct"){
    
    start <- try(as.POSIXct(start, tz = "UTC"))
    
    if(class(start)[1] != "POSIXct") {
      
      stop("Cannot convert start time to POSIXct!")
    }
  }
  
  if(class(stop)[1] != "POSIXct"){
    
    stop <- try(as.POSIXct(stop, tz = "UTC"))
    
    if(class(stop)[1] != "POSIXct") {
      
      stop("Cannot convert stop time to POSIXct!")
    }
  }
  
  if(start >= stop) {
    
    stop("Time interval is negative!")
  }
  
  ## check if data directory exists
  if(dir.exists(dir) == "FALSE") {
    
    stop("Data directory does not exist!")
  }
  
  ## check if deconvolution is to be done
  if(missing(sensor) | missing(logger)) {
    
    if(missing(xml) == FALSE) {
      
      ## set deconvolution flag
      deconvolve <- TRUE
      
      ## create dummy sensor, logger, gain, xml information
      sensor <- NA
      logger <- NA
      gain <- NA
      
    } else {
      
      ## set deconvolution flag
      deconvolve <- FALSE
      
      ## create dummy sensor, logger, gain information
      sensor <- NA
      logger <- NA
      gain <- NA
      xml <- NA
    }
    
  } else {
    
    ## set deconvolution flag
    deconvolve <- TRUE
    
    ## check sensor keyword validity
    if(sensor %in% names(eseis::list_sensor()) == FALSE) {
      
      stop("Sensor keyword is not supported!")
    }
    
    ## check logger keyword validity
    if(logger %in% names(eseis::list_logger()) == FALSE) {
      
      stop("Logger keyword is not supported!")
    }
    
    xml <- NA
  }
  
  ## check/set hours to skip
  if(missing(hours_skip) == TRUE) {
    
    hours_skip <- -1
  }
  
  ## PREPARATION --------------------------------------------------------------

  ## optionally print progress
  if(verbose == TRUE) {
    
    print(paste0("Processing station ", ID, "."))
  }
  
  ## create time slice vector
  t <- seq(from = start, to = stop - 0.1, by = window)
  
  ## extract hours of the dates
  t_hour <- as.numeric(format(x = t, format = "%H", tz = "UTC"))
  
  ## keep only hours specified in parameters
  t <- t[(t_hour %in% hours_skip) == FALSE]
  
  ## try to read one seismic file to estimate frequency vector length
  j <- 1
  while(j <= n) {
    
    if(verbose == TRUE) {
      
      print(paste("  Frequency estimation, attempt", j))
    }
    
    s_test <- try(eseis::read_data(start = t[runif(n = 1,
                                                   min = 1,
                                                   max = length(t))],
                                   duration = window,
                                   station = ID,
                                   component = component,
                                   dir = dir), silent = TRUE)
    
    if(class(s_test)[1] == "try-error") {
      
      j <- j + 1
    } else {
      
      j <- n + 1
    }
  }
  
  ## calculate dummy spectrogram
  p_test <- try(eseis::signal_spectrogram(data = s_test, 
                                     Welch = TRUE,
                                     window = window - 1,
                                     overlap = 0, 
                                     window_sub = round(window / 10),
                                     overlap_sub = 0.5))

  ## create aggregated frequency vector
  f_agg <- seq(from = min(p_test$PSD$f), 
               to = max(p_test$PSD$f), 
               length.out = res)
  
  ## initiate cluster
  cores <- parallel::detectCores()
  
  if(missing(cpu) == FALSE) {
    
    n_cpu <- floor(cores * cpu)
    cores <- ifelse(cores < n_cpu, cores, n_cpu)
  } else {
    
    cores <- 1
  }
  cl <- parallel::makeCluster(getOption("mc.cores", cores))
  
  ## make parameter list for parallel computation
  pars <- list(window = window,
               ID = ID,
               cmp = component,
               dir = dir,
               deconvolve = deconvolve,
               sensor = sensor,
               logger = logger,
               gain = gain,
               xml = xml,
               f_agg = f_agg)
  
  ## PROCESSING -----------------------------------------------------------------
  
  ## optionally print information
  print(paste0("  Preparation finished. ",
               length(t), 
               " PSDs to calculate in total, using ", 
               cores, 
               " CPUs."))
  
  ## get time before processing
  t_sys_0 <- Sys.time()
  
  ## process all time slices
  psd_list <- parallel::parLapply(cl = cl, X = t, fun = function(t, pars) {
    
    ## read file
    s <- try(eseis::read_data(start = t, 
                              duration = pars$window, 
                              station = pars$ID, 
                              component = pars$cmp, 
                              dir = pars$dir))
    
    ## deconvolve data
    if(pars$deconvolve == TRUE) {
      
      if(is.na(pars$xml == TRUE)) {
        
        s <- try(eseis::signal_deconvolve(data = s, 
                                          sensor = pars$sensor, 
                                          logger = pars$logger,
                                          gain = pars$gain))
      } else {
        
        s <- try(eseis::signal_deconvolve(data = s, 
                                          xml = pars$xml))
      }
      

    }
    
    ## calculate spectrogram
    p <- try(eseis::signal_spectrogram(data = s, 
                                       Welch = TRUE,
                                       window = pars$window - 1,
                                       overlap = 0, 
                                       window_sub = round(pars$window / 10),
                                       overlap_sub = 0.5))
    
    ## aggregate power by frequency
    p <- try(stats::approx(x = p$PSD$f, y = p$PSD$S[,1], 
                    xout = pars$f_agg)$y)
    
    ## return output
    if(class(p)[1] == "try-error") {
      
      return(rep(x = NA, times = length(pars$f_agg)))
    } else {
      
      return(p)
    }
    
  }, pars)
  
  ## stop cluster
  try(parallel::stopCluster(cl = cl))
  
  ## convert list to matrix
  psd <- try(do.call(rbind, psd_list))
  
  ## get time after processing
  t_sys_1 <- Sys.time()
  
  ## get time difference
  dt <- as.numeric(difftime(time1 = t_sys_1, 
                            time2 = t_sys_0, 
                            units = "mins"))
  
  ## get number of successfull calculations
  psd_ok <- try(do.call(c, lapply(X = psd_list, FUN = function(psd_list) {
    
    sum(is.na(psd_list)) == 0
  })))
  
  ## optionally, print processing information
  if(verbose == TRUE) {
  
    try(print(paste("  Processing finished. Time: ",
                    round(x = dt, digits = 1),
                    " min. ",
                    sum(psd_ok),
                    " successfull calculations (",
                    round(x = sum(psd_ok) / length(psd_ok) * 100, digits = 1),
                    " %).", 
                    sep = "")))  
  }
  
  ## prepare output
  p_test$PSD$S <- t(psd)
  p_test$PSD$t <- t
  p_test$PSD$f <- f_agg
  
  ## return output
  return(p_test)
}