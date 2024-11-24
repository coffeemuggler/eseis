#' Noise Cross Correlation routine
#' 
#' The function creates a cross correlation time series of two input data 
#' sets. The input data is cut into overlapping snippets and, optionally,  
#' further smaller sub snippets for averaging the results per time snippet. 
#' The data can be made subject to a series of optional preprocessing steps, 
#' i.e. be aggregated, deconvolved, filtered, cut by standard deviation, and 
#' sign-cut. the cross correlation function is calculated and returned for a 
#' defined lag time window. The output of the function is supposed to be 
#' used as input for the function \code{ncc_process()}. 
#' 
#' The sampling interval (\code{dt} must be defined). It is wise to set it 
#' to more than twice the filter's higher corner frequency (\code{f[2]}). 
#' Aggregation is recommended to improve computational efficiency, but is 
#' mandatory if data sets of different sampling intervals are to be analysed. 
#' In that case, it must be possible to aggregate the data sets to the 
#' provided aggregation sampling interval (\code{dt}), otherwise an error 
#' will arise. As an example, if the two data sets have sampling intervals of 
#' \code{1/200} and \code{1/500}, the highest possible aggregated sampling 
#' interval is \code{1/100}. See \code{aux_commondt()} for further 
#' information. 
#' 
#' The function supports parallel processing. However, keep in mind that 
#' calculating the cross correlation functions for large data sets and large 
#' windows will draw serious amounts of memory. For example, a 24 h window 
#' of two seismic signals recorded at 200 Hz will easily use 15 GB of RAM. 
#' Combining this with parallel processing will multiply that memory size. 
#' Therefore, it is better think before going for too high ambitions, and 
#' check how the computer system statistics evolve with increasing windows 
#' and parallel operations.
#' 
#' Deconvolution is recommended if different station hardware and setup is 
#' used for the stations to analyse (i.e., different sensors, loggers or 
#' gain factors). 
#' 
#' To account for biases due to brief events in the signals, the data sets 
#' can be truncated (cut) in their amplitude. This cutting can either be 
#' done based on the data sets' standard deviations (or a multiplicator of 
#' those standard deviations, see \code{signal_cut()} for further details), 
#' using the argument \code{sd = 1}, for one standard deviation. 
#' Alternatively, the data can also be cut by their sign (i.e., positive and 
#' negative values will be converted to \code{1} and \code{-1}, respectively).
#' 
#' @param start \code{POSIXct} value, start time for data set to be analysed.
#' If a character string (or vector) is provided, the function will try to 
#' convert it to POSIXct.
#' 
#' @param stop \code{POSIXct} value, stop time for calculation. 
#' If a character string (or vector) is provided, the function will try to 
#' convert it to POSIXct.
#' 
#' @param ID \code{Character} vector of length two, IDs of the station pair 
#' to be processed. This can either be two separate stations (assuming the  
#' components of those two stations are identical) or the same station 
#' (assuming the components of that one stations differ).
#' 
#' @param component \code{Character} vector of length two, seismic components 
#' to be used. If omitted, the function will use \code{c("Z", "Z")} by default.
#' 
#' @param dir \code{Character} value, path to directory that contains the 
#' seismic files. See \code{read_data} for details and constraints on the 
#' data structure.
#' 
#' @param window \code{Numeric} value, window size for the correlation  
#' calculation, in seconds.
#' 
#' @param overlap \code{Numeric} value, fraction of window overlap. Set to 
#' zero by default, i.e. no overlap.
#' 
#' @param window_sub \code{Numeric} value, size of the sub window used for 
#' averaging (stacking) the correlation function within a window. If omitted, 
#' no averaging will be performed. 
#' 
#' @param lag \code{Numeric} value, window size of the cross correlation 
#' function, in seconds.
#' 
#' @param dt \code{Numeric} mandatory value, sampling interval to which the input  
#' data will be aggregated. Note that the aggregation dt has to be a multiple 
#' of the data sets' dt values. If unsure, set value to original sampling interval 
#' of input data. See details for further information. 
#' 
#' @param deconvolve \code{Logical} value, option to deconvolve the input 
#' data sets. If set to \code{TRUE}, further parameters need to be provided, 
#' see \code{signal_deconvolve()} for details.
#' 
#' @param f \code{Numeric} value or vector of length two, lower and/or 
#' upper cutoff frequencies (Hz). If two values are provided, the function
#' assumes a bandpass filter. See \code{signal_filter} for details.
#' 
#' @param pick \code{Logical} value, option to remove signal snippets where
#' erratic seismic events can be picked in any of the signals. The supported 
#' method is \code{pick_stalta} and requires the provision of all mandatory 
#' arguments (\code{sta}, \code{lta}, \code{on}, \code{off}, \code{freeze}).
#' Attention, the sta and lta lengths must be provided in seconds, not as 
#' the number of samples!
#' 
#' @param whiten \code{Logical} value, option to perform spectral whitening.
#' Default is \code{FALSE} (no whitening). 
#' 
#' @param sd \code{Numeric} value, option to perform standard deviation based 
#' signal amplitude cutting. If omitted, no cutting will be performed. The 
#' value can be any natural number, describing a multiplier of the standard 
#' deviation magnitude of the data.
#' 
#' @param sign \code{Logical} value, option to perform sign based cutting of 
#' the signal amplitude. Default is \code{FALSE} (no cutting).
#' 
#' @param cpu \code{Numeric} value, fraction of CPUs to use. If omitted, 
#' only one CPU will be used.
#' 
#' @param buffer \code{Numeric} value, size of the two sided buffer that will 
#' be added to the windowed time series to account for preprocessing 
#' artifacts. The buffer is defined as the fraction of the window size. By 
#' default it is \code{0.05}.
#' 
#' @param eseis \code{Logical} value, option to return data as \code{eseis}
#' object, default is \code{TRUE}.
#' 
#' @param \dots Further arguments passed to the functions 
#' \code{signal_deconvolve}, \code{signal_filter}
#' 
#' @return \code{List} with spectrogram matrix, time and frequency vectors.
#'  
#' @author Michael Dietze
#' 
#' @keywords eseis
#' 
#' @examples
#' 
#' \dontrun{
#' 
#' ## calculate correlogram
#' cc <- ncc_correlate(start = "2017-04-09 00:30:00", 
#'                      stop = "2017-04-09 01:30:00", 
#'                      ID = c("RUEG1", "RUEG2"), 
#'                      dt = 1/10,
#'                      component = c("Z", "Z"), 
#'                      dir = paste0(system.file("extdata", 
#'                                               package = "eseis"), "/"), 
#'                      window = 600, 
#'                      overlap = 0, 
#'                      lag = 20, 
#'                      deconvolve = TRUE, 
#'                      sensor = "TC120s",
#'                      logger = "Cube3extBOB",
#'                      gain = 1,
#'                      f = c(0.05, 0.1), 
#'                      sd = 1)
#' 
#' ## plot output
#' plot_correlogram(cc)
#' 
#' }
#' 
#' @export ncc_correlate

ncc_correlate <- function(
    
  start,
  stop,
  ID,
  component,
  dir,
  window,
  overlap = 0,
  window_sub,
  lag,
  dt,
  deconvolve = FALSE,
  f,
  pick = FALSE,
  whiten = FALSE,
  sd,
  sign = FALSE,
  cpu,
  buffer = 0.05,
  eseis = TRUE,
  ...
  
) {
  
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
  
  ## check/set components
  if(missing(component) == TRUE) {
    
    component <- c("Z", "Z")
  }
  
  ## check/set sub window
  if(missing(window_sub) == TRUE) {
    
    window_sub <- window
  }
  
  ## check lag and sub window agreement
  if(lag > window_sub) {
    stop("Lag is larger than sub-window or window size!")
  }
  
  ## read additional plot arguments
  extraArgs <- list(...)
  
  ## check/set sensor option
  if ("sensor" %in% names(extraArgs)) {
    sensor <- extraArgs$sensor
  } else { sensor <- NA }
  
  ## check/set logger option
  if ("logger" %in% names(extraArgs)) {
    logger <- extraArgs$logger
  } else { logger <- NA }
  
  ## check/set gain option
  if ("gain" %in% names(extraArgs)) {
    gain <- extraArgs$gain
  } else { gain <- NA }
  
  ## check/set type option
  if ("type" %in% names(extraArgs)) {
    type <- extraArgs$type
  } else { type <- "BP" }
  
  ## check/set method option
  if ("method" %in% names(extraArgs)) {
    method <- extraArgs$method
  } else { method <- NA }
  
  ## check/set sta option
  if ("sta" %in% names(extraArgs)) {
    sta <- extraArgs$sta
  } else { sta <- NA }
  
  ## check/set lta option
  if ("lta" %in% names(extraArgs)) {
    lta <- extraArgs$lta
  } else { lta <- NA }
  
  ## check/set on option
  if ("on" %in% names(extraArgs)) {
    on <- extraArgs$on
  } else { on <- NA }
  
  ## check/set off option
  if ("off" %in% names(extraArgs)) {
    off <- extraArgs$off
  } else { off <- NA }
  
  ## check/set freeze option
  if ("freeze" %in% names(extraArgs)) {
    freeze <- extraArgs$freeze
  } else { freeze <- FALSE }
  
  ## define time vector
  t <- seq(from = start, 
           to = stop - window, 
           by = round((1 - overlap) * window))
  
  ## check/set dt option
  if(missing(dt)) {
    stop("Not dt provided!")
  }
  
  ## check/set sd option
  if(missing(sd)) {
    sd <- 0
  }
  
  ## initiate cluster
  cores <- parallel::detectCores()
  
  if(missing(cpu) == FALSE) {
    
    n_cpu <- floor(cores * cpu)
    cores <- ifelse(cores < n_cpu, cores, n_cpu)
  } else {
    
    cores <- 1
  }
  
  ## make parameter list for parallel computation
  par <- list(ID = ID, 
              component = component,
              window = window,
              overlap = overlap,
              window_sub = window_sub,
              buffer = buffer,
              dir = dir,
              dt = dt,
              deconvolve = deconvolve,
              sensor = sensor,
              logger = logger,
              gain = gain,
              f = f,
              pick = pick, 
              sta = sta,
              lta = lta,
              on = on,
              off = off, 
              freeze = freeze,
              type = type,
              method = method,
              whiten = whiten,
              sd = sd,
              sign = sign,
              lag = lag)
  
  ## get start time
  eseis_t_0 <- Sys.time()
  
  cl <- parallel::makeCluster(getOption("mc.cores", cores))
  
  ## calculate cross correlation function
  CC <- parallel::parLapply(cl = cl, X = t, fun = function(t, par) {
    
    s_1 <- try(eseis::read_data(start = t - round(par$window * par$buffer), 
                                duration = par$window + 
                                  2 * round(par$window * par$buffer), 
                                station = par$ID[1], 
                                component = par$component[1], 
                                dir = par$dir))
    
    s_2 <- try(eseis::read_data(start = t - round(par$window * par$buffer), 
                                duration = par$window + 
                                  2 * round(par$window * par$buffer), 
                                station = par$ID[2], 
                                component = par$component[2], 
                                dir = par$dir))
    
    if(inherits(s_1, "eseis") & inherits(s_2, "eseis")) {
      
      ## optionally fill NA gaps
      try(if(any(is.na(s_1$signal))) {
        s_1 <- try(signal_fill(data = s_1))
      })
      
      try(if(any(is.na(s_2$signal))) {
        s_2 <- try(signal_fill(data = s_2))
      })
      
      ## optionally aggregate data sets
      try(if(is.null(par$dt) == FALSE) {
        
        k <- eseis::aux_commondt(data = c(s_1$meta$dt, 
                                          s_2$meta$dt), 
                                 dt = par$dt)$agg
        
        s_1 <- eseis::signal_aggregate(data = s_1, n = k[1])
        s_2 <- eseis::signal_aggregate(data = s_2, n = k[2])
      })
      
      ## optionally deconvolve data sets
      try(if(par$deconvolve == TRUE) {
        
        s_1 <- eseis::signal_deconvolve(data = s_1,
                                        sensor = par$sensor, 
                                        logger = par$logger, 
                                        gain = par$gain)
        s_2 <- eseis::signal_deconvolve(data = s_2,
                                        sensor = par$sensor, 
                                        logger = par$logger, 
                                        gain = par$gain)
      })
      
      ## demean and detrend data sets
      s_1 <- try(eseis::signal_demean(data = s_1))
      s_2 <- try(eseis::signal_demean(data = s_2))
      
      s_1 <- try(eseis::signal_detrend(data = s_1))
      s_2 <- try(eseis::signal_detrend(data = s_2))
      
      ## taper data sets
      s_1 <- try(eseis::signal_taper(data = s_1, p = par$buffer))
      s_2 <- try(eseis::signal_taper(data = s_2, p = par$buffer))
      
      ## filter data sets
      s_1 <- try(eseis::signal_filter(data = s_1, f = par$f, 
                                      type = par$type, p = par$buffer))
      s_2 <- try(eseis::signal_filter(data = s_2, f = par$f, 
                                      type = par$type, p = par$buffer))
      
      ## create index vector for potentially event contaminated samples
      s_ok <- try(rep(TRUE, s_1$meta$n))
      
      ## optionally identify events
      try(if(par$pick == TRUE) {
        
        ## pick events in signals
        e_1 <- eseis::pick_stalta(data = eseis::signal_envelope(data = s_1), 
                                  sta = par$sta * 1/s_1$meta$dt, 
                                  lta = par$lta * 1/s_1$meta$dt, 
                                  on = par$on, 
                                  off = par$off, 
                                  freeze = par$freeze)$picks
        
        e_2 <- eseis::pick_stalta(data = eseis::signal_envelope(data = s_2), 
                                  sta = par$sta * 1/s_2$meta$dt, 
                                  lta = par$lta * 1/s_2$meta$dt, 
                                  on = par$on, 
                                  off = par$off, 
                                  freeze = par$freeze)$picks
        
        ## combine picks from both signals
        e <- rbind(e_1, e_2)
        
        ## create time vector
        t_ok <- seq(from = s_1$meta$starttime, 
                    by = s_1$meta$dt, 
                    length.out = s_1$meta$n)
        
        ## indicate event-contaminated samples
        if(nrow(e) > 0) {
          
          for(i in 1:nrow(e)) {
            
            s_ok[t_ok >= e$start[i] & 
                   t_ok <= e$start[i] + e$duration[i]] <- FALSE
          }
        }
        
        ## remove temporary objects and collect garbage
        rm(e, e_1, e_2, t_ok)
        gc()
      })
      
      ## optionally whiten data
      if(par$whiten == TRUE) {
        
        s_1 <- try(eseis::signal_whiten(data = s_1))
        s_2 <- try(eseis::signal_whiten(data = s_2))
      }
      
      ## optionally sd-cut data
      if(par$sd != 0) {
        
        s_1 <- try(eseis::signal_cut(data = s_1, k = par$sd))
        s_2 <- try(eseis::signal_cut(data = s_2, k = par$sd))
      }
      
      ## optionally sign-cut data
      if(par$sign == TRUE) {
        
        s_1 <- try(eseis::signal_sign(data = s_1))
        s_2 <- try(eseis::signal_sign(data = s_2))
      }
      
      ## set event contaminated samples to NA
      try(s_1$signal[s_ok == FALSE] <- NA)
      try(s_2$signal[s_ok == FALSE] <- NA)
      
      ## create sub window vector
      t_sub <- try(seq(from = t, 
                       to = t + par$window - par$window_sub,
                       by = par$window_sub))
      
      ## calculate correlation function for (optional) subwindows
      corr_sub <- lapply(X = t_sub, FUN = function(t_sub, par, s_1, s_2) {
          
          ## clip signals to sub window
          s_1_sub <- try(eseis::signal_clip(data = s_1, 
                                        limits = c(t_sub, 
                                                   t_sub + par$window_sub)))
          s_2_sub <- try(eseis::signal_clip(data = s_2, 
                                        limits = c(t_sub, 
                                                   t_sub + par$window_sub)))
          
          ## demean and detrend data sets
          s_1_sub <- try(eseis::signal_demean(data = s_1_sub))
          s_2_sub <- try(eseis::signal_demean(data = s_2_sub))
          
          s_1_sub <- try(eseis::signal_detrend(data = s_1_sub))
          s_2_sub <- try(eseis::signal_detrend(data = s_2_sub))
          
          ## apply FFT
          s_1_fft_sub <- try(fftw::FFT(x = s_1_sub$signal))
          s_2_fft_sub <- try(fftw::FFT(x = s_2_sub$signal))
          
          ## calculate cross-correlation function
          n <- try(s_1_sub$meta$n)
          corr_sub <- try(Conj(s_1_fft_sub) * s_2_fft_sub)
          corr_sub <- try(Re(fftw::IFFT(x = corr_sub)) / n)
          corr_sub <- try(c(corr_sub[1:n], corr_sub[1:(n - 1)]))
          
          ## calculate normalisation factor
          f_norm <- try(prod(c(Re(sqrt(mean(fftw::IFFT(s_1_fft_sub)^2))), 
                           Re(sqrt(mean(fftw::IFFT(s_2_fft_sub)^2))))))
          
          ## normalise correlation function
          corr_sub <- try(corr_sub / f_norm)
          
          ## calculate number of samples in lag time
          n_lag <- try(par$lag * 1 / s_1_sub$meta$dt)
          
          ## truncate correlation function to time lag window
          if(inherits(corr_sub, "try-error") == FALSE) {
            
            if(any(is.na(corr_sub)) == TRUE | n_lag > length(corr_sub)) {
              
              class(corr_sub) <- "try-error"
            } else {
              
              corr_sub <- try(corr_sub[(n - n_lag + 1):(n + 1 + n_lag)])
            }
          }
          
          ## prepare NA case
          if(inherits(corr_sub, "try-error") == TRUE) {
            
            corr_sub <- rep(NA, 2 * (par$lag / par$dt) + 1)
          }
          
          ## remove temporary objects and collect garbage
          try(rm(s_1_sub, s_2_sub, s_1_fft_sub, s_2_fft_sub))
          try(gc())
          
          ## return result
          return(corr_sub)
          
        }, par, s_1, s_2)
      
      ## convert list to data frame and calculate average
      corr_sub <- try(do.call(rbind, corr_sub))
      corr_sub <- try(colMeans(corr_sub, na.rm = TRUE))
      
      if(inherits(corr_sub, "try-error")) {
        corr_sub <- rep(NA, 2 * par$lag / par$dt + 1)
      }
      
    } else {
      
      corr_sub <- rep(NA, 2 * par$lag / par$dt + 1)
    }
    
    ## remove large temporary objects and collect garbage
    s_ok <- NULL
    rm(s_1, s_2, s_ok)
    gc()
    
    return(corr_sub)
    
  }, par = par)
  
  ## stop cluster
  try(parallel::stopCluster(cl = cl))
  
  ## convert list to matrix
  CC <- do.call(cbind, CC)
  
  ## optionally rebuild eseis object
  if(eseis == TRUE) {
    
    ## assign cross correlation data set
    eseis_data <- list(CC = list(CC = CC,
                                 t = t,
                                 lag = seq(from = -lag, to = lag, 
                                           length.out = nrow(CC))),
                       history = eseis::aux_initiateeseis()$history)
    
    ## calculate function call duration
    eseis_duration <- as.numeric(difftime(time1 = Sys.time(), 
                                          time2 = eseis_t_0, 
                                          units = "secs"))
    
    ## update object history
    eseis_data$history[[length(eseis_data$history) + 1]] <- 
      list(time = Sys.time(),
           call = "ncc_correlate()",
           arguments = par,
           duration = eseis_duration)
    names(eseis_data$history)[length(eseis_data$history)] <- 
      as.character(length(eseis_data$history))
    
    ## update data type
    eseis_data$meta$type = "correlogram"
    
    ## set S3 class name
    class(eseis_data)[1] <- "eseis"
    
    ## assign eseis object to output data set
    CC <- eseis_data
  }
  
  ## return output
  return(CC)
}