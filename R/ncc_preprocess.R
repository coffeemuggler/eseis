#' Noise Cross Correlation preprocessing routine
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
#' Aggregation is recommended to improve computational efficiency, but is 
#' mandatory if data sets of different sampling intervals are to be analysed. 
#' In that case, it must be possible to aggregate the data sets to the 
#' provided aggregation sampling interval (\code{dt}), otherwise an error 
#' will arise. As an example, if the two data sets have sampling intervals of 
#' \code{1/200} and \code{1/500}, the highest possible aggregated sampling 
#' interval is \code{1/100}. See \code{aux_commondt()} for further 
#' information. 
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
#' @param window \code{Numeric} value, time window size for the correlation  
#' calculation, in seconds.
#' 
#' @param overlap \code{Numeric} value, fraction of window overlap. Set to 
#' zero by default, i.e. no overlap.
#' 
#' @param lag \code{Numeric} value, window size of the cross correlation 
#' function, in seconds.
#' 
#' @param dt \code{Numeric} value, sampling interval to which the input data 
#' sets will be aggregated. Note that the aggregation dt has to be a multipe 
#' of the data sets' dt values. See details for further information. If 
#' omitted, no aggregation will be performed.
#' 
#' @param deconvolve \code{Logical} value, option to deconvolve the input 
#' data sets. If set to \code{TRUE}, further parameters need to be provided, 
#' see \code{signal_deconvolve()} for details.
#' 
#' @param f \code{Numeric} value or vector of length two, lower and/or 
#' upper cutoff frequencies (Hz). If two values are provided, the function
#' assumes a bandpass filter. See \code{signal_filter} for details.
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
#' ## preprocess cross correlation
#' cc <- ncc_preprocess(start = "2017-04-09 00:30:00", 
#'                      stop = "2017-04-09 01:30:00", 
#'                      ID = c("RUEG1", "RUEG2"), 
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
#' @export ncc_preprocess

ncc_preprocess <- function(

  start,
  stop,
  ID,
  component,
  dir,
  window,
  overlap = 0,
  lag,
  dt,
  deconvolve = FALSE,
  f,
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
  } else { type <- NA }
  
  ## check/set method option
  if ("method" %in% names(extraArgs)) {
    method <- extraArgs$method
  } else { method <- NA }
  
  ## define time vector
  t <- seq(from = start, 
           to = stop - window, 
           by = round((1 - overlap) * window))
  
  ## check/set dt option
  if(missing(dt)) {
    dt <- NULL
  }
  
  ## check/set sd option
  if(missing(sd)) {
    sd <- NULL
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
              buffer = buffer,
              dir = dir,
              dt = dt,
              deconvolve = deconvolve,
              sensor = sensor,
              logger = logger,
              gain = gain,
              f = f,
              type = type,
              method = method,
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
      
      ## optionally aggregate data sets
      if(is.null(par$dt) == FALSE) {
        
        k <- eseis::aux_commondt(data = c(s_1$meta$dt, 
                                   s_2$meta$dt), 
                          dt = par$dt)$agg
        
        s_1 <- eseis::signal_aggregate(data = s_1, n = k[1])
        s_2 <- eseis::signal_aggregate(data = s_2, n = k[2])
      }
      
      ## optionally deconvolve data sets
      if(par$deconvolve == TRUE) {
        
        s_1 <- eseis::signal_deconvolve(data = s_1,
                                 sensor = par$sensor, 
                                 logger = par$logger, 
                                 gain = par$gain)
        s_2 <- eseis::signal_deconvolve(data = s_2,
                                 sensor = par$sensor, 
                                 logger = par$logger, 
                                 gain = par$gain)
      }
      
      ## filter data sets
      s_1 <- eseis::signal_filter(data = s_1, f = par$f, p = par$buffer)
      s_2 <- eseis::signal_filter(data = s_2, f = par$f, p = par$buffer)
      
      ## optionally sd-cut data
      if(is.null(par$sd) == FALSE) {
        
        s_1 <- eseis::signal_cut(data = s_1, k = par$sd)
        s_2 <- eseis::signal_cut(data = s_2, k = par$sd)
      }
      
      ## optionally sign-cut data
      if(par$sign == TRUE) {
        
        s_1 <- eseis::signal_sign(data = s_1)
        s_2 <- eseis::signal_sign(data = s_2)
      }
      
      ## apply FFT
      s_1_fft <- fftw::FFT(x = s_1$signal)
      s_2_fft <- fftw::FFT(x = s_2$signal)
      
      ## calculate cross-correlation function
      n <- s_1$meta$n
      corr <- try(Conj(s_1_fft) * s_2_fft)
      corr <- try(Re(fftw::IFFT(x = corr)) / n)
      corr <- try(c(corr[1:n], corr[1:(n - 1)]))
      
      ## calculate normalisation factor
      f_norm <- prod(c(Re(sqrt(mean(fftw::IFFT(s_1_fft)^2))), 
                       Re(sqrt(mean(fftw::IFFT(s_2_fft)^2)))))
      
      ## normalise correlation function
      corr <- corr / f_norm
      
      ## calculate number of samples in lag time
      n_lag <- par$lag * 1 / s_1$meta$dt

      ## truncate correlation function to time lag window      
      corr <- corr[(n - n_lag + 1):(n + 1 + n_lag)]
      
    } else {
      
      corr <- NA
    }
    
    return(corr)
    
  }, par = par)
  
  ## stop cluster
  try(parallel::stopCluster(cl = cl))
  
  ## extract number of values in correlation function
  n_cc <- max(sapply(X = CC, FUN = length), na.rm = TRUE)
  
  ## expand NA value cases
  CC <- lapply(X = CC, FUN = function(CC, n_cc) {
    
    n_na <- sum(is.na(CC))
    
    if(n_na > 0) {
      
      return(rep(NA, n_cc))
    } else {
      
      return(CC)
    }
    
  }, n_cc = n_cc)
  
  ## convert list to matrix
  CC <- do.call(cbind, CC)

  ## optionally rebuild eseis object
  if(eseis == TRUE) {
    
    ## assign cross correlation data set
    eseis_data <- list(CC = list(CC = CC,
                                 t = t,
                                 lag = seq(from = -lag, to = lag, 
                                           length.out = n_cc)),
                       history = eseis::aux_initiateeseis()$history)
    
    ## calculate function call duration
    eseis_duration <- as.numeric(difftime(time1 = Sys.time(), 
                                          time2 = eseis_t_0, 
                                          units = "secs"))
    
    ## update object history
    eseis_data$history[[length(eseis_data$history) + 1]] <- 
      list(time = Sys.time(),
           call = "ncc_preprocess()",
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