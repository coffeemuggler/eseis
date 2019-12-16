#' Calculate spectrograms (power spectral density estimates) from time series.
#' 
#' This function creates spectrograms from seismic signals. It supports the
#' standard spectrogram approach, multitaper, and the Welch method.
#' 
#' Data containing \code{NA} values is replaced by zeros and set to NA in the 
#' output data set.
#' 
#' @param data \code{Numeric} vector or list of vectors, seismic signal to 
#' be processed.
#' 
#' @param time \code{POSIX.ct} vector with time values. If omitted, an 
#' artificial time vector will be created, based on \code{dt}. Only needed 
#' if \code{data} is no \code{eseis} object.
#' 
#' @param dt \code{Numeric} value, sampling period. If omitted, either 
#' estimated from \code{time} or set to 0.01 s (i.e., f = 100 Hz). Only needed 
#' if \code{data} is no \code{eseis} object.
#' 
#' @param Welch \code{Logical} value, option to use the Welch method for
#' calculations.
#' 
#' @param window \code{Numeric} value, time window length in seconds used to 
#' calculate individual spectra. Set to 1 percent of the time series length 
#' by default.
#' 
#' @param overlap \code{Numeric} value, fraction of window overlap.
#' 
#' @param window_sub \code{Numeric} value, length of the sub-window in seconds 
#' used to calculate spectra. Only relevant if \code{Welch = TRUE}. If omitted, 
#' the sub-window length is set to 10 percent of the main window length.
#' 
#' @param overlap_sub \code{Numeric} value, fraction of sub-window overlap.
#' 
#' @param method  \code{Character} value, method to calculate the spectra. 
#' One out of \code{"periodogram"}, \code{"autoregressive"} and 
#' \code{"multitaper"}. Default is \code{"periodogram"}.
#' 
#' @param nw \code{Numeric} value, multitaper time-bandwidth parameter,
#' default is 4.0.
#' 
#' @param k \code{Numeric} value, multitaper number of tapers, default is 7.
#' 
#' @param n_cores \code{Numeric} value, number of CPU cores to use. Disabled 
#' by setting to 1. Default is 1.
#' 
#' @param plot \code{Logical} value, toggle plot output. Default is
#' \code{FALSE}. For more customised plotting see \code{plot_spectrogram}.
#' 
#' @param \dots Additional arguments passed to the function.
#' 
#' @return \code{List} with spectrogram matrix, time and frequency vectors.
#' 
#' @author Michael Dietze
#' 
#' @seealso \code{\link{spectrum}}, \code{\link{spec.pgram}}, 
#' \code{\link{spec.mtm}}
#' 
#' @keywords eseis
#' 
#' @examples
#' 
#' ## load example data set
#' data("earthquake")
#' 
#' ## calculate and plot PSD straight away
#' P <- signal_spectrogram(data = s$BHZ, 
#'                                time = t, 
#'                                dt = 1 / 200, 
#'                                plot = TRUE)
#' 
#' ## calculate and plot PSD with defined window sizes and the Welch method
#' P <- signal_spectrogram(data = s$BHZ, 
#'                                time = t, 
#'                                dt = 1 / 200, 
#'                                window = 5, 
#'                                overlap = 0.9, 
#'                                window_sub = 3, 
#'                                overlap_sub = 0.9, 
#'                                Welch = TRUE,
#'                                plot = TRUE)
#'                                
#' ## calculate and plot PSD with even smaller window sizes, the Welch
#' ## method and using multitapers, uncomment to use.
#' # P <- signal_spectrogram(data = s$BHZ, 
#' #                                time = t, 
#' #                                dt = 1 / 200, 
#' #                                window = 2, 
#' #                                overlap = 0.9, 
#' #                                window_sub = 1, 
#' #                                overlap_sub = 0.9, 
#' #                                Welch = TRUE,
#' #                                method = "multitaper",
#' #                                plot = TRUE)
#'                       
#' @export signal_spectrogram
signal_spectrogram <- function(
  data,
  time,
  dt,
  Welch = FALSE,
  window,
  overlap = 0.5,
  window_sub,
  overlap_sub = 0.5,
  method = "periodogram",
  nw = 4.0,
  k = 7,
  n_cores = 1,
  plot = FALSE,
  ...
) {
  
  ## check data structure
  if(class(data)[1] == "list") {
    
    ## apply function to list
    data_out <- lapply(X = data, 
                       FUN = eseis::signal_spectrogram, 
                       time = time,
                       dt = dt,
                       Welch = Welch,
                       window = window,
                       overlap = overlap,
                       window_sub = window_sub,
                       overlap_sub = overlap_sub,
                       method = method,
                       nw = nw,
                       k = k,
                       n_cores = n_cores,
                       plot = plot)
    
    ## return output
    return(data_out)
    
  } else {
    
    ## check/store presence of time vector
    if(missing(time) == TRUE) {
      
      time_in <- NULL
    } else {
      
      time_in <- time
    }
    
    ## check/set dt
    if(missing(dt) == TRUE && class(data)[1] != "eseis") {
      
      if(missing(time) == TRUE) {
        
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
      
    } else if(class(data)[1] == "eseis") {
      
      dt <- data$meta$dt
    }
    
    ## handle missing time vector
    if(missing(time) == TRUE && class(data)[1] != "eseis") {
      
      time <- seq(from = as.POSIXct(x = strptime(x = "0000-01-01 00:00:00",
                                                 format = "%Y-%m-%d %H:%M:%S", 
                                                 tz = "UTC"), tz = "UTC"),
                  by = dt,
                  length.out = length(data))
      
      print("No or non-POSIXct time data provided. Default data generated!")
      
    } else if(class(data)[1] == "eseis") {
      
      time <- seq(from = data$meta$starttime, 
                  by = data$meta$dt, 
                  length.out = data$meta$n)
    }
    
    ## handle missing window length
    if(missing(window) == TRUE) {
      
      window <- 0.01 * length(time) * dt
    }
    
    ## handle missing sub-window length
    if(Welch == TRUE && missing(window_sub) == TRUE) {
      
      window_sub <- 0.1 * window
      
    } else if(Welch == FALSE) {
      
      window_sub <- NULL
    }
    
    ## identify NA values
    i_na <- is.na(data)
    
    data[i_na] <- 0
    
    ## get start time
    eseis_t_0 <- Sys.time()
    
    ## collect function arguments
    eseis_arguments <- list(data = "",
                            time = time_in,
                            dt = dt,
                            Welch = Welch,
                            window = window,
                            overlap = overlap,
                            window_sub = window_sub,
                            overlap_sub = overlap_sub,
                            method = method,
                            nw = nw,
                            k = k,
                            n_cores = n_cores,
                            plot = plot)
    
    ## homogenise data structure
    if(class(data)[1] == "eseis") {
      
      ## set eseis flag
      eseis_class <- TRUE
      
      ## store initial object
      eseis_data <- data
      
      ## extract signal vector
      data <- eseis_data$signal
 
    } else {
      
      ## set eseis flag
      eseis_class <- FALSE
    }
    
    ## change method keywords
    if(method == "periodogram") {
      
      method <- "pgram"
    }
    
    if(method == "autoregressive") {
      
      method <- "ar"
    }
    
    ## CALCULATION PART -------------------------------------------------------
    
    ## get window length
    length_window <- round(x = window * (1 / dt), 
                           digits = 0)
    
    ## get number of windows
    n_window <- floor(length(data) / length_window)
    
    ## truncate time series to fit window sizes
    data_trunc <- data[1:(length_window * n_window)]
    time_trunc <- time[1:(length_window * n_window)]
    
    ## get overlap length
    length_overlap <- round(x = length_window * overlap, 
                            digits = 0)
    
    ## calculate indices for slicing the time series
    l_step <- length_window - length_overlap
    slice_start <- seq(from = 1, to = length(data) - length_window, 
                       by = l_step)
    
    ## convert time series to matrix with slices
    data_list <- vector(mode = "list", 
                        length = length(slice_start))
    
    for (i in 1:length(data_list)) {
      
      data_list[[i]] <- data[slice_start[i]:(slice_start[i] + 
                                               length_window - 1)]
    }
    
    ## define generic function to get spectra
    spec_generic <- function(x, method) {
      
      try(spectrum(x = x, 
                   method = method,
                   plot = FALSE)$spec,
          silent = TRUE)
      
    }
    
    ## define multitaper function to get spectrae
    spec_multitaper <- function(x, dt, nw, k) {
      
      try(multitaper::spec.mtm(timeSeries = x, 
                               deltat = dt,
                               nw = nw,
                               k = k,
                               plot = FALSE)$spec,
          silent = TRUE)
    }
    
    ## define generic function for Welch method to get spectrae
    spec_welch <- function(x, window_sub, overlap_sub, dt, method, nw, k) {
      
      ## get sub-window length
      length_window_sub <- round(x = window_sub * (1 / dt), 
                                 digits = 0)
      
      ## get number of sub-windows
      n_window_sub <- floor(length(x) / length_window_sub)
      
      ## truncate time series to fit window sizes
      data_trunc <- data[1:(length_window_sub * n_window_sub)]
      
      ## get overlap length
      length_overlap_sub <- round(x = length_window_sub * overlap_sub, 
                                  digits = 0)
      
      ## calculate indices for slicing the time series
      l_step <- length_window_sub - length_overlap_sub
      slice_start <- seq(from = 1, 
                         to = length(x) - length_window_sub, 
                         by = l_step)
      
      ## convert time series to matrix with slices
      data_list_sub <- vector(mode = "list", 
                              length = length(slice_start))
      
      for (i in 1:length(data_list_sub)) {
        
        data_list_sub[[i]] <- x[slice_start[i]:(slice_start[i] + 
                                                  length_window_sub - 1)]
      }
      
      ## calculate spectrae
      if(method == "multitaper") {
        
        S_welch <- lapply(X = data_list_sub, 
                          FUN = spec_multitaper,
                          dt = dt,
                          nw = nw,
                          k = k)
      } else {
        
        S_welch <- lapply(X = data_list_sub, 
                          FUN = spec_generic,
                          method = method)
      }
      
      ## convert list to matrix
      S_welch <- do.call(cbind, S_welch)
      
      ## calculate mean
      s_mean <- rowMeans(S_welch, na.rm = TRUE)
      
      ## return output
      return(s_mean)
      
    }
    
    ## optinally initiate multicore environment
    if(n_cores > 1) {
      
      ## detect number of CPU cores
      n_cores_system <- parallel::detectCores()
      n_cores <- ifelse(test = n_cores > n_cores_system, 
                        yes = n_cores_system,
                        no = n_cores)
      
      ## initiate cluster
      cl <- parallel::makeCluster(n_cores)
    }
    
    ## calculate spectra for each slice
    if(Welch == FALSE) {
      
      if(method != "multitaper") {
        
        if(n_cores > 1) {
          
          ## non-Welch option in parallel mode
          S <- parallel::parLapply(cl = cl,
                                   X = data_list, 
                                   fun = spec_generic,
                                   method = method)
        } else {
          
          ## non-Welch option in one-core mode
          S <- lapply(X = data_list, 
                      FUN = spec_generic,
                      method = method)
        }
      } else {
        
        if(n_cores > 1) {
          
          ## non-Welch option in parallel mode, multitaper option
          S <- parallel::parLapply(cl = cl,
                                   X = data_list, 
                                   fun = spec_multitaper,
                                   dt = dt,
                                   nw = nw,
                                   k = k)
        } else {
          
          ## non-Welch option in one-core mode, multitaper option
          S <- lapply(X = data_list, 
                      FUN = spec_multitaper,
                      dt = dt,
                      nw = nw,
                      k = k)
        }
        
      }
      
    } else {
      
      if(n_cores > 1) {
        
        ## Welch option in parallel processing mode
        S <- parallel::parLapply(cl = cl,
                                 X = data_list, 
                                 fun = spec_welch,
                                 window_sub = window_sub, 
                                 overlap_sub = overlap_sub, 
                                 dt = dt,
                                 nw = nw, 
                                 k = k,
                                 method = method)
      } else {
        
        ## Welch option in one-core mode
        S <- lapply(X = data_list, 
                    FUN = spec_welch,
                    window_sub = window_sub, 
                    overlap_sub = overlap_sub, 
                    dt = dt,
                    nw = nw,
                    k = k,
                    method = method)
      }
      
      
    }
    
    ## stop cluster
    if(n_cores > 1) {
      
      parallel::stopCluster(cl = cl)
    }
    
    
    ## convert spectra list to matrix
    S <- do.call(cbind, S)
    
    ## convert data to db
    S <- 10 * log10(S)
    
    ## create frequency and time vector of spectrogram
    f_spectrum <- seq(from = 0, 
                      to = 0.5 / dt, 
                      length.out = nrow(S))
    
    t_spectrum <- time[slice_start]
    
    ## assign row- and col-names
    rownames(S) <- f_spectrum
    colnames(S) <- t_spectrum
    
    ## reset NA-affected parts to NA
    if(sum(i_na) > 0) {
      
      i_na_diff <- diff(i_na)
      t_na_start <- time[i_na_diff == 1] - window
      t_na_stop <- time[i_na_diff == -1]
      
      t_na <- data.frame(t_na_start = t_na_start, 
                         t_na_stop = t_na_stop)
      
      for(i in 1:length(t_na)) {
        
        S[,t_spectrum >= t_na[i,1] & t_spectrum <= t_na[i,2]] <- NA
      }
    }
    
    ## assign output
    S <- list(S = S,
              t = t_spectrum,
              f = f_spectrum)
    class(S)[1] <- "spectrogram"
    
    ## optionally plot spectrogram
    if(plot == TRUE) {
      
      plot_spectrogram(data = S, ...)
      
    }
    
    ## optionally rebuild eseis object
    if(eseis_class == TRUE) {
      
      ## assign aggregated signal vector
      eseis_data <- list(PSD = S,
                         history = eseis_data$history)
      
      ## calculate function call duration
      eseis_duration <- as.numeric(difftime(time1 = Sys.time(), 
                                            time2 = eseis_t_0, 
                                            units = "secs"))
      
      ## update object history
      eseis_data$history[[length(eseis_data$history) + 1]] <- 
        list(time = Sys.time(),
             call = "signal_spectrogram()",
             arguments = eseis_arguments,
             duration = eseis_duration)
      names(eseis_data$history)[length(eseis_data$history)] <- 
        as.character(length(eseis_data$history))
      
      ## update data type
      eseis_data$meta$type = "spectrogram"
      
      ## set S3 class name
      class(eseis_data)[1] <- "eseis"
      
      ## assign eseis object to output data set
      S <- eseis_data
    }
    
    ## return output
    return(S) 
  }
}
