#' Perform H-V-spectral ratio analysis of a seismic data set
#' 
#' This function cuts a three component seismic data set into time windows
#' that may or may not overlap and calculates the spectral ratio for each of 
#' these windows. It returns a matrix with the ratios for each time slice. 
#' Thus, it is a wrapper for the function \code{signal_hvratio}. For 
#' further information about the technique and function arguments see the 
#' description of \code{signal_hvratio}.
#' 
#' @param data \code{List}, \code{data frame} or \code{matrix}, seismic
#' componenents to be processed. If \code{data} is a matrix, the components 
#' must be organised as columns. Also, \code{data} can be a list of 
#' \code{eseis} objects.
#' 
#' @param time \code{POSIXct} vector with time values. If omitted, an 
#' synthetic time vector will be created, based on \code{dt}.
#' 
#' @param window \code{Numeric} scalar, time window length in seconds used to 
#' calculate individual spectral ratios. Set to 10 percent of the time  
#' series length by default.
#' 
#' @param overlap \code{Numeric} value, fraction of window overlap.
#' 
#' @param dt \code{Numeric} value, sampling period.
#' 
#' @param method \code{Character} value, method for calculating the spectra. 
#' One out of \code{"periodogram"} , \code{"autoregressive"} and 
#' \code{"multitaper"}, default is \code{"periodogram"}.
#' 
#' @param kernel \code{Numeric} value, window size (defined by number of 
#' samples) of the moving window used for smoothing the spectra. By default 
#' no smoothing is performed.
#' 
#' @param order \code{Character} value, order of the seismic components. 
#' Describtion must contain the letters \code{"x"},\code{"y"} and
#' \code{"z"} in the order according to the input data set. Default is 
#' \code{"xyz"} (NW-SE-vertical).
#' 
#' @param plot \code{Logical} value, toggle plot output. Default is
#' \code{FALSE}.
#' 
#' @param \dots Additional arguments passed to the plot function.
#' 
#' @return A \code{matrix} with the h-v-frequency ratios for each time slice.
#' 
#' @author Michael Dietze
#' 
#' @keywords eseis
#' 
#' @examples 
#' ## load example data set
#' data(earthquake)
#' 
#' ## ATTENTION, THIS EXAMPLE DATA SET IS FAR FROM IDEAL FOR THIS PURPOSE
#' 
#' ## detrend data
#' s <- signal_detrend(data = s)
#' 
#' ## calculate the HV ratios straightforward
#' HV <- aux_hvanalysis(data = s,
#'                      dt = 1 / 200,
#'                      kernel = 100)
#' 
#' ## calculate the HV ratios with plot output, userdefined palette
#' plot_col <- colorRampPalette(colors = c("grey", "darkblue", "blue", "orange"))
#' HV <- aux_hvanalysis(data = s,
#'                      dt = 1 / 200,
#'                      kernel = 100,
#'                      plot = TRUE,
#'                      col = plot_col(100))
#' 
#' ## calculate the HV ratios with optimised method settings
#' HV <- aux_hvanalysis(data = s, 
#'                      time = t,
#'                      dt = 1 / 200, 
#'                      window = 10, 
#'                      overlap = 0.9, 
#'                      method = "autoregressive",
#'                      plot = TRUE,
#'                      col = plot_col(100),
#'                      xlab = "Time (UTC)",
#'                      ylab = "f (Hz)")
#'                      
#' ## calculate and plot stack (mean and sd) of all spectral ratios
#' HV_mean <- apply(X = HV, MARGIN = 1, FUN = mean)
#' HV_sd <- apply(X = HV, MARGIN = 1, FUN = sd)
#' HV_f <- as.numeric(rownames(HV))
#' 
#' plot(x = HV_f, y = HV_mean, type = "l", ylim = c(0, 50))
#' lines(x = HV_f, y = HV_mean + HV_sd, col = 2)
#' lines(x = HV_f, y = HV_mean - HV_sd, col = 2)              
#'
#' @export aux_hvanalysis
#'  
aux_hvanalysis <- function(
  
  data,
  time,
  window,
  overlap = 0,
  dt,
  method = "periodogram",
  kernel,
  order = "xyz",
  plot = FALSE,
  ...
) {
  
  ## get start time
  eseis_t_0 <- Sys.time()
  
  ## check/set time
  if(class(data[[1]])[1] != "eseis") {
    
    ## set eseis flag
    eseis_class <- FALSE
    
    ## check/set sampling period
    if(missing(dt) == TRUE) {
      
      warning("No dt provided! Set to 1/ 200 by default.")
      
      dt <- 1 / 200
    }
    
    ## check/set time vector
    if(missing(time) == TRUE) {
      
      time <- seq(from = 0, 
                  to = length(data[[1]])) * dt
    }
    
    ## homogenise data structure
    if(class(data)[1] != "list") {
      
      data <- as.list(as.data.frame(data))
    }
    
  } else {
    
    ## set eseis flag
    eseis_class <- TRUE
    
    ## store initial object
    eseis_data <- data
    
    ## extract signal vector
    data <- lapply(X = data, FUN = function(X) {
      
      X$signal
    })
    
    ## extract sampling period
    dt <- eseis_data[[1]]$meta$dt
    
    ## create time vector
    time <- seq(from = eseis_data[[1]]$meta$starttime,
                by = eseis_data[[1]]$meta$dt,
                length.out = eseis_data[[1]]$meta$n)

  }
  
  ## check/set window size
  if(missing(window) == TRUE) {
    
    window <- round(x = length(time) * 0.1 * dt, digits = 0)
  }
  
  ## check/set kernel size
  if(missing(kernel) == TRUE) {
    
    kernel <- NULL
  }
    
  ## collect function arguments
  eseis_arguments <- list(data = "",
                          time = time,
                          window = window,
                          overlap = overlap,
                          dt = dt,
                          method = method,
                          kernel = kernel,
                          order = order,
                          plot = plot)

  ## create window limit vectors
  l_lower <- seq(from = 1, 
                 to = length(data[[1]]) - window * 1 / dt, 
                 by = round(x = window * (1 - overlap) * 1 / dt, 
                            digits = 0))

  l_upper <- l_lower + window * 1 / dt
  
  l <- as.list(data.frame((rbind(l_lower, l_upper))))
  
  ## apply hv-function to windows
  if(is.null(kernel) == TRUE) {
    
    hv_slices <- lapply(
      X = l, 
      FUN = function(X, data, dt, method, order) {
        
        data_i <- do.call(cbind, data)[X[1]:X[2],]
        eseis::signal_hvratio(data = data_i, 
                              dt = dt, 
                              method = method, 
                              order = order)
        
      }, data = data, 
      dt = dt, 
      method = method, 
      order = order)
    
  } else {
    
    hv_slices <- lapply(
      X = l, 
      FUN = function(X, data, dt, method, kernel, order) {
        
        data_i <- do.call(cbind, data)[X[1]:X[2],]
        eseis::signal_hvratio(data = data_i, 
                              dt = dt, 
                              method = method, 
                              kernel = kernel, 
                              order = order)
        
      }, data = data, 
      dt = dt, 
      method = method, 
      kernel = kernel, 
      order = order)
  }
  
  hv_ratios <- do.call(cbind, lapply(X = hv_slices, FUN = function(X) {
    
    X$ratio
  }))
  
  hv_frequency <- hv_slices[[1]]$frequency
  
  hv_time <- time[round(x = (l_lower + l_upper) / 2, digits = 0)]
  
  rownames(hv_ratios) <- hv_frequency
  colnames(hv_ratios) <- hv_time
  
  if(plot == TRUE) {
    
    image(x = hv_time,
          y = hv_frequency, 
          z = t(hv_ratios),
          ...)
  }
  
  ## optionally rebuild eseis object
  if(eseis_class == TRUE) {
    
    ## assign aggregated signal vector
    eseis_data <- list(hv = list(time = hv_time,
                                 frequncy = hv_frequency,
                                 ratios = hv_ratios),
                       history = eseis_data$history)
    
    ## calculate function call duration
    eseis_duration <- as.numeric(difftime(time1 = Sys.time(), 
                                          time2 = eseis_t_0, 
                                          units = "secs"))
    
    ## update object history
    eseis_data$history[[length(eseis_data$history) + 1]] <- 
      list(time = Sys.time(),
           call = "aux_hvanalysis()",
           arguments = eseis_arguments,
           duration = eseis_duration)
    names(eseis_data$history)[length(eseis_data$history)] <- 
      as.character(length(eseis_data$history))
    
    ## set S3 class name
    class(eseis_data)[1] <- "eseis"
    
    ## assign eseis object to output data set
    hv <- eseis_data
  }
  
  return(hv_ratios)
}


