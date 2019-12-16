#' Calculate h-v-ratio of seismic components
#' 
#' This function uses three components of a seismic signal, evaluates 
#' their spectra and builds the ratio of horizontal to vertical power. 
#' For details see http://www.geopsy.org/documentation/geopsy/hv.html.
#' 
#' The spectra should be smoothed. This can either be done directly 
#' during their calculation or before the calculation of the ratio. For 
#' the former case set \code{method = "autoregressive"}. For the latter 
#' case provide a value for \code{"kernel"}, which is the smoothing 
#' window size. Smoothing is performed with the logarithms of the spectral 
#' power data, using \code{caTools::runmean()} with the 
#' \code{endrule = "NA"}. After smoothing the data is re-linearised.
#' 
#' @param data \code{List}, \code{data frame} or \code{matrix}, seismic
#' componenents to be processed. If \code{data} is a matrix, the components 
#' must be organised as columns. Also, \code{data} can be a list of 
#' \code{eseis} objects.
#' 
#' @param dt \code{Numeric} value, sampling period.
#' 
#' @param method \code{Character} value, method for calculating the spectra. 
#' One out of \code{"periodogram"} , \code{"autoregressive"} and 
#' \code{"multitaper"}, default is \code{"periodogram"}.
#' 
#' @param kernel \code{Numeric} value, window size (number of samples) of 
#' the moving window used for smoothing the spectra. By default no smoothing 
#' is performed.
#' 
#' @param order \code{Character} value, order of the seismic components. 
#' Describtion must contain the letters \code{"x"},\code{"y"} and
#' \code{"z"} in the order according to the input data set. Default is 
#' \code{"xyz"} (EW-SN-vertical).
#' 
#' @return A \code{data frame} with the h-v-frequency ratio.
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
#' ## calculate h-v-ratio, will be very rugged
#' hv <- signal_hvratio(data = s, 
#'                      dt = 1 / 200)
#' plot(hv$ratio, 
#'      type = "l", 
#'      log = "y")
#' 
#' ## calculate h-v-ratio using the autogressive spectrum method
#' hv <- signal_hvratio(data = s, 
#'                      dt = 1 / 200, 
#'                      method = "autoregressive")
#' plot(hv, type = "l")
#' 
#' ## calculate h-v-ratio with a smoothing window equivalent to dt
#' hv <- signal_hvratio(data = s, 
#'                      dt = 1 / 200,
#'                      kernel = 200)
#' plot(hv, type = "l")
#' 
#' @export signal_hvratio
#' 
signal_hvratio <- function(
  
  data,
  dt,
  method = "periodogram",
  kernel,
  order = "xyz"
) {
  
  ## check/set dt
  if(missing(dt) == TRUE && class(data[[1]])[1] != "eseis") {
    
    warning("Sampling frequency missing! Set to 1/200")
    
    dt <- 1 / 200
    
  } else if(missing(dt) == TRUE){
    
    dt <- NULL
  }
  
  if(missing(kernel) == TRUE) {
    
    kernel <- NULL
  }
  
  ## get start time
  eseis_t_0 <- Sys.time()
  
  ## collect function arguments
  eseis_arguments <- list(data = "",
                          dt = dt,
                          method = method,
                          kernel = kernel,
                          order = order)
  
  ## homogenise data structure
  if(class(data[[1]])[1] == "eseis") {
    
    ## set eseis flag
    eseis_class <- TRUE
    
    ## store initial object
    eseis_data <- data
    
    ## extract signal vector
    data <- lapply(X = data, FUN = function(X) {
      
      X$signal
    })
    
    ## convert signal vector list to matrix
    data <- do.call(cbind, data)
    
    ## update dt
    dt <- eseis_data[[1]]$meta$dt
    
  } else {
    
    ## set eseis flag
    eseis_class <- FALSE
  }
    
  if(class(data)[1] != "list") {
    
    data <- as.list(as.data.frame(data))
  }
  
  ## optionally update component order
  component_ID <- strsplit(x = order, split = "")[[1]]
  
  data <- data[order(component_ID)]

  ## calculate spectra
  s <- eseis::signal_spectrum(data = data, 
                              dt = dt, 
                              method = method)
  
  ## optionally smooth data
  if(is.null(kernel) == FALSE) {
    
    ## log spectral power vectors
    s_log <- lapply(X = s, FUN = function(X) {
      
      log(X$spectrum)
    })
    
    ## apply running mean
    s_log_smooth <- lapply(X = s_log, FUN = function(X, kernel) {
      
      caTools::runmean(x = X, 
                       k = kernel, 
                       endrule = "NA")
    }, kernel = kernel)
    
    ## bring data back to normal space
    s_smooth <- lapply(X = s_log_smooth, FUN = function(X) {
      
      exp(X)
    })
  } else {
    
    s_smooth <- lapply(X = s, FUN = function(X) {
      
      X$spectrum
    })
  }
  
  ## calculate h-v-ratio
  hv <- data.frame(
    frequency = s[[1]]$frequency,
    ratio = sqrt(s_smooth[[1]]^2 + s_smooth[[2]]^2) / s_smooth[[3]])
  
  ## optionally rebuild eseis object
  if(eseis_class == TRUE) {
    
    ## assign aggregated signal vector
    eseis_data <- list(ratio = hv,
                       history = eseis_data$history)
    
    ## calculate function call duration
    eseis_duration <- as.numeric(difftime(time1 = Sys.time(), 
                                          time2 = eseis_t_0, 
                                          units = "secs"))
    
    ## update object history
    eseis_data$history[[length(eseis_data$history) + 1]] <- 
      list(time = Sys.time(),
           call = "signal_hvratio()",
           arguments = eseis_arguments,
           duration = eseis_duration)
    names(eseis_data$history)[length(eseis_data$history)] <- 
      as.character(length(eseis_data$history))
    
    ## update data type
    eseis_data$meta$type = "hvratio"
    
    ## set S3 class name
    class(eseis_data)[1] <- "eseis"
    
    ## assign eseis object to output data set
    hv <- eseis_data
  }
  
  ## return output
  return(hv)
}