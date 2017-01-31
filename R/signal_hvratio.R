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
#' must be organised as columns.
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
#' @param order \code{Caracter} value, order of the seismic components. 
#' Describtion must contain the letters \code{"x"},\code{"y"} and
#' \code{"z"} in the order according to the input data set. Default is 
#' \code{"xyz"} (NW-SE-vertical).
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
#' plot(hv, type = "l")
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
  
  ## check/set input arguments
  if(missing(dt) == TRUE) {
    
    stop("No sampling period provided!")
  }
  
  ## homogenise data structure
  if(class(data) != "list") {
    
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
  if(missing(kernel) == FALSE) {
    
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
  
  ## return output
  return(hv)
}