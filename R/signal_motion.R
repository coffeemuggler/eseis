#' Calculate particle motion parameters
#' 
#' The function calculates from a data set of three seismic 
#' components of the same signal the following particle motion 
#' paramters using a moving window approach: horizontal-vertical 
#' eigenvalue ratio, azimuth and inclination.
#' 
#' The function code is loosely based on the function GAZI() from 
#' the package RSEIS with removal of unnecessary content and updated 
#' or rewritten loop functionality.
#' 
#' @param data \code{List}, \code{data frame} or \code{matrix}, seismic
#' componenents to be plotted. If \code{data} is a matrix, the components 
#' must be organised as columns.
#' 
#' @param time \code{POSIXct} vector, time vector corresponding to the 
#' seismic signal components. If omitted, a synthetic time vector will 
#' be generated. If omitted, the sampling period (\code{dt}) must be 
#' provided.
#' 
#' @param dt \code{Numeric} value, sampling period. Only needed if 
#' \code{time} is omitted.
#' 
#' @param window \code{Numeric} value, time window length (given as 
#' number of samples) used to calculate the particle motion parameters. 
#' If value is even, it will be set to the next smaller odd value. If 
#' omitted, the window size is set to 1 percent of the time series length by
#' default.
#' 
#' @param step \code{Numeric} value, step size (given as number of samples), 
#' by which the window is shifted over the data set. If omitted, the step 
#' size is set to 50 percent of the window size by default.
#' 
#' @param order \code{Character} value, order of the seismic components. 
#' Describtion must contain the letters \code{"x"},\code{"y"} and
#' \code{"z"} in the order according to the input data set. Default is 
#' \code{"xyz"} (NW-SE-vertical).
#' 
#' @return A List object with eigenvalue ratios (\code{eigen}), 
#' azimuth (\code{azimuth}) and inclination (\code{inclination}) as well
#' as the corresponding time vector for these values.
#' 
#' @author Michael Dietze
#' 
#' @keywords eseis
#' 
#' @examples 
#' ## load example data set
#' data(earthquake)
#' 
#' ## filter seismic signals
#' s <- eseis::signal_filter(data = s, 
#'                           dt = 1/200, 
#'                           f = c(1, 3))
#' 
#' ## integrate signals to get displacement
#' s_d <- eseis::signal_integrate(data = s, dt = 1/200)
#' 
#' ## calculate particle motion parameters
#' pm <- signal_motion(data = s_d, 
#'                     time = t, 
#'                     window = 100, 
#'                     step = 10)
#'                     
#' ## plot function output
#' par_original <- par(no.readonly = TRUE)
#' par(mfcol = c(2, 1))
#' 
#' plot(x = t, y = s$BHZ, type = "l")
#' plot(x = pm$time, y = pm$azimuth, type = "l")
#' 
#' par(par_original)
#' 
#' @export signal_motion
#'
signal_motion <- function(
  
  data,
  time, 
  dt,
  window,
  step,
  order = "xyz"
) {
  
  ## homogenise data structure
  if(class(data) == "list") {
    
    data <- do.call(cbind, data)
  }
  
  data <- as.data.frame(x = data)
  
  ## optionally update component order
  component_ID <- strsplit(x = order, split = "")[[1]]
  
  data <- data[,order(component_ID)]
  
  ## check/set time vector
  if(missing(time) == TRUE) {
    
    if(missing(dt) == TRUE) {
      
      stop("Neither time nor dt provided!")
    } else {
      
      time <- seq(from = 0,
                  by = dt,
                  length.out = nrow(data))
    }
  }
  
  ## check/set window size
  if(missing(window) == TRUE) {
    
    window <- round(x = nrow(data) * 0.01, 
                    digits = 0)
  }
  
  ## optionally convert window size to even number
  if(window %% 2 != 0) {
    
    window <- window - 1
  }
  
  ## check/set step size
  if(missing(step) == TRUE) {
    
    step <- round(x = window * 0.5, 
                  digits = 0)
  }
  
  ## define window indices
  window_left <- seq(from = 1, 
                     to = nrow(data) - window, 
                     by = step)
  
  window_right <- seq(from = window, 
                      to = nrow(data), 
                      by = step)
  
  ## define output variables
  time_i <- time[(window_left + window_right) / 2]
  eig_ratio_i <- numeric(length = length(window_left))
  azimuth_i <- numeric(length = length(window_left))
  inclination_i <- numeric(length = length(window_left))
  
  ## do window-based calculus
  for(i in 1:length(window_left)) {
    
    ## isolate data within window
    data_i <- data[window_left[i]:window_right[i],]
    
    ## calculate covariance matrix
    cov_i <- stats::var(x = data_i)
    
    ## calcualate eigen space
    eig_i <- eigen(x = cov_i, symmetric = TRUE)
    
    ## calculate eigenvalue ratio
    eig_ratio_i[i] <- 1 - ((eig_i$values[2] + eig_i$values[3]) / 
                             (2 * eig_i$values[1]))
    
    ## calculate azimuth
    azimuth_i[i] <- 180 / pi * atan2(eig_i$vectors[2,1], 
                                     eig_i$vectors[3,1])
    
    ## calculate inclination
    inclination_i[i] <- abs(180 / pi * atan2(eig_i$vectors[1,1], 
                                             sqrt(eig_i$vectors[2,1]^2 + 
                                                    eig_i$vectors[3,1]^2)))
  }
  
  ## create furntion output
  data_out <- list(time = time_i,
                   eigen = eig_ratio_i,
                   azimuth = azimuth_i,
                   inclination = inclination_i)
  
  ## return function output
  return(data_out)
}
