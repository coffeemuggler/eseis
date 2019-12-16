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
#' componenents to be processed. If \code{data} is a matrix, the components 
#' must be organised as columns. Also, \code{data} can be a list of 
#' \code{eseis} objects.
#' 
#' @param time \code{POSIXct} vector, time vector corresponding to the 
#' seismic signal components. If omitted, a synthetic time vector will 
#' be generated. If omitted, the sampling period (\code{dt}) must be 
#' provided.
#' 
#' @param dt \code{Numeric} value, sampling period. Only needed if 
#' \code{time} is omitted or if \code{data} is no \code{eseis} object.
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
#' \code{"xyz"} (EW-NS-vertical).
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
#'                     dt = 1 / 200,
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
  
  ## check/set dt
  if(missing(dt) == TRUE && class(data[[1]])[1] != "eseis") {
    
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
    
  } else if(missing(dt) == TRUE){
    
    dt <- NULL
    
    time <- NULL
  }
  
  ## check/set window size
  if(missing(window) == TRUE) {
    
    if(class(data[[1]])[1] == "eseis") {
      
      n <- data[[1]]$meta$n
    } else {
      
      n <- nrow(data)
    }
    
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
  
  ## get start time
  eseis_t_0 <- Sys.time()
  
  ## collect function arguments
  eseis_arguments <- list(data = "",
                          time = time, 
                          dt = dt,
                          window = window,
                          step = step,
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
    
    ## update dt
    dt <- eseis_data[[1]]$meta$dt
    
    ## generate time vector
    time <- seq(from = eseis_data[[1]]$meta$starttime, 
                by = eseis_data[[1]]$meta$dt,
                length.out = eseis_data[[1]]$meta$n)
    
  } else {
    
    ## set eseis flag
    eseis_class <- FALSE
  }
  
  ## homogenise data structure
  if(class(data)[1] == "list") {
    
    data <- do.call(cbind, data)
  }
  
  data <- as.data.frame(x = data)
  
  ## optionally update component order
  component_ID <- strsplit(x = order, split = "")[[1]]
  
  data <- data[,order(component_ID)]
  
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
  
  ## optionally rebuild eseis object
  if(eseis_class == TRUE) {
    
    ## assign aggregated signal vector
    eseis_data <- list(time = time_i,
                       eigen = eig_ratio_i,
                       azimuth = azimuth_i,
                       inclination = inclination_i,
                       history = eseis_data[[1]]$history)
    
    ## calculate function call duration
    eseis_duration <- as.numeric(difftime(time1 = Sys.time(), 
                                          time2 = eseis_t_0, 
                                          units = "secs"))
    
    ## update object history
    eseis_data$history[[length(eseis_data$history) + 1]] <- 
      list(time = Sys.time(),
           call = "signal_motion()",
           arguments = eseis_arguments,
           duration = eseis_duration)
    names(eseis_data$history)[length(eseis_data$history)] <- 
      as.character(length(eseis_data$history))
    
    ## set S3 class name
    class(eseis_data)[1] <- "eseis"
    
    ## assign eseis object to output data set
    data_out <- eseis_data
  }
  
  ## return function output
  return(data_out)
}
