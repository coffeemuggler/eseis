#' Calculate particle motion parameters
#' 
#' The function calculates from a data set of three seismic 
#' components of the same signal the following particle motion 
#' paramters using a moving window approach: horizontal-vertical 
#' eigenvalue ratio, azimuth and inclination.
#' 
#' The function code is loosely based on the function GAZI() from 
#' the package RSEIS with removal of unnecessary content and updated 
#' or rewritten loop functionality. In its current form, it also uses 
#' additional workflows from obspy.signal.polarisation, specifically 
#' following the Flinn (1965) approach. It windows the input signals,
#' calculates the covariance matrix and performs a singular values 
#' decomposition to use the eigen values and vectors to determine the 
#' ratios corresponding to the output values rectilinearity, angularity,
#' azimuth and incidence. 
#' 
#' Note that the names of the output objects as well as the calculation 
#' routine have changed from the earlier version (V. 0.6.0), to increase 
#' computational efficiency and fix a bug in the windowing implementation.
#' 
#' @param data \code{List} of eseis objects or \code{matrix}, seismic
#' components to be processed. If \code{data} is a matrix, the components 
#' must be organised as columns.
#' 
#' @param time \code{POSIXct} vector, time vector corresponding to the 
#' seismic signal components. If omitted, a synthetic time vector will 
#' be generated. If omitted, the sampling period (\code{dt}) must be 
#' provided or is taken from the first eseis object in the \code{data} list.
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
#' Description must contain the letters \code{"x"},\code{"y"} and
#' \code{"z"} in the order according to the input data set. Default is 
#' \code{"xyz"} (EW-NS-vertical).
#' 
#' @return A List object with rectilinearity (\code{rectilinearity}),  
#' angularity (\code{polarity}), azimuth (\code{azimuth}) and incidence 
#' (\code{incidence}), as well as the corresponding time vector for 
#' these values.
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
#' ## convert list of signal vectors to column-wise matrix
#' s <- do.call(cbind, s)
#' 
#' ## calculate particle motion parameters
#' pm <- signal_motion(data = s, 
#'                     dt = 1 / 200,
#'                     window = 500, 
#'                     step = 250)
#'                     
#' ## plot function output
#' par_original <- par(no.readonly = TRUE)
#' par(mfcol = c(2, 2))
#' 
#' plot(pm$time, pm$rect, type = "b")
#' plot(pm$time, pm$plan, type = "b")
#' plot(pm$time, pm$azimuth, type = "b")
#' plot(pm$time, pm$incidence, type = "b")
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
        
        stop("Neither time vector nor dt value provided!")
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
  
  ## check/set time vector
  if(missing(time) == TRUE && class(data[[1]])[1] != "eseis") {
    
    time <- seq(from = 0,
                by = dt,
                length.out = nrow(data))
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
  
  ## calculate particle motion parameters for sliced data
  motion <- lapply(X = window_left, FUN = function(l, data, window) {
    
    ## define right window
    r <- l + window
    
    ## clip data to window size  
    x <- data[l:r,]
    
    ## calculate covariance matrix
    covmat <- stats::cov(x)
    
    ## calculate singular decomposition
    sv <- svd(x = covmat)
    
    ## calculate rectangularity    
    rect <- 1 - sqrt(sv$d[2] / sv$d[1])
    
    ## calculate planarity
    plan <- 1 - ((2 * sv$d[3]) / (sv$d[2] + sv$d[1]))
    
    ## calculate azimuth
    azimuth <- 180 / pi * atan2(sv$u[1,1], sv$u[2,1])
    
    ## calculate incidence
    incidence <- 180 / pi * atan2(sqrt(sv$u[1,1]^2 + sv$u[2,1]^2), 
                                  sv$u[3,1])
    
    ## correct values for trigonometric quadrants
    if(azimuth < 0) {azimuth <- 360 + azimuth}
    if(incidence < 0) {incidence <- incidence + 180}
    if(incidence > 90) {
      incidence <- 180 - incidence
      if(azimuth > 180) {
        azimuth <- azimuth - 180
      } else {
        azimuth <- azimuth + 180
      }
    }
    if(azimuth > 180) {azimuth <- azimuth - 180}
    
    ## return output
    return(data.frame( rect = rect,
                       plan = plan,
                       azimuth = azimuth,
                       incidence = incidence))
    
  }, data, window)
  
  ## convert list to data frame
  motion <- do.call(rbind, motion)
  
  ## calculate average window time
  time_avg <- time[floor(window_left +  step/2)]
  
  ## create function output
  data_out <- list(time = time_avg,
                   rect = motion$rect,
                   plan = motion$plan,
                   azimuth = motion$azimuth,
                   incidence = motion$incidence)
  
  ## optionally rebuild eseis object
  if(eseis_class == TRUE) {
    
    ## assign aggregated signal vector
    eseis_data <- list(time = time_avg,
                       rect = motion$rect,
                       plan = motion$plan,
                       azimuth = motion$azimuth,
                       incidence = motion$incidence,
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