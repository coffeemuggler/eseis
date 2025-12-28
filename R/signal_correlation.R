#' Calculate signal cross-correlation values
#' 
#' This function calculates the running cross-correlation of two or more 
#' seismic signas and returns that characteristic function.
#'
#' @param data \code{eseis} object, \code{list} object with data sets to be 
#' processed. The list can contain numeric vectors or eseis objects. It is 
#' assumed that all vectors have the same length.
#' 
#' @param window \code{Numeric} value, size of the running window, in 
#' number of samples
#' 
#' @return \code{Numeric} running cross-correlation of the input signals.
#' 
#' @author Michael Dietze
#' 
#' @keywords eseis
#' 
#' @examples
#' 
#' ## calculate cross-correlation
#' s_cc <- signal_correlation(data = list(a = runif(1000),
#'                                        b = runif(1000),
#'                                        c = runif(1000)),
#'                            window = 200)
#' 
#' @export signal_correlation

signal_correlation <- function(
    data,
    window = 200
) {
  
  ## check that time window is provided
  if(missing(window) == TRUE) {
    stop("No window provided!")
  }
  
  ## check that more than one data set is provided
  if(class(data)[1] == "numeric" | length(data) == 1) {
    
    stop("At least two signals must be provided!")
  }

  ## check/adjust data structure
  if(class(data[[1]])[1] == "eseis") {
    
    ## set eseis flag
    flag_eseis <- TRUE

    ## get start time
    eseis_t_0 <- Sys.time()

    ## collect function arguments
    eseis_arguments <- list(data = "",
                            window = window)

    ## store initial object
    eseis_data <- data[[1]]

    ## extract signal vectors
    data <- lapply(X = data, FUN = function(X) {

      X$signal
    })

    ## test common length of input objects
    n_data <- sapply(X = data, FUN = length)

    if(any(n_data != mean(n_data))) {

      stop("Input data does not have common number of samples!")
    }

    ## convert list to matrix
    data <- do.call(rbind, data)

  } else {

    ## set eseis flag
    flag_eseis <- FALSE
    
    ## homogenise input data
    data <- do.call(rbind, data)
  }
  
  ## define combinations of signals
  cmb <- combn(x = 1:nrow(data), m = 2)
  
  ## calculate correlations
  data_cc <- lapply(X = 1:ncol(cmb), FUN = function(ii, cmb, data, window) {
    
    .run_cor(x = data[cmb[1,ii],], y = data[cmb[2,ii],], k = window)
  }, cmb = cmb, data = data, window = window)
  data_cc <- do.call(rbind, data_cc)
  
  ## average results
  data_out <- colMeans(x = data_cc, na.rm = TRUE)
  
  ## remove edge effects
  n_smp <- length(data_out)
  data_out[1] <- data_out[2]
  data_out[(n_smp - window):n_smp] <- data_out[n_smp - window - 1]
  
  ## optionally, re-build eseis object
  if(flag_eseis == TRUE) {
    
    ## assign aggregated signal vector
    eseis_data$signal <- data_out
    
    ## calculate function call duration
    eseis_duration <- as.numeric(difftime(time1 = Sys.time(),
                                          time2 = eseis_t_0,
                                          units = "secs"))
    
    ## update object history
    eseis_data$history[[length(eseis_data$history) + 1]] <-
      list(time = Sys.time(),
           call = "signal_correlation()",
           arguments = eseis_arguments,
           duration = eseis_duration)
    names(eseis_data$history)[length(eseis_data$history)] <-
      as.character(length(eseis_data$history))
    
    ## update data type and meta data
    eseis_data$meta$type <- "characteristic"
    eseis_data$meta$component <- "NA"
    
    ## assign eseis object to output data set
    data_out <- eseis_data
  }

  ## return data set
  return(invisible(data_out))
}