#' Analyse correlation data for relative velocity changes.
#' 
#' The function calculates relative velocity changes over time based on 
#' cross-correlation data (i.e., the output of \code{aux_nccpreprocess}).
#' 
#' @param data \code{list} object, output of \code{aux_nccpreprocess},
#' containing the elements \code{time} (vector with time stamps of 
#' cross-correlation data), \code{lag} (vector of lag times around centre)
#' and \code{correlation} (matrix with row-wise organised time snippets of 
#' cross-correlation data).
#' 
#' @param master \code{Character} value, method used to calculate master 
#' correlation trace. One out of \code{"mean"}, \code{"median"} and 
#' \code{"quantile"}. Default is \code{"mean"}. IF \code{"quantile"} is used,
#' the quantile probability must be specified as well, e.g., 
#' \code{"probs = 0.5"}. Alternatively, the master correlation function can 
#' be provided directly as numeric vector.
#' 
#' @param normalise \code{Logical} value, option to normalise the data set 
#' before calculating the master trace. Default is \code{TRUE}.
#' 
#' @param sides \code{Character} value. One out of \code{"both"} (both sides
#' of the input data), \code{"left"} (only negative time lags), \code{"right"} 
#' (only positive time lags) and \code{single} (only right side is used,  
#' expecting data from a single source and direction). Default is 
#' \code{"both"}.
#' 
#' @param range \code{Numeric} value, relative range of the stretch. Default is 
#' \code{0.01} (1 percent).
#' 
#' @param steps \code{Numeric} value, number of stretch steps (step 
#' resolution). Default is \code{100}.
#' 
#' @param method \code{Charcter} value, method used to identify best match of 
#' cross correlation time slices with stretched master data set. One out of 
#' \code{"rmse"} (minimum root mean square error) and \code{"r"} (maximum 
#' R^2). Default is \code{"r"}.
#' 
#' @param reject \code{Numeric} value, rejection threshold for stretch values.
#' This value defines up to which quantile matching stretch solutions will 
#' be treated as valid solutions. Default is \code{0} (Only the minimum RMSE
#' value or the maximum R^2 value is returned). A chane to \code{0.05} will 
#' return mean and standard deviation of the five best percent of the 
#' solutions.
#' 
#' @param \dots Further arguments passed to the function.
#' 
#' @return A \code{Data frame}, mean and standard deviations of the relative 
#' velocity changes for input time windows.
#' 
#' @author Michael Dietze
#' 
#' @keywords eseis
#' 
#' @examples
#' 
#' ## preprocess data and calculate window-wise cross-correlation functions
#' cc <- ncc_preprocess(period = as.POSIXct(x = c("2017-04-09 00:00:00",
#'                                                   "2017-04-09 03:00:00"),
#'                                             tz = "UTC"),
#'                         station = c("RUEG1", "RUEG2"),
#'                         component = c("BHZ", "BHZ"),
#'                         dir = paste0(system.file("extdata", 
#'                                                  package="eseis"), "/"),
#'                         window = 1.5 * 3600,
#'                         overlap = 0.5,
#'                         window_sub = 1800,
#'                         overlap_sub = 0,
#'                         lag = 20,
#'                         deconvolve = TRUE,
#'                         sensor = "TC120s",
#'                         logger = "Cube3ext",
#'                         gain = 1,
#'                         f = c(0.1, 0.5),
#'                         sign = TRUE)  
#'                         
#' ## calculate relative velocity change
#' dv <- ncc_process(data = cc)
#' 
#' ## plot velocity change time series
#' plot(x = cc$time, 
#'      y = dv$mean, 
#'      type = "l")
#'                                                               
#' @export ncc_process
ncc_process <- function(
  data,
  master = "mean",
  normalise = TRUE,
  sides = "both",
  range = 0.01,
  steps = 100,
  method = "r",
  reject = 0,
  ...
) {
  
  ## check/set arguments ------------------------------------------------------
  
  ## check input data set
  if(missing(data) == TRUE) {
    
    stop("No data set present!")
  } else {
    
    if(class(data) != "list") {
      
      stop("Data set must be a list, see aux_nccpreprocess()!")
    } else {
      
      if(sum(names(data) == c("time", "lag", "correlation")) != 3) {
        
        stop("Data set must contain elements time, lag and correlation!")
      }
    }
  }
  
  ## check keywords for master argument
  if(sum(master == c("mean", "median", "quantile")) < 1) {
    
    stop("Keyword for master not supported!")
  }
  
  ## check keywords for sides argument
  if(sum(sides == c("both", "left", "right", "single")) < 1) {
    
    stop("Keyword for sides not supported!")
  }
  
  ## check method argument
  if(sum(method == c("rmse", "r")) < 1) {
    
    "Method for finding best stretch match not supported!"
  }
  
  ## check rejection threshold value
  if(reject < 0 | reject > 1) {
    
    stop("Only rejection values between 0 and 1 are allowed!")
  }
  
  ## extract additional arguments
  args <- list(...)

  ## generate master trace ----------------------------------------------------
  
  ## calculate normalised correlation data set
  if(normalise == TRUE) {
    
    data_norm <-  2 * t(apply(X = data$correlation, 
                         MARGIN = 1, 
                         FUN = function(data) {
      
                           (data - min(data, na.rm = TRUE)) / 
                             (max(data, na.rm = TRUE) - 
                                min(data, na.rm = TRUE))
    })) - 1
  } else {
    
    data_norm <- data$correlation
  }
  
  if(master == "mean") {
    
    data_master <- colMeans(data_norm, na.rm = TRUE)
    
  } else if(master == "median") {
    
    data_master <- apply(X = data_norm, 
                         MARGIN = 2,
                         FUN = quantile, 
                         probs = 0.5, 
                         na.rm = TRUE)
    
  } else if(master == "quantile") {
    
    ## check/set probs argument
    if ("probs" %in% names(args)) {
      
      quantile_probs <- args$probs
    }
    else {
      
      quantile_probs <- 0.5
      warning("No probs value given, set to 0.5 automatically!")
    }
    
    data_master <- apply(X = data_norm, 
                         MARGIN = 2,
                         FUN = quantile, 
                         probs = quantile_probs, 
                         na.rm = TRUE)
  }
  
  ## generate stretch vector
  stretchs <- seq(from = -range, 
                  to = range, 
                  length.out = steps)
  
  ## generate stretching factors for time lag vector
  k <- exp(-stretchs)
  
  ## taper reference trace and extend it to avoid interpolation artefacts
  n_add <- round(length(data_master) * 0.05, 0)
  
  data_master_taper <- signal_taper(data = data_master, 
                                    p = 0.01)
  data_master_ext <- caTools::runmean(x = c(rep(NA, n_add), 
                                            data_master_taper, 
                                            rep(NA, n_add)), 
                                      k = n_add, 
                                      endrule = "mean")
  
  data_master_ext[(n_add + 1):(length(data_master_ext) - n_add)] <- 
    data_master_taper
  
  ## generate time index vector
  if(sides == "single") {
    
    time_idx <- seq(from = 1, 
                    to = length(data_master_ext))
  } else {
    
    time_idx <- seq(from = 1, 
                    to = length(data_master_ext)) - 
      length(data_master_ext) / 2
  }
  
  ## create spline interpolator for reference trace
  spln <- splinefun(x = time_idx,
                    y = data_master_ext, 
                    method = "fmm")
  
  ## apply spline to 
  data_master_stretch <- lapply(X = k, FUN = function(k, time_idx){
    
    spln(x = time_idx * k)
  }, time_idx)
  
  ## convert list to matrix
  data_master_stretch <- do.call(rbind, 
                                 data_master_stretch)
  
  ## remove extended parts
  data_master_stretch <- data_master_stretch[,-(1:n_add)]
  data_master_stretch <- data_master_stretch[,1:ncol(data_norm)]
  
  ## convert input data row-wise to list
  data_norm_list <- as.list(as.data.frame(t(data_norm)))

  if(method == "r") {
    
    ## calculate max R^2 between stretched master traces and data
    delta <- 
      lapply(X = data_norm_list, FUN = function(x, 
                                                data_master_stretch,
                                                reject) {
        
        r <- apply(X = t(data_master_stretch), 
                      MARGIN = 2, 
                      FUN = function(data_master_stretch) {
                        
                        cor(x = x, y = data_master_stretch)^2
                      })
        
        i_get <- seq(from = 1, 
                     to = length(r))[r >= quantile(x = r, 
                                                   probs = 1 - reject, 
                                                   na.rm = TRUE)]
        
        return(i_get)
        
    }, data_master_stretch, reject)
    
  } else if(method == "rmse") {
    
    ## calculare RMS differences between stretched master traces and data
    delta <- 
      lapply(X = data_norm_list, FUN = function(x, 
                                                data_master_stretch,
                                                reject) {
        
        diff <- apply(X = t(data_master_stretch), 
                      MARGIN = 2, 
                      FUN = function(data_master_stretch) {
                        
                        sqrt(mean((data_master_stretch - x)^2, 
                                  na.rm = TRUE))
                      })
        
        i_get <- seq(from = 1, 
                     to = length(diff))[diff <= quantile(x = diff, 
                                                         probs = reject, 
                                                         na.rm = TRUE)]
        
        return(i_get)
        
    }, data_master_stretch, reject)
  }
  
  ## get mean delta
  delta_mean <- lapply(X = delta, FUN = function(delta, stretchs) {
    
    -mean(stretchs[delta], 
         na.rm = TRUE)
    
  }, stretchs)
  
  delta_mean <- do.call(c, delta_mean)
  
  ## get mean delta
  delta_sd <- lapply(X = delta, FUN = function(delta, stretchs) {
    
    sd(stretchs[delta], 
       na.rm = TRUE)
    
  }, stretchs)
  
  delta_sd <- do.call(c, delta_sd)
  
  ## return output
  return(data.frame(mean = delta_mean,
                    sd = delta_sd))
}
