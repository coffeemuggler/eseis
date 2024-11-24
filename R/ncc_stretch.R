#' Estimate relativ wave velocity change (dv/v) by correlation stretching
#' 
#' The function estimates the relative seismic wave velocity changes over 
#' time based on matching iteratively stretched master correlations to 
#' previously calculated correlograms (cf. \code{ncc_correlate}).
#'
#' @param data \code{eseis} object of type \code{correlation}, output 
#' of \code{aux_correlate}.
#' 
#' @param lag \code{Numeric} vector of length two, range of the time lage
#' to analyse. If omitted, the time lag of the input data 
#' (\code{x$CC$lag}) is used.
#' 
#' @param master \code{Character} vector or value, either a user defined 
#' master correlation function or a keyword denoting the  method used to 
#' calculate master the correlation function. One out of \code{"mean"}, 
#' \code{"median"} and \code{"quantile"}. Default is \code{"mean"}. if 
#' \code{"quantile"} is used, the quantile probability must be specified as 
#' well, e.g., \code{"probs = 0.5"}. 
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
#' value or the maximum R^2 value is returned, and the returned standard 
#' deviation will be NA). A change to \code{0.05} will return mean and 
#' standard deviation of the five best percent of the solutions.
#' 
#' @param \dots Further arguments passed to the function.
#' 
#' @return A \code{data.frame}, object with the time and relative wave 
#' velocity change estimate.  
#' 
#' @author Michael Dietze
#' 
#' @keywords eseis
#' 
#' @examples
#' 
#' \dontrun{
#' 
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
#'    ## estimate dv/v
#'    dv <- ncc_stretch(data = cc, range = 0.05)
#'    
#'    ## plot result
#'    plot(dv$time, dv$dvv, type = "l")
#'                      
#' }           
#'                                                               
#' @export ncc_stretch

ncc_stretch <- function(
    data,
    lag,
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
  if(inherits(x = data, what = "eseis") == FALSE) {
    
    stop("Input data must be an eseis object of type correlogram!")
  } else {
    
    if(data$meta$type != "correlogram") {
      
      stop("Input data must be an eseis object of type correlogram!")
    }
  }
  
  ## check keywords for master argument
  if(master[1] %in% c("mean", "median", "quantile") == FALSE) {
    
    if(is.numeric(master) == FALSE) {
     
      stop("Keyword for master not supported!")
    }
  }
  
  ## check keywords for sides argument
  if(sides %in% c("both", "left", "right", "single") == FALSE) {
    
    stop("Keyword for sides not supported!")
  }
  
  ## check method argument
  if(method %in% c("rmse", "r") == FALSE) {
    
    "Method for finding best stretch match not supported!"
  }
  
  ## check rejection threshold value
  if(reject < 0 | reject > 1) {
    
    stop("Only rejection values between 0 and 1 are allowed!")
  }
  
  ## extract additional arguments
  args <- list(...)
  
  ## generate master trace ----------------------------------------------------
  
  ## optionally clip correlogram to lag of interest
  if(missing(lag) == FALSE) {
    
    ## check that user define lag is not larger than data lag
    if(lag[1] < min(data$CC$lag) | lag[2] > max(data$CC$lag)) {
      
      stop("Lag is smaller or larger than available from data!")
    } else {
      
      ## identify suitable lag values
      lag_ok <- data$CC$lag >= lag[1] & data$CC$lag <= lag[2]
      
      ## remove lag values out of range
      data$CC$lag <- data$CC$lag[lag_ok]
      data$CC$CC <- data$CC$CC[lag_ok,]
    }
  }
  
  ## calculate normalised correlation data set
  if(normalise == TRUE) {
    
    data_norm <-  2 * (apply(X = data$CC$CC, 
                              MARGIN = 2, 
                              FUN = function(data) {
                                
                                (data - min(data, na.rm = TRUE)) / 
                                  (max(data, na.rm = TRUE) - 
                                     min(data, na.rm = TRUE))
                              })) - 1
  } else {
    
    data_norm <- data$CC$CC
  }
  
  ## calculate master correlation
  if(is.numeric(master)) {
    
    data_master <- master
    
  } else if(master[1] == "mean") {
    
    data_master <- rowMeans(data_norm, na.rm = TRUE)
    
  } else if(master[2] == "median") {
    
    data_master <- apply(X = data_norm, 
                         MARGIN = 1,
                         FUN = quantile, 
                         probs = 0.5, 
                         na.rm = TRUE)
    
  } else if(master[3] == "quantile") {
    
    ## check/set probs argument
    if ("probs" %in% names(args)) {
      
      quantile_probs <- args$probs
    } else {
      
      quantile_probs <- 0.5
      warning("No probs value given, set to 0.5 automatically!")
    }
    
    data_master <- apply(X = data_norm, 
                         MARGIN = 1,
                         FUN = quantile, 
                         probs = quantile_probs, 
                         na.rm = TRUE)
  }
  
  ## replace NA by zero values
  data_norm[is.na(data_norm)] <- 0
  data_master[is.na(data_master)] <- 0
  
  ## extend master correlation to avoid interpolation artefacts
  n_add <- round(length(data_master) * 0.05, 0)
  
  data_master_ext <- caTools::runmean(x = c(rep(NA, n_add), 
                                            data_master, 
                                            rep(NA, n_add)), 
                                      k = n_add, 
                                      endrule = "mean")
  
  data_master_ext[(n_add + 1):(length(data_master_ext) - n_add)] <- 
    data_master
  
  ## generate stretch vector
  stretches <- seq(from = -range, 
                  to = range, 
                  length.out = steps)
  
  ## generate stretching factors for time lag vector
  k <- exp(-stretches)
  
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
  
  ## apply spline
  data_master_stretch <- lapply(X = k, FUN = function(k, time_idx){
    
    spln(x = time_idx * k)
  }, time_idx)
  
  ## convert list to matrix
  data_master_stretch <- do.call(rbind, 
                                 data_master_stretch)
  
  ## remove extended parts
  data_master_stretch <- 
    data_master_stretch[,(n_add + 1):(ncol(data_master_stretch) - n_add)]
  
  ## convert input data row-wise to list
  data_norm_list <- as.list(as.data.frame((data_norm)))
  
  if(method == "r") {
    
    ## calculate max R^2 between stretched master traces and data
    delta <- 
      lapply(X = data_norm_list, FUN = function(x, 
                                                data_master_stretch,
                                                reject) {
        
        r <- apply(X = data_master_stretch, 
                   MARGIN = 1, 
                   FUN = function(data_master_stretch) {
                     
                     cor(x = x, y = data_master_stretch)^2
                   })
        
        i_get <- seq(from = 1, 
                     to = length(r))[r >= quantile(x = r, 
                                                   probs = 1 - reject, 
                                                   na.rm = TRUE)]
        
        return(list(i_get = i_get,
                    r_all = r))
        
      }, data_master_stretch, reject)
    
  } else if(method == "rmse") {
    
    ## calculate RMS differences between stretched master traces and data
    delta <- 
      lapply(X = data_norm_list, FUN = function(x, 
                                                data_master_stretch,
                                                reject) {
        
        diff <- apply(X = data_master_stretch, 
                      MARGIN = 1, 
                      FUN = function(data_master_stretch) {
                        
                        sqrt(mean((data_master_stretch - x)^2, 
                                  na.rm = TRUE))
                      })
        
        i_get <- seq(from = 1, 
                     to = length(diff))[diff <= quantile(x = diff, 
                                                         probs = reject, 
                                                         na.rm = TRUE)]
        
        return(list(i_get = i_get,
                    r_all = diff))
        
      }, data_master_stretch, reject)
  }
  
  ## separate outputs
  delta_r <- lapply(X = delta, FUN = function(x) {
    
    x$r_all
  })
  
  delta_max <- lapply(X = delta, FUN = function(x) {
    
    x$i_get
  })
  
  ## convert correlation coefficients to matrix
  delta_r <- do.call(cbind, delta_r)
  delta_r <- delta_r[nrow(delta_r):1,]
  
  ## get mean delta
  delta_mean <- lapply(X = delta_max, FUN = function(delta_max, stretches) {
    
    -mean(stretches[delta_max], 
          na.rm = TRUE)
    
  }, stretches)
  
  delta_mean <- do.call(c, delta_mean)
  
  ## return output
  return(data.frame(time = data$CC$t, 
                    dvv = delta_mean))
}
