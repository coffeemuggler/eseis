#' Estimate relative wave velocity change (dv/v) by correlation stretching
#' 
#' The function estimates the relative seismic wave velocity changes over 
#' time based on matching iteratively stretched master correlations to 
#' previously calculated correlograms (cf. \code{ncc_correlate}).
#'
#' @param data \code{eseis} object of type \code{correlogram}, output 
#' of \code{aux_correlate}.
#' 
#' @param range \code{Numeric} value, relative range of the stretch. Default is 
#' \code{0.01} (1 percent).
#' 
#' @param steps \code{Numeric} value, number of stretch steps (step 
#' resolution). Default is \code{100}.
#' 
#' @param lag \code{Numeric} vector of length two, range of the time lage
#' to analyse. If omitted, the time lag of the input data 
#' (\code{x$CC$lag}) is used.
#' 
#' @param sides \code{Character} value. One out of \code{"both"} (both sides
#' of the input data), \code{"left"} (only negative time lags), \code{"right"} 
#' (only positive time lags) and \code{single} (only right side is used,  
#' expecting data from a single source and direction). Default is 
#' \code{"both"}.
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
#' @param method \code{Charcter} value, method used to identify best match of 
#' cross correlation time slices with stretched master data set. One out of 
#' \code{"rms"} (inverse root mean square difference) and \code{"r"} (maximum 
#' R^2). Default is \code{"r"}.
#' 
#' @param min \code{Numeric} value, minimum R^2 value between a correlation 
#' function and the master function to yield a valid dv/v output. Cases with 
#' smaller R^2 values are set to \code{NA}. Default is \code{0} (no threshold 
#' applied). A meaningful threshold might be \code{0.5}.
#' 
#' @param reject \code{Numeric} value, rejection threshold for stretch values.
#' This value defines up to which quantile matching stretch solutions will 
#' be treated as valid solutions. Default is \code{0} (Only the minimum RMS
#' value or the maximum R^2 value is returned, and the returned standard 
#' deviation will be NA). A change to \code{0.05} will return mean and 
#' standard deviation of the five best percent of the solutions.
#' 
#' @param eseis \code{Logical} value, option to return data as \code{eseis}
#' object, default is \code{TRUE}.
#' 
#' @param \dots Further arguments passed to the function.
#' 
#' @return An \code{eseis} object, hence a \code{list} with the goodness-of-fit 
#' matrix, best-matching dv/v estimate, time and tested dv/v vectors, as well 
#' as meta data and the object's processing history.
#' 
#' @author Michael Dietze
#' 
#' @keywords eseis
#' 
#' @examples
#' 
#' \dontrun{
#' 
#'   ## calculate correlogram
#'   cc <- ncc_correlate(start = "2017-04-09 00:30:00", 
#'                       stop = "2017-04-09 01:30:00", 
#'                       ID = c("RUEG1", "RUEG2"), 
#'                       dt = 1/10,
#'                       component = c("Z", "Z"), 
#'                       dir = paste0(system.file("extdata", 
#'                                    package = "eseis"), "/"), 
#'                       window = 600, 
#'                       overlap = 0, 
#'                       lag = 20, 
#'                       f = c(0.05, 0.1), 
#'                       sd = 1)
#'    
#'    ## estimate dv/v
#'    dv <- ncc_stretch(data = cc, 
#'                      lag = c(1, 10), 
#'                      range = 0.4)
#'    
#'    ## plot result
#'    plot_dvv(data = dv)
#'                      
#' }           
#'                                                               
#' @export ncc_stretch

ncc_stretch <- function(
    data,
    range = 0.01,
    steps = 100,
    lag,
    sides = "both",
    master = "mean",
    normalise = TRUE,
    method = "r",
    min = 0,
    reject = 0,
    eseis = TRUE,
    ...
) {
  
  ## get start time
  eseis_t_0 <- Sys.time()
  
  ## collect function arguments
  eseis_arguments <- list(data = "",
                          lag = lag,
                          sides = sides, 
                          master = master,
                          normalise = normalise, 
                          range = range, 
                          steps = steps,
                          method = method,
                          min = min, 
                          reject = 0)
  
  ## store initial object
  eseis_data <- data[-1]
  
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
  
  ## check method argument
  if(method %in% c("rms", "r") == FALSE) {
    
    "Method for finding best stretch match not supported!"
  }
  
  ## check rejection threshold value
  if(reject < 0 | reject > 1) {
    
    stop("Only rejection values between 0 and 1 are allowed!")
  }
  
  ## extract additional arguments
  args <- list(...)
  
  ################################################################################
  #data_save <- data
  
  ## generate master trace ----------------------------------------------------
  
  ## optionally clip correlogram to lag of interest
  if(missing(lag) == FALSE) {
    
    ## handle left/right case
    if(sides == "left") {
      
      ## check that user define lag is not larger than data lag
      if(-lag[2] < min(data$CC$lag) | -lag[1] > max(data$CC$lag)) {
        
        stop("Lag is smaller or larger than available from data!")
      } else {
        
        ## get desired lag indices
        i_ok <- data$CC$lag >= -lag[2] & data$CC$lag <= 0
        
        ## remove lag values out of range
        data$CC$lag <- data$CC$lag[i_ok]
        data$CC$CC <- data$CC$CC[i_ok,]
        
        ## identify suitable lag values
        lag_ok <- data$CC$lag <= -lag[1]
      }
      
    } else if(sides == "right") {
      
      ## check that user define lag is not larger than data lag
      if(lag[1] < min(data$CC$lag) | lag[2] > max(data$CC$lag)) {
        
        stop("Lag is smaller or larger than available from data!")
      } else {
        
        ## get desired lag indices
        i_ok <- data$CC$lag >= 0 & data$CC$lag <= lag[2]
        
        ## remove lag values out of range
        data$CC$lag <- data$CC$lag[i_ok]
        data$CC$CC <- data$CC$CC[i_ok,]
        
        ## identify suitable lag values
        lag_ok <- data$CC$lag >= lag[1]
      }
    } else if(sides == "both") {
      
      ## check that user define lag is not larger than data lag
      if(-lag[2] < min(data$CC$lag) | -lag[1] > max(data$CC$lag) | 
         lag[1] < min(data$CC$lag) | lag[2] > max(data$CC$lag)) {
        
        stop("Lag is smaller or larger than available from data!")
      } else {
        
        ## get desired lag indices
        i_ok <- data$CC$lag >= -lag[2] & data$CC$lag <= 0 | 
          data$CC$lag >= 0 & data$CC$lag <= lag[2]
        
        ## remove lag values out of range
        data$CC$lag <- data$CC$lag[i_ok]
        data$CC$CC <- data$CC$CC[i_ok,]
        
        ## identify suitable lag values
        lag_ok <- data$CC$lag <= -lag[1] | data$CC$lag >= lag[1]
      }
    } else {
      
      stop("Keyword for sides not supported!")
    }
  }
  
  ## set unsuitable values to NA
  data$CC$CC[!lag_ok,] <- NA
  
  ## calculate normalised correlation data set
  if(normalise == TRUE) {
    
    data$CC$CC <-  2 * (apply(X = data$CC$CC, 
                              MARGIN = 2, 
                              FUN = function(data) {
                                
                                (data - min(data, na.rm = TRUE)) / 
                                  (max(data, na.rm = TRUE) - 
                                     min(data, na.rm = TRUE))
                              })) - 1
  }
  
  ## calculate master correlation
  if(is.numeric(master)) {
    
    data_master <- master
    
  } else if(master[1] == "mean") {
    
    data_master <- rowMeans(data$CC$CC, na.rm = TRUE)
    
  } else if(master[2] == "median") {
    
    data_master <- apply(X = data$CC$CC, 
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
    
    data_master <- apply(X = data$CC$CC, 
                         MARGIN = 1,
                         FUN = quantile, 
                         probs = quantile_probs, 
                         na.rm = TRUE)
  }
  
  ## identify NA values
  i_na <- is.na(data_master)
  
  ## replace NA by zero values
  data$CC$CC[i_na,] <- 0
  data_master[i_na] <- 0
  
  ## extend master correlation to avoid interpolation artefacts, get extent
  n <- round(length(data_master) * 0.1, 0)
  
  ## add n NA values on both sides
  data_master_ext <- c(rep(NA, n), data_master, rep(NA, n))
  
  ## interpolate added sides by running mean
  data_master_ext <- caTools::runmean(x = data_master_ext, 
                                      k = n, 
                                      endrule = "mean")
  
  ## re-insert original data in non-padded part
  data_master_ext[(n + 1):(length(data_master_ext) - n)] <- data_master
  
  ## generate dvv stretch vector
  dvv <- seq(from = -range, to = range, length.out = steps)
  
  ## generate stretching factors for time lag vector
  k <- exp(-dvv)
  
  ## generate time index vector for spline interpolation
  if(sides == "left") {
    
    time_idx <- seq(from = -length(data_master_ext), to = -1)
  } else if(sides == "right") {
    
    
    time_idx <- seq(from = 1, to = length(data_master_ext))
  } else {
    
    time_idx <- seq(from = 1, to = length(data_master_ext)) - 
      (length(data_master_ext) / 2)
  }
  
  ## re-insert NA values
  data$CC$CC[i_na,] <- NA
  data_master[i_na] <- NA
  
  ## create spline interpolator for master correlation
  spln <- splinefun(x = time_idx, y = data_master_ext, method = "fmm")
  
  ## apply spline
  stretches <- lapply(X = k, FUN = function(k, time_idx) {
    
    spln(x = time_idx * k)
  }, time_idx)
  
  ## convert list to matrix
  stretches <- do.call(rbind, stretches)
  
  ## remove extended parts
  stretches <- stretches[,(n + 1):(ncol(stretches) - n)]
  
  ## re-insert NA values
  stretches[,i_na] <- NA
  
  ## convert input data row-wise to list
  data_list <- as.list(as.data.frame(data$CC$CC))
  
  ## compare empirical correlations against stretched master correlations
  if(method == "r") {
    
    ## R^2 case
    D <- lapply(X = data_list, FUN = function(x, stretches, reject) {
      
      ## calculate R^2 for each stretched master correlation function 
      r <- apply(X = stretches, MARGIN = 1, FUN = function(y, x) {
        cor(x = x, y = y, use = "na.or.complete")^2
      }, x = x)
      
      ## get quantile threshold below which to reject 
      q_rej <- quantile(x = r, probs = 1 - reject, na.rm = TRUE)
      
      ## isolate stretched master correlations above rejection threshold
      i_ok <- seq(from = 1, to = length(r))[r >= q_rej]
      
      ## return output
      return(list(i_ok = i_ok,
                  r_all = r,
                  r_max = max(r, na.rm = TRUE)))
      
    }, stretches, reject)
    
  } else if(method == "rms") {
    
    ## RMS difference case
    D <- lapply(X = data_list, FUN = function(x, stretches, reject) {
        
      ## calculate RMS for each stretched master correlation function 
      rms <- apply(X = stretches, MARGIN = 1, FUN = function(y, x) {
        
        sqrt(mean((y - x)^2, na.rm = TRUE))
      }, x = x)
      
      ## normalise and invert rms
      i_rms <- (max(rms) - rms)^2
      
      ## get quantile threshold below which to reject 
      q_rej <- quantile(x = i_rms, probs = 1 - reject, na.rm = TRUE)
        
      ## isolate stretched master correlations below rejection threshold
      i_ok <- seq(from = 1, to = length(i_rms))[i_rms >= q_rej]
        
      ## return output
      return(list(i_ok = i_ok,
                  r_all = i_rms,
                  r_max = max(i_rms, na.rm = TRUE)))
      
      }, stretches, reject)
  }
  
  ## extract goodness of fit values and build to matrix
  D_r <- do.call(cbind, lapply(X = D, FUN = function(x) {x$r_all}))
  
  ## extract dvv indices of appropriate goodness of fit values 
  i_ok <- lapply(X = D, FUN = function(x, min) {
  
    r_i <- ifelse(test = x$r_max >= min, yes = x$i_ok, no = NA)
  }, min)
  
  ## calculate average best fit dvv values
  D_mean <- do.call(c, lapply(X = i_ok, FUN = function(i_ok, dvv) {
    mean(dvv[i_ok], na.rm = TRUE)
  }, dvv))
  
  ## flip output to get correct velocity change direction
  D_mean <- -D_mean
  D_r <- D_r[nrow(D_r):1,]
    
  ## build output object
  DVV <- list(t = data$CC$t, 
              dvv = as.numeric(D_mean),
              range = dvv,
              gof = D_r)
  
  ## optionally rebuild eseis object
  if(eseis == TRUE) {
    
    ## assign cross correlation data set
    eseis_data$DVV <- DVV
    eseis_data <- eseis_data[c(length(eseis_data), 1:(length(eseis_data)-1))]
    
    ## calculate function call duration
    eseis_duration <- as.numeric(difftime(time1 = Sys.time(), 
                                          time2 = eseis_t_0, 
                                          units = "secs"))
    
    ## update object history
    eseis_data$history[[length(eseis_data$history) + 1]] <- 
      list(time = Sys.time(),
           call = "ncc_stretch()",
           arguments = par,
           duration = eseis_duration)
    names(eseis_data$history)[length(eseis_data$history)] <- 
      as.character(length(eseis_data$history))
    
    ## update data type
    eseis_data$meta$type = "velocity"
    
    ## set S3 class name
    class(eseis_data)[1] <- "eseis"
    
    ## assign eseis object to output data set
    DVV <- eseis_data
  }
  
  ## return output
  return(DVV)
}
