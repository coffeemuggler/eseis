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
#' @param \dots Further arguments passed to the function.
#' 
#' @return A \code{Numeric} vector, relative velocity changes for input 
#' time windows.
#' 
#' @author Michael Dietze
#' 
#' @keywords eseis
#' 
#' @examples
#' 
#' ## preprocess data and calculate window-wise cross-correlation functions
#'                      
#' @export aux_nccprocess
aux_nccprocess <- function(
  data,
  master = "mean",
  normalise = TRUE,
  sides = "both",
  range = 0.01,
  steps = 100,
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
  if(sum(sides == c("mean", "median", "quantile")) < 1) {
    
    "Keyword for master not supported!"
  }
  
  ## check keywords for sides argument
  if(sum(sides == c("both", "left", "right", "single")) < 1) {
    
    "Keyword for sides not supported!"
  }
  
  ## extract additional arguments
  args <- list(...)

  ## generate master trace ----------------------------------------------------
  
  ## calculate normalised correlation data set
  if(normalise == TRUE) {
    
    data_norm <-  2 * t(apply(X = data$correlation, 
                         MARGIN = 1, 
                         FUN = function(data) {
      
      (data - min(data)) / (max(data) - min(data))
    })) - 1
  } else {
    
    data_norm <- data$correlation
  }
  
  if(master == "mean") {
    
    data_master <- colMeans(data_norm)
  } else if(master == "median") {
    
    data_master <- apply(X = data_norm, 
                         MARGIN = 2,
                         FUN = quantile, 
                         probs = 0.5)
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
                         probs = quantile_probs)
  }
  
  ## generate stretch vector
  stretchs <- seq(from = -range, 
                  to = range, 
                  length.out = steps)
  
  ## generate stretching factors for time lag vector
  k <- exp(-stretchs)
  
  ## taper reference trace and extend it to avoid interploation artefacts
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
                    to = length(data_master_ext)) - length(data_master_ext) / 2
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
  data_master_stretch <- do.call(rbind, data_master_stretch)
  
  ## remove extended parts
  data_master_stretch <- data_master_stretch[,-(1:n_add)]
  data_master_stretch <- data_master_stretch[,1:ncol(data_norm)]
  
  ## convert input data row-wise to list
  data_norm_list <- as.list(as.data.frame(t(data_norm)))
  
  ## calculare RMS differences between stretched master traces and data
  delta <- lapply(X = data_norm_list, FUN = function(x, data_master_stretch) {
    
    diff <- apply(X = t(data_master_stretch), 
          MARGIN = 2, 
          FUN = function(data_master_stretch) {
            
            sqrt(mean((data_master_stretch - x)^2))
            
          })
    
    i_min <- seq(from = 1, to = length(diff))[diff == min(diff)]
    
    return(i_min)
  }, data_master_stretch)
  
  ## convert list to vector
  delta <- as.numeric(do.call(c, delta))
  
  ## assign dv vector values
  dv <- stretchs[delta]
  
  ## return output
  return(dv)
}
