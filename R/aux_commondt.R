#' Identify highest common sampling interval
#' 
#' The function compares the sampling intervals of a list of \code{eseis}
#' objects and identifies the highest common sampling interval (dt) as well 
#' as the aggregation factors for each \code{eseis} object needed to reach 
#' this common sampling interval.
#' 
#' @param data \code{list} of \code{eseis} objects or vector of sampling 
#' intervals to be checked for highest common sampling interval
#' 
#' @param dt \code{Numeric} vector of length one, user-defined common 
#' sampling frequency for which aggregation factors shall be computed.
#' 
#' @return \code{list} object with elements \code{dt} (highest common 
#' sampling interval) and \code{agg} (aggregation factors for each 
#' of the input data sets to reach the common sampling interval)
#' 
#' @author Michael Dietze
#' 
#' @keywords eseis
#' 
#' @examples
#' 
#' \dontrun{
#' 
#' ## TO BE WRITTEN
#' }
#'                      
#' @export aux_commondt
aux_commondt <- function(
  data,
  dt
) {
  
  ## check/set input data
  if(class(data)[1] == "list") {
    
    ## get classes of list elements
    list_classes <- do.call(c, lapply(X = data, FUN = function(data) {
      
      class(data)[1]
    }))
    
    ## check if list elements are eseis objects
    if(sum(list_classes == "eseis") != length(data)) {
      
      stop("Input data does not entirely consist of eseis objects!")
    } else {
      
      ## extract sampling intervals
      dt_data <- do.call(c, lapply(X = data, FUN = function(data) {
        
        data$meta$dt
      }))
    }
  } else if(class(data)[1] == "numeric") {
    
    dt_data <- data
  } else {
    
    stop("Input data must be either eseis object or vector of dt values!")
  }
  
  ## define function to find greatest common divisor
  gcd <- function (x) {
    
    gcdp <- function(x) {
      x_max <- max(x)
      x_min <- min(x)
      
      if (x_max == x_min) {
        
        return (x_max)
      } else {
        
        return(gcdp(c(x_min, x_max - x_min)))
      }  
    }
    
    g <- gcdp(x)

    if(length(x) > 2) {
      
      for (i in 3:length(x)) {
        
        g <- gcdp(c(g, x[i]))
        
        if (g == 1) {
          
          break
        } 
      }
    }
    
    return(g)
  }
  
  ## find common divisor
  if(missing(dt) == TRUE) {
    
    dt_common <- gcd(x = dt_data)
  } else {
    dt_common <- dt
  }
  
  ## find aggregation factors
  aggregation <- dt_common / dt_data
  
  ## generate output data set
  data_out <- list(dt = dt_common,
                   agg = aggregation)
  
  ## return output
  return(data_out)
}
