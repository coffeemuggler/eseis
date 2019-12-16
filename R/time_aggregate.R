#' Aggregate a time series
#' 
#' The time series \code{x} is aggregated by an integer factor \code{n}.
#' 
#' @param data \code{POSIXct} vector, time to be processed.
#' 
#' @param n \code{Numeric} value, number of samples to be aggregated to one
#' new data value. Must be an integer value greater than 1. Default is 
#' \code{2}.
#' 
#' @return \code{POSIXct} vector, aggregated data.
#' 
#' @author Michael Dietze
#' 
#' @keywords eseis
#' 
#' @examples
#' 
#' ## load example data set
#' data(rockfall)
#' 
#' ## aggregate time series
#' rockfall_t_agg <- time_aggregate(data = rockfall_t, 
#'                           n = 2)
#' 
#' ## compare results
#' range(rockfall_t)
#' diff(rockfall_t)
#' 
#' range(rockfall_t_agg)
#' diff(rockfall_t_agg)
#' 
#' @export time_aggregate
time_aggregate <- function(
  data,
  n = 2
) {
  
  ## check aggregation factor
  if(signif(n) != n) {
    
    n <- round(x = n, 
               digits = 0)
    warning("Aggregation factor rounded to integer value!")
  }
  
  if(n < 1) {
    
    n <- 1
    warning("Aggregation factor smaller than 1, set to 1 automatically!")
  }
  
  ## check input data format
  if(class(x = data)[1] == "POSIXct") {
    
    if(n %% 2 == 0) {
      
      ## resample input data set
      data_agg <- data[seq(from = 1, 
                           to = length(data), 
                           by = n)]
      
      ## calculate mean input data difference
      d_data_1 <- mean(x = diff(x = data), 
                       na.rm = TRUE)
      
      ## calculate mean aggregated data difference
      d_data_2 <- mean(x = diff(x = data_agg), 
                       na.rm = TRUE)
      
      ## shift aggregated values to center of original values
      data_agg <- data_agg - d_data_1 / 2 + d_data_2 / 2
    } else {
      
      data_agg <- data[seq(from = ceiling(n / 2), 
                           to = length(data), 
                           by = n)]
    }
  } else {
    
    data_agg <- data
    warning("No POSIXct object submitted! No changes applied.")
  }
  
  ## return output
  return(data_agg)
}