#' Convert time string to Julian Day
#' 
#' This function converts a POSIXct-like date into the corresponding Julian 
#' Day number and returns it as string format.
#' 
#' There is also a more powerful function to convert between different time 
#' formats, see \code{time_convert} for details.
#' 
#' @param time \code{Character} value, date to convert to Julian Day. If 
#' no POSIXct format is provided, the function will try to convert the 
#' input data to POSIXct, assuming the time zone is UTC.
#' 
#' @return \code{Character} value, Julian Day corresponding to the input 
#' date.
#'
#' @author Michael Dietze
#' 
#' @keywords eseis
#' 
#' @examples
#' 
#' time_jd(time = "2020-05-01")
#' 
#' @export time_jd

time_jd <- function(
    
  time

) {
  
  ## check if input format is POSIXct
  if(inherits(x = time, what = "POSIXct") == FALSE) {
    
    ## try to convert to POSIXct
    time <- try(as.POSIXct(x = time, tz = "UTC"))
  }
  
  ## convert date to Julian Day
  jd <- format(x = time, format = "%j")
  
  ## return output
  return(jd)
}