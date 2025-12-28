#' Convert Julian Day to Date and vice versa
#' 
#' The function converts a Julian Day value to a date, to \code{POSIXct} if a 
#' year is provided, otherwise to \code{POSIXlt}.
#' 
#' @param input \code{Numeric} vector, input time Supported formats are 
#' \code{YYYY-MM-DD}, \code{JD} and \code{POSIXct}.
#' 
#' @param output \code{Numeric} vector, output time. Supported formats are 
#' \code{YYYY-MM-DD}, \code{JD} and \code{POSIXct}.
#' 
#' @param timezone \code{Character} vector, time zone of the output date. 
#' Default is \code{"UTC"}.
#' 
#' @param year \code{Character} vector, year of the date. Only used when 
#' \code{input} is \code{JD}. If omitted, the current year is used.
#' 
#' @return \code{Numeric} vector, 
#' @author Michael Dietze
#' @keywords eseis
#' @examples
#' 
#' ## convert Julian Day 18 to POSIXct
#' time_convert(input = 18, output = "POSIXct")
#' 
#' ## convert Julian Day 18 to yyyy-mm-dd
#' time_convert(input = 18, output = "yyyy-mm-dd")
#' 
#' ## convert yyyy-mm-dd to Julian Day
#' time_convert(input = "2016-01-18", output = "JD")
#' 
#' ## convert a vector of Julian Days to yyyy-mm-dd
#' time_convert(input = 18:21, output = "yyyy-mm-dd")
#'                      
#' @export time_convert
time_convert <- function(
  input,
  output,
  timezone = "UTC",
  year
) {
  
  ## check/set output
  if(missing(output) == TRUE) {
    
    warning("No output format provided! No changes applied")
    
    return(input)
  }
  
  ## check input formats
  if(class(input)[1] == "POSIXct") {
    
    if(output == "JD") {
      
      ## extract Julian Day from POSIXct string
      time_out <- as.numeric(as.POSIXlt(input)$yday + 1)
    } else {
      
      ## extract date components from POSIXct string
      yr <- as.numeric(format(input, "%Y"))
      mo <- as.numeric(format(input, "%m"))
      dom <- as.numeric(format(input, "%d"))
      
      ## pad with zeros
      mo <- ifelse(nchar(mo) < 2, paste("0", mo, sep = ""), mo)
      dom <- ifelse(nchar(dom) < 2, paste("0", dom, sep = ""), dom)
      
      ## assign string to output data set
      time_out <- paste(yr, mo, dom, sep = "-")
    }
    
  } else if(nchar(input[1]) <= 3) {
    
    ## check/set year
    if(missing(year) == TRUE) {
      
      year <- as.numeric(format(Sys.Date(), "%Y"))
    }
    
    if(output == "POSIXct") {
      
      ## convert Julian Day to date
      input_date <- as.Date(x = input, 
                            origin = paste(year, "-01-01", sep = "")) - 1
      
      ## convert date to POSIXct format
      time_out <- as.POSIXct(input_date, tz = timezone)
    } else {
      
      ## convert Julian Day to date
      time_out <- as.Date(x = input, 
                          origin = paste(year, "-01-01", sep = "")) - 1
    }
    
  } else {
    
    ## check/set year
    if(missing(year) == TRUE) {
      
      year <- as.numeric(format(Sys.Date(), "%Y"))
    }
    
    if(output == "POSIXct") {
      
      ## convert yyyy-mm-dd to POSIXct
      time_out <- as.POSIXct(strptime(input, format = "%Y-%m-%d"), 
                             tz = timezone)
    } else {
      
      ## convert yyyy-mm-dd to Julian Day
      time_out <- as.numeric(as.POSIXlt(strptime(input, 
                                                 format = "%Y-%m-%d"), 
                                        tz = timezone)$yday + 1)
    }
  }
  
  ## return output
  return(time_out)
}

