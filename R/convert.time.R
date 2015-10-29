convert.time <-
structure(function(# Function to convert time units.
  ### This function converts time units.

  data,
  ### \code{numeric} vector with times to be processed.
  
  unit.in = "POSIXct",
  ### \code{Character} scalar, input date format, one out of 
  ### \code{"POSIXct"}, \code{"JD"} (Julian Day), \code{"day"}, \code{"hour"}, 
  ### \code{"min"}, \code{"sec"}. Default is \code{"POSIXct"}.
  
  unit.out = "sec",
  ### \code{Character} scalar, input date format, one out of 
  ### \code{"POSIXct"}, \code{"JD"} (Julian Day), \code{"day"}, \code{"hour"}, 
  ### \code{"min"}, \code{"sec"}. Default is \code{"sec"}.
  
  year
  ### \code{numeric} scalar, optional year of input data. only relevant if 
  ### non-POSIXct-data should be converted to POSIXct. If omitted, the current 
  ### year is used.
){

  ## homogenise inptu data
  if(is.atomic(data) == FALSE) {
    stop("Only vectors are supported!")
  }

  ## convert/assign input datato POSIXct
  if(unit.in != "POSIXct") {
    
    stop("Input data type not supported, yet. Sorry!")
    
  } else {
    data.POSIXct <- data
  }
  
  ## convert POSIXct data to output format
  
  if(unit.out == "sec") {
    data.out <- as.numeric(format(data.POSIXct, "%S"))
  } else if(unit.out == "min") {
    data.out <- as.numeric(format(data.POSIXct, "%M"))
  } else if(unit.out == "hour") {
    data.out <- as.numeric(format(data.POSIXct, "%H"))
  } else if(unit.out == "day") {
    data.out <- as.numeric(format(data.POSIXct, "%d"))
  }  else if(unit.out == "JD") {
    data.out <- as.numeric(format(data.POSIXct, "%j"))
  } else if(unit.out == "month") {
    data.out <- as.numeric(format(data.POSIXct, "%m"))
  } else if(unit.out == "year") {
    data.out <- as.numeric(format(data.POSIXct, "%Y"))
  } else {
    stop("Output unit not supported!")
  }

  ## return output
  return(data.out)
  ### \code{Numeric} vector of time units.
  
}, ex = function() {

  
})
