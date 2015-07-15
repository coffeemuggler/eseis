create.ts <-
structure(function(# Function to create time series from sac-files.
  ### This function converts a sac-object, either into a data frame or a 
  ### zoo-object, thereby only keeping the signal and, optionally, time part.
  
  data,
  ### \code{list} object, data set to be converted.
  
  unit = "JD",
  ### \code{Character} scalar, date format of the output, one out of 
  ### \code{"JD"} (Julian Day), \code{"hour"}, \code{"min"}, \code{"sec"}.
  
  shift = 0,
  ### Numeric \code{scalar}, time shift [sec].
  
  object = "data.frame",
  ### \code{Character} scalar, object type to be created, one out of
  ### \code{"data.frame"} (data frame) or a \code{"zoo"} (zoo object).Each 
  ### object will be build by two vectors: \code{signal} and \code{time}.
  
  keep.time = TRUE
  ### \code{Logical} scalar, option to return time vector.

  ){

  ## check if data set is list object
  if(is.list(data) == FALSE) {stop("The input data is not no list structure!")}
  
  ## create target variables
  t <- strip.time(data = data, unit = unit, shift = shift)
  amp <- numeric(0)
  
  ## create one merged data set (t, amp)
  for(i in 1:length(data)) {
    ## create time vector
    dt <- data[[i]]$dt
    amp <- c(amp, data[[i]]$amp)
  }
  
  if(object == "data.frame") {
    if(keep.time == TRUE) {
      data <- data.frame(time = t, signal = amp)      
    } else {
      data <- data.frame(signal = amp)
    }
  } else if(object == "zoo") {
    data <- zoo(x = amp, order.by = t)
  }
  
  return(data)
  ### either a \code{data frame} or a \code{zoo} object
   
  ##seealso<<
  ## \code{\link{read.sac}}, \code{\link{zoo}}
  
  ##details<<
  ## Although the funciton name may imply that a \code{ts} object (time
  ## series object) is returned, this is not the case. A \code{ts} object
  ## only allows definition of a start year and quarter, which is not
  ## appropriate for such high resolution data as seismic data.
  
}, ex = function(){
  ## load example data set
  data(sac, envir = environment())
  
  ## create data frame object with Julian Day time stamp
  t.JD <- create.ts(data = sac)
  
  ## create data frame object with Julian Day time stamp
  t.JD.signal <- create.ts(data = sac, keep.time = FALSE)

  ## create zoo object with hourly time stamp
  t.hour <- create.ts(data = sac, unit = "hour", object = "zoo")
})
