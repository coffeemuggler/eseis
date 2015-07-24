convert.time <-
structure(function(# Function to convert time units.
  ### This function converts time units.

  data,
  ### \code{List} object or \code{numeric} vector, data set to be processed.
  
  unit.in,
  ### \code{Character} scalar, input date format, one out of 
  ### \code{"JD"} (Julian Day), \code{"day"}, \code{"hour"}, 
  ### \code{"min"}, \code{"sec"}.
  
  unit.out
  ### \code{Character} scalar, output date format, one out of 
  ### \code{"day"} (day), \code{"hour"}, \code{"min"}, \code{"sec"}.
){
  ## create target variable
  if(is(data, "list") == TRUE & is.numeric(data[[1]][[1]]) == TRUE) {
    t.in <- data$time$signal
  } else {
    t.in <- data
  }
  
  ## convert input format to seconds
  if(unit.in == "JD") {
    t.sec <- t.in * 86400
  } else if(unit.in == "day") {
    t.sec <- t.in * 86400
  } else if(unit.in == "hour") {
    t.sec <- t.in * 3600
  } else if(unit.in == "min") {
    t.sec <- t.in * 60
  }  else if(unit.in == "sec") {
    t.sec <- t.in
  }
  
  ## convert time to day
  if(unit.out == "day") {
    t.out <- t.sec / 86400
  } else if(unit.out == "hour") {
    t.out <- t.sec / 3600
  } else if(unit.out == "min") {
    t.out <- t.sec / 60
  }  else if(unit.out == "sec") {
    t.out <- t.sec
  }
  
  ## asign converted time to object
  if(is(data, "list") == TRUE & is.numeric(data[[1]][[1]]) == TRUE) {
    data$time$signal <- t.out
    data$summary$unit <- unit.out
  } else {
    data <- t.out
  }
  
  return(data)
  ### \code{Numeric} vector of time units.
  
  ##details<<
  ## This function primarilty aids plotting purposes. to display signal
  ## traces in appropriate units (default would be seconds) the time units
  ## must be converted.
  
  ##seealso<<
  ## \code{\link{read.sac}}, \code{\link{strip.time}},\code{\link{create.ts}}
  
  ##keyword<<
  ## environmental seismology
  
}, ex = function() {
  ## load example data
  data(signal.1, envir = environment())
  
  ## show time unit of example data set
  signal.1$summary$unit
  
  ## convert hours to minutes
  signal.1.min <- convert.time(data = signal.1, 
                               unit.in = signal.1$summary$unit,
                               unit.out = "min")
  
  ## convert only time vector
  time.vector.hour <- signal.1$time$signal
  time.vector.min <- convert.time(data = time.vector.hour,
                                  unit.in = "hour",
                                  unit.out = "min")  
  
  ## show summaries of the two data sets
  summary(time.vector.hour)
  summary(time.vector.min)
  
  ## Nota bene, convert date from d-m-Y to Julian day
  date <- "10.10.2010"
  strptime(x = date, format = "%d.%d.%Y")$yday + 1
})
