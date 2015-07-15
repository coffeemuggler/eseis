strip.time <-
structure(function(# Function to create time units from sac-files.
  ### The function strips the time vector from sac objects.
  data,
  ### object, data set to be processed. 

  unit = "JD",
  ### \code{Character} scalar, date format of the output, one out of 
  ### \code{"JD"} (Julian Day), \code{"day"}, \code{"hour"}, \code{"min"}, 
  ### \code{"sec"}.

  shift = 0
  ### \code{Numeric} scalar, time shift [sec].
){
  ## create target variable
  t <- numeric(0)
  
  ## loop through all data sets
  for(i in 1:length(data)) {
    ## create time vector
    dt <- data[[i]]$dt
    t.0 <- ifelse(length(t) == 0, 0, max(t) + dt)
    t <- c(t, shift + seq(from = t.0 / dt, to = i * data[[i]]$N - 1) * dt)
  }  
  
  ## convert time to user-defined format
  if(unit == "JD") {
    shift <- data[[1]]$DATTIM$jd + 
      data[[1]]$DATTIM$hr / 24 + 
      data[[1]]$DATTIM$mi / 1440 +
      data[[1]]$DATTIM$sec / 86400
    t <- shift + seq(from = min(t),
                     to = max(t),
                     by = dt) / 86400
  } else if(unit == "day") {
    shift <- data[[1]]$DATTIM$hr / 24 + 
      data[[1]]$DATTIM$mi / 1440 +
      data[[1]]$DATTIM$sec / 86400
    t <- shift + seq(from = min(t),
                     to = max(t),
                     by = dt) / 86400
  } else if(unit == "hour") {
    shift <- data[[1]]$DATTIM$hr + 
      data[[1]]$DATTIM$mi / 60 +
      data[[1]]$DATTIM$sec / 3600
    t <- shift + seq(from = min(t),
                     to = max(t),
                     by = dt) / 3600
  } else if(unit == "min") {
    shift <-data[[1]]$DATTIM$mi +
      data[[1]]$DATTIM$sec / 60
    t <- shift + seq(from = min(t),
                     to = max(t),
                     by = dt) / 60
  }  else if(unit == "sec") {
    shift <- data[[1]]$DATTIM$sec
    t <- shift + seq(from = min(t),
                     to = max(t),
                     by = dt)
  }
  
  return(t)
  ### A vector of time units.
  
  ##seealso<<
  ## \code{\link{read.sac}}
  
  ##keyword<<
  ## environmental seismology
}, ex = function(){
  ## load example data
  data(sac, envir = environment())
  
  ## create time vector from data array
  t <- strip.time(data = sac, unit = "hour")
  
  ## check results (in minutes), should be 10 minutes
  summary(t * 60)
})
