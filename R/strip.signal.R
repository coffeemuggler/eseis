strip.signal <-
structure(function(# Function to strip the signal part from sac-files.
  ### This function converts a sac-object into a vector containing only the
  ### signal part.
  
  data
  ### \code{list} object, data set to be converted.
  
){
  
  ## check if data set is list object
  if(is.list(data) == FALSE) {stop("The input data is not no list structure!")}
  
  ## create target variable
  signal <- numeric(0)
  
  ## create one merged data set (t, amp)
  for(i in 1:length(data)) {
    signal <- c(signal, data[[i]]$amp)
  }
  
  return(signal)
  ### \code{Numeric} vector with signal part of the sac-object.
  
  ##seealso<<
  ## \code{\link{read.sac}}, \code{\link{strip.time}}
    
}, ex = function() {
  ## load example data set
  data(sac, envir = environment())
  
  ## create data frame object with Julian Day time stamp
  signal <- strip.signal(data = sac)
  
  ## plot signal
  plot.signal(data = signal)
  
})
