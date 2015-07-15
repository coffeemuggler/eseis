clip.array <-
structure(function(# Function to clip a data array.
  ### This function clips (or truncates) a data array. Clipping can include the
  ### temporal dimension (time) or the data structure (i.e. number of stations,
  ### number of components). Entire array components (e.g. \code{metadata},
  ### \code{events}, \code{signal}) can be removed as well.

  data,
  ### \code{List} object, array structure to be processed.
  
  time.clip,
  ### \code{numeric} vector, segment used for clipping the array. Interval 
  ### must refer to the signal time vector (not the spectrum time vector).
  
  summary
  ### \code{List} object, new structure of the summary component. Set to
  ### \code{NULL} to remove.
  
  #################### ADD ALL FURTHER PARAMETERS WHEN THERE IS TIME
){
  ## create output variable
  data.out <- data
  
  ## clip data set temporally
  if(missing(time.clip) == FALSE) {
    
    ## check/correct data consistency
    if(length(time.clip) != 2) {stop("Time interval not of length 2!")}
    if(time.clip[1] >= time.clip[2]) {stop("Time interval zero or negative!")}
    if(is.numeric(time.clip) == FALSE) {stop("Time interval not numeric!")}
    if(time.clip[1] < min(data$time$signal)) {
      warning("Lower time interval boundary lower than signal, adjusted!")
      time.clip[1] <- min(data$time$signal)}
    if(time.clip[2] > max(data$time$signal)) {
      warning("Upper time interval boundary higher than signal, adjusted!")
      time.clip[2] <- max(data$time$signal)}
    
    ## set clipping flags
    flag.signal <- rep(0, length(data$time$signal))
    flag.signal[data$time$signal >= time.clip[1] & 
                  data$time$signal <= time.clip[2]] <- 1
    
    flag.spectrum <- rep(0, length(data$time$spectrum))
    flag.spectrum[data$time$spectrum >= time.clip[1] & 
                    data$time$spectrum <= time.clip[2]] <- 1
    
    ## clip time component
    data.out$time$signal <- data$time$signal[flag.signal == 1]
    
    if(exists(x = "spectrum", where = data$time) == TRUE) {
      data.out$time$spectrum <- 
        data$time$spectrum[flag.spectrum == 1]
    }
    
    ## clip signal component
    for(i in 1:length(data$signal)) {
      for(j in 1:length(data$signal[[i]])) {
        data.out$signal[[i]][[j]] <- 
          data$signal[[i]][[j]][flag.signal == 1]
        data.out$signal[[i]][[j]] <- 
          data$signal[[i]][[j]][flag.signal == 1]
      }
      j <- 1
    }
    
    ## clip spectrum component
    for(i in 1:length(data$spectrum)) {
      for(j in 1:length(data$spectrum[[i]])) {
        data.out$spectrum[[i]][[j]] <- 
          data$spectrum[[i]][[j]][flag.spectrum == 1]
        data.out$spectrum[[i]][[j]] <- 
          data$spectrum[[i]][[j]][flag.spectrum == 1]
      }
      j <- 1
    }
  }
  
  ## update summary values
  data.out$summary$N <- length(data.out$time$signal)
  
  return(data.out)
  ### A list object with the clipped data set.
  
  ##seealso<<
  ## \code{\link{build.array}}
  
  ##keyword<<
  ## environmental seismology
  
}, ex = function(){
  ## load example data set
  data(signal.1, envir = environment())
  
  ## clip data set to first half of original length, i.e. first 5 minutes
  signal.1.clipped <- clip.array(data = signal.1, 
                                 time.clip = c(11, 11 + 1/12))
})
