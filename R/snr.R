snr <-
structure(function(# Function to calculate signal-to-noise-ratios.
  ### The function returns the signal-to-noise-ratios (SNR) of input signals. 
  ### The SNR is defined as max(data) / mean(data).

  data
  ### Numeric \code{vector} or \code{matrix} with respective seismic traces.
  
){
  
  ## check/set data structure
  if(is.matrix(data) == FALSE) {
    data <- rbind(data)
  }
  
  ## calculate SNR
  snr <- apply(X = data, MARGIN = 1, FUN = max) /
    apply(X = data, MARGIN = 1, FUN = mean)
  
  return(snr)
  ### A vector of time units.

  
  ##keyword<<
  ## environmental seismology
}, ex = function(){
  ## no example yet
  
})
