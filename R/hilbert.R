hilbert <-
structure(function(# Function to calculate the hilbert transform
  ### This function calculates the hilbert transform of signals.
  
  data
  ### Numeric \code{matrix} with seismic traces.
){
  
  ## check/set data structure
  if(is.matrix(data) == FALSE) {
    data <- rbind(data)
  }

  ## create hilbert vector
  h <- numeric(length = ncol(data))
  
  ## ceck odd vs. even data length
  if(ncol(data) %% 2 == 0) {
    h[c(1, ncol(data) / 2 + 1)] <- 1
    h[2:(ncol(data) / 2)] <- 2
  }
  else {
    h[1] <- 1
    h[2:((ncol(data) + 1) / 2)] <- 2
  }
  
  ## create output data set
  data.out <- data
  
  ## loop through all signal vectors
  for(i in 1:nrow(data)) {
    data.out[i,] <- fft(h * fft(data[i,]), inverse = TRUE) / ncol(data)    
  }

  ## return hilbert data set
  return(invisible(data.out))
  ### A list object with the merged data set.
  
  ##references<< Code is modifed from R-package hht.
  
  ##keyword<<
  ## environmental seismology
  
}, ex = function(){
  ## load example data set
  data(signal.1, envir = environment())
  
  ## extract 3 traces of station 1 to matrix
  s.1 <- rbind(signal.1$signal$station1$x,
               signal.1$signal$station1$y,
               signal.1$signal$station1$z)
  
  ## calculate hilbert transform of the signals
  h.1 <- hilbert(data = s.1[1,])
})
