aggregate.signal <-
structure(function(# Function to aggreagte time series.
  ### This function returns a resampled time series. 
  
  x,
  ### \code{Numeric} vector or matrix, data set to be processed. Can 
  ### either be a seismic signal or a time vector (in the case of time
  ### only a vector is supported).

  n = 2
  ### \code{Numeric} scalar, number of samples to be aggregated to one 
  ### new sample.
  
){
  
  ## check input data format
  if(class(x = x)[1] == "POSIXct") {
    
    if(n %% 2 == 0) {
      
      ## resample input data set
      x.agg <- x[seq(from = 1, to = length(x), by = n)]
      
      ## calculate mean input data difference
      dx.1 <- mean(x = diff(x = x), na.rm = TRUE)
      
      ## calculate mean aggregated data difference
      dx.2 <- mean(x = diff(x = x.agg), na.rm = TRUE)
      
      ## shift aggregated values to center of original values
      x.agg <- x.agg - dx.1 / 2 + dx.2 / 2
    } else {
      
      x.agg <- x[seq(from = ceiling(n / 2), to = length(x), by = n)]
    }
  } else {
    
    ## check/set data structure
    if(is.matrix(x) == FALSE) {
      x <- rbind(x)
    }
    
    ## aggregate data
    if(n %% 2 == 0) {
      
      ## resample input data set
      x.agg <- x[,seq(from = 1, to = ncol(x), by = n)]
      
      ## check/restore data structure
      if(is.matrix(x.agg) == FALSE) {
        x.agg <- t(as.matrix(x.agg))
      }
      
      ## calculate mean input data difference
      dx.1 <- mean(x = apply(X = x, 
                             MARGIN = 1, 
                             FUN = diff), 
                   na.rm = TRUE)
      
      ## calculate mean aggregated data difference
      dx.2 <- mean(x = apply(X = x.agg, 
                             MARGIN = 1, 
                             FUN = diff), 
                   na.rm = TRUE)
      
      ## shift aggregated values to center of original values
      x.agg <- x.agg - dx.1 / 2 + dx.2 / 2
      
    } else {
      
      x.agg <- x[,seq(from = ceiling(n / 2), to = ncol(x), by = n)]
    }
  }
  

  
  ## optionally create extended output
  return(x.agg)
  ### Numeric vector or matrix with aggregated time series.
  
  ##keyword<<
  ## environmental seismology
  
}, ex = function(){

  ## create example data set
  x <- 1:10
  
  ## aggregate x by factor 2
  x.agg.2 <- aggregate.signal(x = x, 
                              n = 2)
  
  ## aggregate x by factor 3
  x.agg.3 <- aggregate.signal(x = x, 
                              n = 3)
  
  ## show results graphically
  plot(x = x, 
       y = rep(x = 1, times = length(x)), 
       ylim = c(1, 3))
  
  points(x = x.agg.2, 
         y = rep(x = 2, times = length(x.agg.2)), 
         col = 2)
  
  points(x = x.agg.3, 
         y = rep(x = 3, times = length(x.agg.3)), 
         col = 3)

  abline(v = x.agg.2, 
         col = 2)
  
  abline(v = x.agg.3, 
         col = 3)
  
  ## create a POSIXct time series
  t <- as.POSIXct(x = strptime(x = 1:50, format = "%s"), tz = "UTC")
  
  ## aggregate time series
  t.agg <- aggregate.signal(x = t, n = 2)
  
  ## compare results
  range(t)
  diff(t)
  
  range(t.agg)
  diff(t.agg)
  
  ## create signal matrix
  X <- rbind(1:100, 1001:1100, 10001:10100)
  
  ## aggregate signal matrix by factor 4
  X.agg <- aggregate.signal(x = X, n = 4)
   
})
