migrate <-
structure(function(# Function to migrate signals in space
  ### The function performs signal migration in space in order to determine 
  ### the location of a seismic signal.
  
  data,
  ### Numeric \code{matrix}, seismic signals to cross-correlate.
  
  d.stations,
  ### Numeric \code{matrix}, inter-station distances.
  
  d.map,
  ### \code{List} object, distance maps for each station.
  
  v,
  ### Numeric \code{scalar}, mean velocity of seismic waves in the medium,
  ### in m/s.
  
  dt,
  ### Numeric \code{scalar}, sampling period
  
  snr,
  ### Numeric \code{vector}, optional signal-to-noise-ratios for each signal 
  ### trace, used for normalisation.
  
  normalisation = FALSE
  ### Logical \code{scalar}, option to normalise stations correlations by 
  ### signal-to-noise-ratios. If used, snr data must be provided.
  
){

  ## check/set data structure
  if(is.matrix(data) == FALSE) {
    stop("Input signals must be more than one!")
  }
  
  if(is.matrix(d.stations) == FALSE) {
    stop("Station distance matrix must be symmetric matrix!")
  }

  if(nrow(d.stations) != ncol(d.stations)) {
    stop("Station distance matrix must be symmetric matrix!")
  }
  
  if(is.list(d.map) == FALSE) {
    stop("Distance maps must be list objects with SpatialGridDataFrames!")
  }

  if(class(d.map[[1]]) != "SpatialGridDataFrame") {
    stop("Distance maps must be list objects with SpatialGridDataFrames!")
  }
  
  if(normalisation == TRUE & missing(snr) == TRUE) {
    stop("Normalisation cannot be done without snr data!")
  }
  
  if(normalisation == TRUE & length(snr) != nrow(data)) {
    stop("Number of snr values does not match numebr of signals!")
  }
  
  ## calculate signal duration
  duration <- ncol(data) * dt
  
  ## create output map
  map <- round(x = raster::raster(d.map[[1]]) - 
                 raster::raster(d.map[[1]]), digits = 0)
  
  ## create counter variable
  n.count <- 1
  
  ## perform the cross-correlation
  for(i in 1:(nrow(data) - 1)) {
    
    for(j in (i + 1):nrow(data)) {
      
      ## calculate cross-correlation
      cc <- ccf(x = data[i,], 
                y = data[j,], 
                lag.max = duration * 200,
                plot = FALSE)
      
      ## assign lag times
      lags <- cc$lag * dt

      ## assign correlation values
      c <- cc$acf
      
      ## calculate SNR normalisation factor
      if(normalisation == TRUE) {
        
        norm <- (max(data[i,]) / mean(data[i,]) + 
                   max(data[j,]) / mean(data[j,])) / 
          (mean(max(data) / apply(X = data, MARGIN = 1, FUN = mean)))
      } else {
        
        norm <- 1
      }
      
      ## calculate minimum and maximum possible lag times
      lag.min <- which.max(diff(lags >= -d.stations[i,j]/v))
      lag.max <- which.min(diff(lags <= d.stations[i,j]/v))
      
      ## clip correlation vector to lag ranges
      c.temp <- c[lag.min:lag.max]
      
      ## calculate lag times
      t.temp <- lags[lag.min:lag.max]
      
      ## get correlation values and lag times for maxima
      t.max <- t.temp[c.temp == max(c.temp)]
      
      ## calculate modelled and emprirical lag times
      dt.model <- (raster::raster(d.map[[i]]) - 
                     raster::raster(d.map[[j]])) / v
      dt.empiric <-  d.stations[i,j] / v
      
      ## calculate PDF for each pixel
      c.map <- exp(-0.5 * (((dt.model - t.max) / dt.empiric)^2)) * norm
      
      ## add PDF to original grid
      map <- map + c.map
      
      ## update counter
      n.count <- n.count + 1
    }
    j = 1
  }
  
  ## correct output by number of station correlations
  map <- map / (n.count - 1)

  return(map)
  ### A SpatialGridDataFrame-object with Gaussian probability density 
  ### function values for each grid cell.
  
  ##keyword<<
  ## environmental seismology
}, ex = function(){
  ## no example yet
  
})
