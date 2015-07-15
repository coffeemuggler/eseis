merge.array <-
structure(function(# Function to merge data arrays.
  ### This function merges data arrays. Merging can include the
  ### temporal dimension (time).
  data
  ### \code{List} object, data sets to be processed (e.g. 
  ### \code{list(array.1, array.2)}).
){
  ## create output data set
  data.out <- data[[1]]
  
  ## merge summary part
  
  ## merge time part
  if(length(data[[1]]$time) > 0) {
    for(j in 1:length(data[[1]]$time)) {
      values.merged <- data[[1]]$time[[j]]
      for(i in 2:length(data)) {
        values.merged <- c(values.merged, data[[i]]$time[[j]])
      }
      i <- 2
      data.out$time[[j]] <- values.merged
    }  
  }
  
  ## merge signal part
  if(length(data[[1]]$signal) > 0) {
    for(k in 1:length(data[[1]]$signal)) {
      for(j in 1:length(data[[1]]$signal[[1]])) {
        values.merged <- data[[1]]$signal[[k]][[j]]
        for(i in 2:length(data)) {
          values.merged <- c(values.merged, data[[i]]$signal[[k]][[j]])
        }
        i <- 2
        data.out$signal[[k]][[j]] <- values.merged
      }
      j <- 1
    }
  }
  
  ## merge spectrum part
  if(length(data[[1]]$spectrum) > 0) {
    for(k in 1:length(data[[1]]$spectrum)) {
      for(j in 1:length(data[[1]]$spectrum[[1]])) {
        values.merged <- data[[1]]$spectrum[[k]][[j]]
        for(i in 2:length(data)) {
          if(length(data[[i]]$spectrum[[k]][[j]]) > 0) {
            values.merged <- rbind(values.merged, data[[i]]$spectrum[[k]][[j]])
          }
        }
        i <- 2
        data.out$spectrum[[k]][[j]] <- values.merged
      }
      j <- 1
    }
  }
  
  ## merge event part
  
  return(data.out)
  ### A list object with the merged data set.
  
  ##references<<
  
  ##keyword<<
  ## environmental seismology
  
}, ex = function(){
  ## load example data set
  data(signal.1, envir = environment())
  
  ## clip original data set
  signal.1.1 <- clip.array(data = signal.1, 
                           time.clip = c(11, 11 + 1/12))
  signal.1.2 <- clip.array(data = signal.1, 
                           time.clip = c(11 + 1/12, 11 + 2/12))
  
  ## merge data set
  signal.1.merged <- merge.array(data = list(signal.1.1, signal.1.2))
})
