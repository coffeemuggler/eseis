#' Migrate signals of a seismic event through a grid of locations.
#'
#' The function performs signal migration in space in order to determine 
#' the location of a seismic signal.
#'
#' @param data \code{Numeric} matrix or \code{eseis} object, seismic signals 
#' to cross-correlate.
#' 
#' @param d_stations \code{Numeric} matrix, inter-station distances. Output 
#' of \code{distance_stations}.
#' 
#' @param d_map \code{List} object, distance maps for each station (i.e., 
#' \code{SpatialGridDataFrame} objects). Output of \code{distance_map}.
#' 
#' @param v \code{Numeric} value, mean velocity of seismic waves (m/s).
#' 
#' @param dt \code{Numeric} value, sampling period.
#' 
#' @param snr \code{Numeric} vector, optional signal-to-noise-ratios for 
#' each signal trace, used for normalisation. If omitted it is calculated
#' from input signals.
#' 
#' @param normalise \code{Logical} value, option to normalise stations 
#' correlations by signal-to-noise-ratios.
#' 
#' @return A SpatialGridDataFrame-object with Gaussian probability density
#' function values for each grid cell.
#' 
#' @author Michael Dietze
#' 
#' @examples
#'
#' \dontrun{
#' 
#' ## create synthetic DEM
#' dem <- raster::raster(nrows = 20, ncols = 20, 
#'                       xmn = 0, xmx = 10000, 
#'                       ymn= 0, ymx = 10000, 
#'                       vals = rep(0, 400))
#' 
#' ## define station coordinates
#' sta <- data.frame(x = c(1000, 9000, 5000),
#'                   y = c(1000, 1000, 9000),
#'                   ID = c("A", "B", "C"))
#' 
#' ## create synthetic signal (source in the center of the DEM)
#' s <- rbind(dnorm(x = 1:1000, mean = 400, sd = 50),
#'            dnorm(x = 1:1000, mean = 400, sd = 50),
#'            dnorm(x = 1:1000, mean = 400, sd = 50))
#' 
#' ## plot DEM and stations
#' raster::plot(dem)
#' 
#' text(x = sta$x, 
#'      y = sta$y, 
#'      labels = sta$ID)
#' 
#' ## calculate spatial distance maps and inter-station distances
#' D <- eseis::spatial_distance(stations = sta[,1:2],
#'                              dem = dem)
#' 
#' ## locate signal
#' e <- eseis::spatial_migrate(data = s, 
#'                             d_stations = D$stations, 
#'                             d_map = D$maps, 
#'                             v = 1000, 
#'                             dt = 1/10)
#' 
#' ## get most likely location coordinates
#' xy <- sp::coordinates(e)[raster::values(e) == max(raster::values(e))]
#' 
#' ## plot location estimate, most likely location estimate and stations
#' raster::plot(e)
#' points(xy[1], 
#'        xy[2],
#'        pch = 20)
#' points(sta[,1:2])
#' 
#' }
#'
#' @export spatial_migrate
spatial_migrate <- function(
  data,
  d_stations,
  d_map,
  snr,
  v,
  dt,
  normalise = TRUE
) {
  
  ## check/set data structure
  if(is.matrix(data) == FALSE) {
    
    if(class(data) == "list") {
      
      ## extract sampling period from first eseis object
      dt <- try(data[[1]]$meta$dt)
      
      ## check if dt can be extracted, otherwise stop function
      if(class(dt) == "try-error") {
        
        stop("signal object seems to contain no eseis objects!")
      }
      
      ## strip and organise signal vectors in matrix
      
      data <- do.call(rbind, lapply(X = data, FUN = function(data) {
        
        data$signal
      }))
    } else {
      
      stop("Input signals must be more than one!")
    }
  }
  
  if(is.matrix(d_stations) == FALSE) {
    stop("Station distance matrix must be symmetric matrix!")
  }
  
  if(nrow(d_stations) != ncol(d_stations)) {
    stop("Station distance matrix must be symmetric matrix!")
  }
  
  if(is.list(d_map) == FALSE) {
    stop("Distance maps must be list objects with SpatialGridDataFrames!")
  }
  
  if(class(d_map[[1]]) != "SpatialGridDataFrame") {
    stop("Distance maps must be list objects with SpatialGridDataFrames!")
  }
  
  if(normalise == TRUE & missing(snr) == TRUE) {
    
    print("No snr given. Will be calculated from signals")
    
    snr <- apply(X = data, MARGIN = 1, FUN = max, na.rm = TRUE) / 
      apply(X = data, MARGIN = 1, FUN = mean, na.rm = TRUE)
  }
  
  if(normalise == TRUE & length(snr) != nrow(data)) {
    stop("Number of snr values does not match number of signals!")
  }
  
  ## normalise input signals
  s_min <- matrixStats::rowMins(data, na.rm = TRUE)
  s_max <- matrixStats::rowMaxs(data, na.rm = TRUE)

  data <- (data - s_min) / (s_max - s_min)
  
  ## calculate signal duration
  duration <- ncol(data) * dt
  
  ## create output map
  map <- round(x = raster::raster(d_map[[1]]) - 
                 raster::raster(d_map[[1]]), digits = 0)
  
  ## create counter variable
  n_count <- 1
  
  ## perform the cross-correlation
  for(i in 1:(nrow(data) - 1)) {
    
    for(j in (i + 1):nrow(data)) {
      
      if(sum(is.na(data[i,])) == 0 & sum(is.na(data[j,])) == 0) {
        
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
        if(normalise == TRUE) {
          
          norm <- (max(data[i,], na.rm = TRUE) / mean(data[i,], na.rm = TRUE) + 
                     max(data[j,], na.rm = TRUE) / mean(data[j,], na.rm = TRUE)) / 
            (mean(max(data, na.rm = TRUE) / apply(X = data, 
                                                  MARGIN = 1, 
                                                  FUN = mean, na.rm = TRUE), na.rm = TRUE))
        } else {
          
          norm <- 1
        }
        
        ## calculate minimum and maximum possible lag times
        lag.min <- which.max(diff(lags >= -d_stations[i,j]/v))
        lag.max <- which.min(diff(lags <= d_stations[i,j]/v))
        
        ## clip correlation vector to lag ranges
        c.temp <- c[lag.min:lag.max]
        
        ## calculate lag times
        t.temp <- lags[lag.min:lag.max]
        
        ## get correlation values and lag times for maxima
        t.max <- t.temp[c.temp == max(c.temp)]
        
        ## calculate modelled and emprirical lag times
        dt.model <- (raster::raster(d_map[[i]]) - 
                       raster::raster(d_map[[j]])) / v
        dt.empiric <-  d_stations[i,j] / v
        
        ## calculate PDF for each pixel
        c.map <- exp(-0.5 * (((dt.model - t.max) / dt.empiric)^2)) * norm
        
        ## add PDF to original grid
        map <- map + c.map
        
        ## update counter
        n_count <- n_count + 1
      } else {
        
        ## add PDF to original grid
        map <- map
        
        ## update counter
        n_count <- n_count + 1      }

    }
    j = 1
  }
  
  ## correct output by number of station correlations
  map <- map / (n_count - 1)
  
  ## divide output by two
  map <- map / 2
  
  return(map)
}