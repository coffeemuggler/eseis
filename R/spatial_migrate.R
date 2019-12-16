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
#' s <- rbind(dnorm(x = 1:1000, mean = 500, sd = 50),
#'            dnorm(x = 1:1000, mean = 500, sd = 50),
#'            dnorm(x = 1:1000, mean = 500, sd = 50))
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
#'                             dt = 1/100)
#' 
#' ## get most likely location coordinates (example contains two equal points)
#' xy <- matrix(sp::coordinates(e)[raster::values(e) == max(raster::values(e))],
#'              ncol = 2)[1,]
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
    
    if(class(data)[1] == "list") {
      
      ## extract sampling period from first eseis object
      dt <- try(data[[1]]$meta$dt)
      
      ## check if dt can be extracted, otherwise stop function
      if(class(dt)[1] == "try-error") {
        
        stop("Signal object seems to contain no eseis objects!")
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
  
  if(class(d_map[[1]])[1] != "SpatialGridDataFrame") {
    stop("Distance maps must be list objects with SpatialGridDataFrames!")
  }
  
  ## assign snr values for normalisation
  if(normalise == TRUE & missing(snr) == TRUE) {
    
    print("No snr given. Will be calculated from signals")
    
    snr_flag = TRUE
    
  } else {
    
    snr_flag <- FALSE
  }
  
  ## collect descriptive statistics of traces
  s_min <- matrixStats::rowMins(data, na.rm = TRUE)
  s_max <- matrixStats::rowMaxs(data, na.rm = TRUE)
  s_mean <- matrixStats::rowMeans2(data, na.rm = TRUE)
  
  ## calculate/assign snr values
  if(snr_flag == TRUE) {
    
    s_snr <- s_max / s_mean
  } else {
    
    s_snr <- rep(1, nrow(data))
  }
  
  ## normalise input signals
  data <- (data - s_min) / (s_max - s_min)
  
  ## calculate signal duration
  duration <- ncol(data) * dt
  
  ## get combinations of stations
  pairs <- combn(x = nrow(data), 
                 m = 2)
  
  ## convert matrix to list
  pairs <- as.list(as.data.frame((pairs)))
  
  ## process all station pairs
  maps <- 
    lapply(X = pairs, FUN = function(pairs, data, duration, dt, 
                                     d_stations, v, s_max, s_snr, d_map) {
    
    ## calculate cross correlation function
    cc = acf(x = cbind(data[pairs[1],], 
                       data[pairs[2],]), 
             lag.max = duration * 1 / dt, 
             plot = FALSE)
    
    ## build lags vector
    lags <- c(rev(cc$lag[-1, 2, 1]), 
              cc$lag[, 1, 2]) * dt
    
    ## build correlation value vector
    cors <- c(rev(cc$acf[-1, 2, 1]), 
              cc$acf[, 1, 2])
    
    
    ## calculate minimum and maximum possible lag times
    lag_lim <- ceiling(d_stations[pairs[1], pairs[2]] / v)
    lag_ok <-lags >= -lag_lim & lags <= lag_lim
    
    ## calculate lag times
    lags <- lags[lag_ok]
    
    ## clip correlation vector to lag ranges
    cors <- cors[lag_ok]
    
    ## calculate SNR normalisation factor
    if(normalise == TRUE) {
      
      norm <- ((s_snr[pairs[1]] + s_snr[pairs[2]]) / 2) / mean(s_snr)
    } else {
      
      norm <- 1
    }
    
    ## get lag for maximum correlation
    t_max <- lags[cors == max(cors)]
    
    ## calculate modelled and emprirical lag times
    lag_model <- (raster::raster(d_map[[pairs[1]]]) - 
                    raster::raster(d_map[[pairs[2]]])) / v
    lag_empiric <-  d_stations[pairs[1], pairs[2]] / v
    
    ## calculate source density map
    cors_map <- exp(-0.5 * (((lag_model - t_max) / lag_empiric)^2)) * norm
    
    ## return output
    return(cors_map@data@values)
    
  }, data, duration, dt, d_stations, v, s_max, s_snr, d_map)
  
  ## convert list to matrix
  maps_values <- do.call(rbind, maps)
  
  ## assign sum of density values to input map
  map_out <- raster::raster(d_map[[1]])
  map_out@data@values <- matrixStats::colMeans2(x = maps_values)
  
  ## return output
  return(map_out)
}