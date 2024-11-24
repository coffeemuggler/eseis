#' Migrate signals of a seismic event through a grid of locations.
#'
#' The function performs signal migration in space in order to determine 
#' the location of a seismic signal.
#' 
#' With the switch from the package raster to the package terra, the 
#' resulting distance maps can no longer be saved in lists as distance
#' maps. Thus, the function re-defines the distance SpatRaster objects by
#' a list of data on crs, extent, resolution and raster values. As a 
#' consequence, plotting the data requires turning them into a SpatRaster
#' object, first (see examples).
#'
#' @param data \code{Numeric} matrix or \code{eseis} object, seismic signals 
#' to cross-correlate.
#' 
#' @param d_stations \code{Numeric} matrix, inter-station distances. Output 
#' of \code{spatial_distance}.
#' 
#' @param d_map \code{List} object, distance maps for each station. Output 
#' of \code{spatial_distance}.
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
#' @param verbose \code{Logical} value, option to show extended function 
#' information as the function is running. Default is \code{FALSE}.
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
#' dem <- terra::rast(nrows = 20, ncols = 20, 
#'                    xmin = 0, xmax = 10000, 
#'                    ymin= 0, ymax = 10000, 
#'                    vals = rep(0, 400))
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
#' terra::plot(dem)
#' 
#' text(x = sta$x, 
#'      y = sta$y, 
#'      labels = sta$ID)
#' 
#' ## calculate spatial distance maps and inter-station distances
#' D <- spatial_distance(stations = sta[,1:2],
#'                              dem = dem)
#'                              
#' ## restore SpatRaster object for plotting purpose
#' D_map_1 <- terra::rast(crs = D$maps[[1]]$crs,
#'                        ext = D$maps[[1]]$ext,
#'                        res = D$maps[[1]]$res,
#'                        val = D$maps[[1]]$val)
#'                       
#' ## plot distance map
#' terra::plot(D_map_1) 
#' 
#' ## locate signal
#' e <- spatial_migrate(data = s, 
#'                      d_stations = D$matrix, 
#'                      d_map = D$maps, 
#'                      v = 1000, 
#'                      dt = 1/100)
#' 
#' ## get most likely location coordinates
#' e_max <- spatial_pmax(data = e)
#' 
#' ## plot location estimate, most likely location estimate and stations
#' terra::plot(e)
#' points(e_max[1], 
#'        e_max[2],
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
    normalise = TRUE,
    verbose = FALSE
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
    
    stop("Distance maps must be list objects with SpatRasters!")
  }
  
  if(is.numeric(v) == FALSE & class(v)[1] != "SpatRaster") {
    
    stop("Velocity must be numeric value of SpatRaster!")
  }
  
  ## assign snr values for normalisation
  if(normalise == TRUE & missing(snr) == TRUE) {
    
    if(verbose == TRUE) {
      
      print("No snr given. Will be calculated from signals")
    }
    
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
  pairs <- combn(x = nrow(data), m = 2)
  
  ## convert matrix to list
  pairs <- as.list(as.data.frame((pairs)))
  
  ## build raster sets from map meta data
  d_map <- lapply(X = d_map, FUN = function(d_map) {
    
    terra::rast(extent = d_map$ext,
                res = d_map$res, 
                crs = d_map$crs,
                vals = d_map$val)
  })

  ## process all station pairs
  maps <- lapply(X = pairs, 
                 FUN = function(pairs, data, duration, dt,
                                d_stations, v, s_max, s_snr, d_map) {
                   
                   ## calculate cross correlation function
                   cc = acf(x = cbind(data[pairs[1],], data[pairs[2],]), 
                            lag.max = duration * 1 / dt, 
                            plot = FALSE)
                   
                   ## build lags vector
                   lags <- c(rev(cc$lag[-1, 2, 1]), cc$lag[, 1, 2]) * dt
                   
                   ## build correlation value vector
                   cors <- c(rev(cc$acf[-1, 2, 1]), cc$acf[, 1, 2])
                   
                   ## collect/transform velocity value(s)
                   if(is.numeric(v) == TRUE) {
                     v_lag <- v
                   } else {
                     v_lag <- as.numeric(terra::values(v))
                   }

                   ## calculate minimum and maximum possible lag times
                   lag_lim <- max(ceiling(d_stations[pairs[1], pairs[2]] / 
                                            v_lag))
                   lag_ok <-lags >= -lag_lim & lags <= lag_lim
                   
                   ## calculate lag times
                   lags <- lags[lag_ok]
                   
                   ## clip correlation vector to lag ranges
                   cors <- cors[lag_ok]
                   
                   ## calculate SNR normalisation factor
                   if(normalise == TRUE) {
                     
                     norm <- ((s_snr[pairs[1]] + s_snr[pairs[2]]) / 2) / 
                       mean(s_snr)
                   } else {
                     
                     norm <- 1
                   }
                   
                   ## get lag for maximum correlation
                   t_max <- lags[cors == max(cors)]
                   
                   ## calculate modelled and emprirical lag times
                   lag_model <- (d_map[[pairs[1]]] - d_map[[pairs[2]]]) / v
                   lag_empiric <- d_stations[pairs[1], pairs[2]] / v
                   
                   ## calculate source density map
                   cors_map <- exp(-0.5 * (((lag_model - t_max) / 
                                              lag_empiric)^2)) * norm
                   
                   ## return output
                   return(as.numeric(terra::values(cors_map)))
                   
                 }, data, duration, dt, d_stations, v, s_max, s_snr, d_map)
  
  ## convert list to matrix
  maps_values <- do.call(rbind, maps)
  
  ## assign sum of density values to input map
  map_out <- d_map[[1]]
  terra::values(map_out) <- matrixStats::colMeans2(x = maps_values)
  
  ## return output
  return(map_out)
}
