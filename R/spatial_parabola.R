#' Locate signals of a seismic event by time difference parabola overlay
#'
#' The function performs event location in space by finding the grid cell 
#' with minimum average travel time difference using the parabola approach. For 
#' further information see also Hibert et al. (2014) DOI: 10.1002/2013JF002970.
#'
#' @param data \code{Numeric} matrix or \code{eseis} object, seismic signals 
#' to cross-correlate.
#' 
#' @param d_map \code{List} object, distance maps for each station. Output 
#' of \code{spatial_distance}.
#' 
#' @param v \code{Numeric} value or vector, apparent seismic wave velocity
#' (m/s). 
#' 
#' @param dt \code{Numeric} value, sampling period.
#' 
#' @param plot \code{Character} value, keyword defining if or which output 
#' is to be plotted. If omitted, no plot output is generated. If set to 
#' \code{"parabola"}, a plot with all overlaid station pair parabolas is 
#' created. If set to \code{"offset"}, a plot of the average time offset 
#' of each grid cell is created.
#' 
#' @param \dots Additional arguments passed to the plot function.
#' 
#' @return A terra raster with average travel time offsets for each grid 
#' cell, implying the most likely source location coinciding with the 
#' smallest offset value.
#' 
#' @author Michael Dietze, Clement Hibert (ITES Strasbourg)
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
#' s <- rbind(dnorm(x = 1:1000, mean = 400, sd = 50),
#'            dnorm(x = 1:1000, mean = 400, sd = 50),
#'            dnorm(x = 1:1000, mean = 800, sd = 50))
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
#' e <- spatial_parabola(data = s, 
#'                       d_map = D$maps, 
#'                       v = 1000, 
#'                       dt = 1/100,
#'                       plot = "parabola",
#'                       zlim = c(0, 2))
#' }
#'
#' @export spatial_parabola

spatial_parabola <- function(
    data,
    d_map,
    v,
    dt,
    plot,
    ...
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
  
  if(is.list(d_map) == FALSE) {
    
    stop("Distance maps must be list objects with SpatRasters!")
  }
  
  if(is.numeric(v) == FALSE) {
    
    stop("Velocity must be numeric value")
  }
  
  ## build terra rasters from distance objects
  D_rast <- lapply(X = d_map, FUN = function(d_map) {
    
    terra::rast(crs = d_map$crs,
                ext = d_map$ext,
                res = d_map$res,
                val = d_map$val)
  })
  
  ## convert distances to travel times
  T_rast <- lapply(X = D_rast, FUN = function(D_rast, v) {
    
    D_rast / v
  }, v = v)
  
  ## define station pairs
  ij <- as.list(as.data.frame(combn(x = 1:nrow(data), m = 2)))
  
  ## calculate pairwise arrival time differences
  T_d_ij <- lapply(X = ij, FUN = function(ij, T_rast) {
    
    (T_rast[[ij[1]]] - T_rast[[ij[2]]])
    
  }, T_rast = T_rast)
  
  ## estimate maximum lag time
  lag_max <- ceiling(max(sapply(X = T_rast, FUN = function(T_rast) {
    max(terra::values(T_rast))
  })) / dt)
  
  ## calculate pairwise signal offsets
  t_d_ij <- lapply(X = ij, FUN = function(ij, data, lag_max, dt) {
    
    ## calculate cross-correlation function for station pairs
    cc <- ccf(x = data[ij[1],], 
              y = data[ij[2],],
              lag.max = lag_max / dt, 
              plot = FALSE)
    
    ## find time offset of maximum correlation value
    t_d <- cc$lag[cc$acf == max(cc$acf)] * dt
    
    return(t_d)
    
  }, data = data, lag_max = lag_max, dt)
  
  ## convert list to vector
  t_d_ij <- do.call(c, t_d_ij)
  
  ## calculate time mismatches
  T_dev <- lapply(X = 1:length(T_d_ij), FUN = function(i, T_d_ij, t_d_ij) {
    
    abs(T_d_ij[[i]] - (t_d_ij[i]))
  }, T_d_ij = T_d_ij, t_d_ij = t_d_ij)
  
  ## calculate mean time offsets for each grid cell
  T_dev_avg <- sum(do.call(c, T_dev)) / length(T_dev)
  
  ## optional plot part
  if(missing(plot) == FALSE) {
    
    ## read additional plot arguments
    arg <- list(...)
    
    ## check/set colours
    if ("col" %in% names(arg) == FALSE) {
      arg$col <- colorspace::sequential_hcl(n = 200, 
                                            palette = "Inferno",
                                            rev = TRUE)
    }
    
    ## check/set z limits
    if ("zlim" %in% names(arg) == FALSE) {
      arg$zlim <- range(terra::values(do.call(c, T_dev)))
    }
    
    if(plot == "parabola") {
      
      T_plot <- lapply(X = T_dev, FUN = function(T_dev, arg) {
        
        terra::values(T_dev)[terra::values(T_dev) < arg$zlim[1]] <- 
          arg$zlim[1]
        terra::values(T_dev)[terra::values(T_dev) > arg$zlim[2]] <- 
          arg$zlim[2]
        
        return(T_dev)
      }, arg)

      T_plot <- sum(do.call(c, T_plot)) / length(T_plot)
      
      terra::plot(T_plot, col = arg$col)
    }
    
    if(plot == "offset") {
      
      T_plot <- T_dev_avg
      terra::values(T_plot)[terra::values(T_plot) < arg$zlim[1]] <- 
        arg$zlim[1]
      terra::values(T_plot)[terra::values(T_plot) > arg$zlim[2]] <- 
        arg$zlim[2]
      
      terra::plot(T_plot, col = arg$col, range = arg$zlim)
    }
    
  }
  
  ## assign output
  map_out <- T_dev_avg

  ## return output
  return(map_out)
}