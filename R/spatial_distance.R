#' Calculate topography-corrected distances for seismic waves.
#' 
#' The function calculates topography-corrected distances either between 
#' seismic stations or from seismic stations to pixels of an input raster.
#' 
#' Topography correction is necessary because seismic waves can only travel 
#' on the direct path as long as they are within solid matter. When the 
#' direct path is through air, the wave can only travel along the surface
#' of the landscape. The function accounts for this effect and returns the
#' corrected travel distance data set.
#' 
#' @param stations \code{Numeric} matrix of length two, x- and y-coordinates 
#' of the seismic stations to be processed (column-wise orgnaised).The 
#' coordinates must be in metric units, such as the UTM system and 
#' match with the reference system of the \code{dem}.
#' 
#' @param dem \code{raster} object, the digital elevation model (DEM) to be 
#' processed. The DEM must be in metric units, such as the UTM system and 
#' match with the reference system of the coordinates of \code{stations}. 
#' See \code{raster} for supported types and how to read these to R.
#' 
#' @param topography \code{Logical} scalar, option to enable topography
#' correction, default is \code{TRUE}.
#' 
#' @param cores \code{Numeric} scalar, number of CPU cores to use, only
#' relevant for multicore computers. Default is \code{1}.
#' 
#' @param dmap \code{Logical} scalar, option to enable/disable calculation
#' of distance maps. Default is \code{TRUE}.
#' 
#' @param dstation \code{Logical} scalar, option to enable/disable calculation
#' of interstation distances. Default is \code{TRUE}.
#' 
#' @param aoi \code{Numeric} vector of length four, bounding coordinates of 
#' the area of interest to process, in the form \code{c(x0, x1, y0, y1)}. Only 
#' implemented for single core mode (i.e., \code{cores = 1}).
#' 
#' @return \code{List} object with distance maps list and station distance 
#' matrix.
#' @author Michael Dietze
#' @keywords eseis
#' @examples
#' \dontrun{
#' ## load and aggregate example DEM
#' data("volcano")
#' dem <- raster::raster(volcano)
#' dem <- raster::aggregate(x = dem, 2) * 10
#' dem@extent <- dem@extent * 1000
#' dem@extent <- dem@extent + c(510, 510, 510, 510)
#' 
#' ## define example stations
#' stations <- cbind(c(200, 700), c(220, 700))
#' 
#' ## plot example data
#' raster::plot(dem)
#' points(stations[,1], stations[,2])
#' 
#' ## calculate distance matrices and stations distances
#' D <- spatial_distance(stations = stations, 
#'                       dem = dem, 
#'                       topography = TRUE, 
#'                       cores = 1)
#' 
#' ## plot distance map for station 2
#' raster::plot(D$maps[[2]])
#' 
#' ## show station distance matrix
#' print(D$stations)
#' 
#' ## run with small aoi
#' D <- spatial_distance(stations = stations, 
#'                       dem = dem, 
#'                       topography = TRUE, 
#'                       cores = 1, 
#'                       aoi = c(400, 600, 600, 800))
#' } 
#'                                           
#' @export spatial_distance
spatial_distance <- function(
  stations,
  dem, 
  topography = TRUE,
  cores = 1,
  dmap = TRUE,
  dstation = TRUE,
  aoi
) {
  
  ## PART 1 - calculate distance maps -----------------------------------------
  
  ## optionally calculate distance maps
  if(dmap == TRUE) {
    
    ## run in non-parallel mode
    if(cores == 1) {
      
      ## create output data set
      maps <- vector(mode = "list", length = nrow(stations))
      
      ## convert DEM to SpatialGridDataFrame
      dem <- methods::as(dem, "SpatialGridDataFrame")
      
      ## get mean dem grid size
      mean_cell_size <- mean(dem@grid@cellsize, na.rm = TRUE)
      
      ## convert xy-coordinates of stations to SpatialPoints
      xy <- sp::SpatialPoints(coords = stations[,1:2], 
                              proj4string = sp::CRS(raster::projection(dem)))
      
      ## optionally assign DEM z-value to stations
      z <- as.numeric(sp::over(x = xy, y = dem)[,1])
      
      ## check/set aoi
      if(missing(aoi) == TRUE) {
        
        map_coords <- sp::coordinates(obj = dem)
        
        aoi <- c(min(map_coords[,1]), 
                 max(map_coords[,1]),
                 min(map_coords[,2]), 
                 max(map_coords[,2]))
      }
      
      aoi_flag <- dem
      aoi_flag@data <- aoi_flag@data * 0
      aoi_xy <- sp::coordinates(aoi_flag)
      aoi_flag@data[[1]][aoi_xy[,1] >= aoi[1] & aoi_xy[,1] <= aoi[2] &
                           aoi_xy[,2] >= aoi[3] & aoi_xy[,2] <= aoi[4]] <- 1
      
      ## create preliminary output variables
      map_i <- dem
      
      ## loop through all stations
      for(i in 1:length(xy)) {
        
        ## print progress
        print(paste("Processing station", i))
        
        ## calculate euclidian distances
        dem_x <- sp::coordinates(dem)[,1]
        dem_y <- sp::coordinates(dem)[,2]
        
        dx <- dem_x - sp::coordinates(xy)[i,1]
        dy <- dem_y - sp::coordinates(xy)[i,2]
        dt <- sqrt(dx^2 + dy^2)
        
        ## loop through all grid cells
        for(j in 1:length(dt)) {
          
          ## check aoi containment
          if(aoi_flag@data[[1]][j] == 1) {
            
            ## calculate number of points to interpolate
            n_i <- round(x = dt[j] / (0.1 * mean_cell_size), digits = 0)
            
            ## correct for zero points
            n_i <- ifelse(n_i == 0, 1, n_i)
            
            ## create x-vector
            x_i <- seq(from = sp::coordinates(xy)[i,1], 
                       to = sp::coordinates(dem)[j,1],
                       length.out = n_i)
            
            ## create y-vector
            y_i <- seq(from = sp::coordinates(xy)[i,2], 
                       to = sp::coordinates(dem)[j,2],
                       length.out = n_i)
            
            ## convert x and y vector to SpatialPoints coordinates
            xy_i <- sp::SpatialPoints(
              coords = cbind(x_i, y_i), 
              proj4string = sp::CRS(raster::projection(dem)))
            
            ## interpolate xy by DEM
            z_i <- sp::over(x = xy_i, y = dem)[,1]
            
            ## calculate direct line elevantion
            z_d <- seq(from = z[i], 
                       to = dem@data[[1]][j], 
                       length.out = length(z_i))
            
            ## calculate difference of DEM to direct elevation change
            d_e <- z_d - z_i
            
            ## calculate elevation path
            if(topography == TRUE) {
              
              z_e <- ifelse(d_e < 0, z_i, z_d)
              
            } else {
              
              z_e <- z_d
              
            }
            
            ## calculate path length
            l <- sqrt((x_i[length(x_i)] - x_i[1])^2 + 
                        (y_i[length(x_i)] - y_i[1])^2 +
                        sum(diff(z_e))^2)
            
            ## assign topography-corrected distance to grid data set
            map_i@data[[1]][j] <- l
          } else {
            
            map_i@data[[1]][j] <- NA
          }
        }
        maps[[i]] <- map_i
      }
      
    } else {
      ## run in parallel mode
      
      ## detect number of cores
      n_cores <- parallel::detectCores()
      n_cores <- ifelse(n_cores < cores, n_cores, cores)
      
      ## initiate cluster
      cl <- parallel::makeCluster(getOption("mc.cores", n_cores))
      
      ## create output data set
      maps <- vector(mode = "list", 
                     length = nrow(stations))
      
      ## convert DEM to SpatialGridDataFrame
      dem <- as(dem, "SpatialGridDataFrame")
      
      ## convert xy-coordinates of stations to SpatialPoints
      xy <- sp::SpatialPoints(coords = stations[,1:2], 
                              proj4string = sp::CRS(raster::projection(dem)))
      
      ## assign DEM z-value to stations
      z <- as.numeric(sp::over(x = xy, y = dem)[,1])
      
      ## create preliminary output variables
      map_i <- dem
      
      ## define parallel mode function
      work_parallel <- function(x, dem) {
        
        ## extract input parameters from input string
        x_sep <- strsplit(x = x, split = "__", fixed = TRUE)
        
        topo_option <- as.numeric(x_sep[[1]][1])
        dt <- as.numeric(x_sep[[1]][2])
        dem_cellsize <- as.numeric(x_sep[[1]][3])
        dem_projection <- as.character(x_sep[[1]][4])
        x_station <- as.numeric(x_sep[[1]][5])
        y_station <- as.numeric(x_sep[[1]][6])
        z_station <- as.numeric(x_sep[[1]][7])
        x_dem <- as.numeric(x_sep[[1]][8])
        y_dem <- as.numeric(x_sep[[1]][9])
        z_dem <- as.numeric(x_sep[[1]][10])
        
        
        ## calculate number of points to interpolate
        n_i <- round(x = dt / (0.1 * dem_cellsize), 
                     digits = 0)
        
        ## correct for zero points
        n_i <- ifelse(n_i == 0, 1, n_i)
        
        ## create x-vector
        x_i <- seq(from = x_station, 
                   to = x_dem,
                   length.out = n_i)
        
        ## create y-vector
        y_i <- seq(from = y_station, 
                   to = y_dem,
                   length.out = n_i)
        
        ## convert x and y vector to SpatialPoints coordinates
        xy_i <- sp::SpatialPoints(coords = cbind(x_i, y_i), 
                                  proj4string = sp::CRS(dem_projection))
        
        ## interpolate xy by DEM
        z_i <- sp::over(x = xy_i, y = dem)[,1]
        
        ## calculate direct line elevantion
        z_d <- seq(from = z_station, 
                   to = z_dem, 
                   length.out = length(z_i))
        
        ## calculate difference of DEM to direct elevation change
        d_e <- z_d - z_i
        
        ## calculate elevation path
        if(topo_option == 1) {
          
          z_e <- ifelse(d_e < 0, z_i, z_d)
          
        } else {
          
          z_e <- z_d
          
        }
        
        ## calculate path length
        l <- sqrt((x_i[length(x_i)] - x_i[1])^2 + 
                    (y_i[length(x_i)] - y_i[1])^2 +
                    sum(diff(z_e))^2)
        
        ## return output
        return(l)
        
      }
      
      ## loop through all stations
      for(i in 1:length(xy)) {
        
        ## print progress
        print(paste("Processing map for station", i))
        
        ## calculate euclidian distances
        dx <- sp::coordinates(dem)[,1] - sp::coordinates(xy)[i,1]
        dy <- sp::coordinates(dem)[,2] - sp::coordinates(xy)[i,2]
        
        ## assign input data for parallel helper function
        n_data <- nrow(dem)
        
        topo_option <- rep(x = as.numeric(topography), 
                           times = n_data)
        dt <- sqrt(dx^2 + dy^2)
        dem_cellsize <- rep(x = mean(dem@grid@cellsize, na.rm = TRUE), 
                            times = n_data)
        dem_projection <- rep(x = as.character(
          sp::CRS(raster::projection(dem))), 
          times = n_data)
        
        x_station <- rep(x = as.numeric(sp::coordinates(xy)[i,1]), 
                         times = n_data)
        y_station <- rep(x = as.numeric(sp::coordinates(xy)[i,2]), 
                         times = n_data)
        z_station <- rep(x = z[i],
                         times = n_data)
        
        x_dem <- as.numeric(sp::coordinates(dem)[,1])
        y_dem <- as.numeric(sp::coordinates(dem)[,2])
        z_dem <- as.numeric(dem@data[[1]])
        
        input_parallel <- paste(topo_option,
                                dt, 
                                dem_cellsize,
                                dem_projection,
                                x_station,
                                y_station,
                                z_station,
                                x_dem,
                                y_dem,
                                z_dem,
                                sep = "__")
        
        
        ## calculate distance data
        l <- parallel::parLapply(cl, 
                                 X = input_parallel, 
                                 fun = work_parallel,
                                 dem = dem)
        
        ## assign topography-corrected distance to grid data set
        map_i@data[[1]] <- unlist(l)
        
        maps[[i]] <- map_i
      }
      
      ## stop cluster
      parallel::stopCluster(cl)
    }
  } else {
    
    maps <- vector(mode = "list", length = nrow(stations))
  }


  ## PART 2 - calculate station distances -------------------------------------
  
  if(dstation == TRUE) {
    
    ## print progress
    print("Processing station distances")
    
    ## create output data set
    distances <- matrix(nrow = nrow(stations),
                        ncol = nrow(stations))
    rownames(distances) <- rownames(stations)
    colnames(distances) <- rownames(stations)
    
    ## convert xy-coordinates of stations to SpatialPoints
    xy <- sp::SpatialPoints(coords = stations[,1:2], 
                            proj4string = sp::CRS(raster::projection(dem)))
    
    ## convert DEM to SpatialGridDataFrame
    dem <- methods::as(dem, "SpatialGridDataFrame")
    
    ## loop through all stations
    for(i in 1:length(xy)) {
      
      ## calculate euclidian xy-distances between stations
      dx_stations <- sp::coordinates(xy)[,1] - sp::coordinates(xy)[i,1]
      dy_stations <- sp::coordinates(xy)[,2] - sp::coordinates(xy)[i,2]
      dt_stations <- sqrt(dx_stations^2 + dy_stations^2)
      
      ## assign DEM z-value to stations
      z <- sp::over(x = xy, y = dem)
      
      ## loop through all stations
      for(j in 1:length(dt_stations)) {
        
        ## calculate number of points to interpolate
        n_i <- round(x = dt_stations[j] / (0.1 * mean(
          dem@grid@cellsize, na.rm = TRUE)), 
          digits = 0)
        
        ## correct for zero points
        n_i <- ifelse(n_i == 0, 1, n_i)
        
        ## create x-vector
        x_i <- seq(from = sp::coordinates(xy)[i,1], 
                   to = sp::coordinates(xy)[j,1],
                   length.out = n_i)
        
        ## create y-vector
        y_i <- seq(from = sp::coordinates(xy)[i,2], 
                   to = sp::coordinates(xy)[j,2],
                   length.out = length(x_i))
        
        ## convert x and y vector to SpatialPoints coordinates
        xy_i <- sp::SpatialPoints(coords = cbind(x_i, y_i), 
                                  proj4string = sp::CRS(raster::projection(dem)))
        
        ## interpolate xy by DEM
        z_i <- as.numeric(unlist(sp::over(x = xy_i, y = dem)))
        
        ## calculate direct line elevantion
        z_d <- seq(from = z[i,], to = z[j,], length.out = length(z_i))
        
        ## calculate difference of DEm to direct elevation change
        d_e <- z_d - z_i
        
        ## calculate elevation path
        z_e <- ifelse(d_e < 0, z_i, z_d)
        
        ## calculate path length and assign it to output data set
        distances[i,j] <- sum(sqrt(diff(x_i)^2 + diff(y_i)^2 + diff(z_e)^2))
      }
    }
  } else {
    
    distances <- matrix(nrow = nrow(stations),
                        ncol = nrow(stations))
    rownames(distances) <- rownames(stations)
    colnames(distances) <- rownames(stations)
  }

  ## return distance matrices
  return(list(maps = maps,
              stations = distances))
}