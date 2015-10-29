distance.map <-
structure(function(# Function to calculate a distance map
  ### This function calculates the distances between grid-cells and seismic
  ### stations.
  
  stations,
  ### Numeric \code{matrix} x-, y-, z-coordinates of the seismic stations. If
  ### a DEM is provided station coordinates may be limited to x- and y-values.
  
  DEM,
  ### Numeric \code{matrix}, or \code{raster} object, digital elevation model
  ### used to calculate the distance of eachpixel to each station. The 
  ### coordinate system and projection of the station coordinates will be set 
  ### to those of the DEM. If station coordinates do not have a z-coordinate, 
  ### this z-coordinate will be taken from the DEM.
  
  topography.correction = TRUE
  ### Logical \code{scala}, option to calculate distance map in topography
  ### corrected mode.
){
  
  ## check input data
  if(is.matrix(stations) == FALSE | ncol(stations) < 2) {
    
    if(missing(DEM) == TRUE) {
      
      stop("Data set stations is no matrix with two or three columns!")
    }
  }
  
  if(missing(DEM) == FALSE) {
    
    if(typeof(DEM) != "S4" | class(DEM) != "RasterLayer") {
      
      stop("DEM is neither a matrix or raster data set.")
    }
  }
  
  ## create output data set
  maps <- list(NA)
  
  ## convert DEM to SpatialGridDataFrame
  DEM <- as(DEM, "SpatialGridDataFrame")
  
  ## check for NA values
  warning("JUST A NOTE: EDIT CODE TO IMPLEMENT NA-CHECK OF DEM, LINE 1279")
  
  ## convert xy-coordinates of stations to SpatialPoints
  xy <- sp::SpatialPoints(coords = stations[,1:2], 
                          proj4string = sp::CRS(raster::projection(DEM)))
  
  ## optionally assign DEM z-value to stations
  if(ncol(stations) == 2) {
    z <- as.numeric(sp::over(x = xy, y = DEM)[,1])
  } else {
    z <- stations(,3)
  }
  
  ## create preliminary output variables
  map.i <- DEM
  
    ## loop through all stations
    for(i in 1:length(xy)) {
      
      ## print progress
      print(paste("Processing station", i))
            
      ## calculate euclidian distances
      dx <- sp::coordinates(DEM)[,1] - sp::coordinates(xy)[i,1]
      dy <- sp::coordinates(DEM)[,2] - sp::coordinates(xy)[i,2]
      dt <- sqrt(dx^2 + dy^2)

      ## loop through all grid cells
      for(j in 1:length(dt)) {
        
        ## calculate number of points to interpolate
        n.i <- round(x = dt[j] / (0.1 * mean(DEM@grid@cellsize)), 
                     digits = 0)
        
        ## correct for zero points
        n.i <- ifelse(n.i == 0, 1, n.i)
        
        ## create x-vector
        x.i <- seq(from = sp::coordinates(xy)[i,1], 
                   to = sp::coordinates(DEM)[j,1],
                   length.out = n.i)
        
        ## create y-vector
        y.i <- seq(from = sp::coordinates(xy)[i,2], 
                   to = sp::coordinates(DEM)[j,2],
                   length.out = n.i)
        
        ## convert x and y vector to SpatialPoints coordinates
        xy.i <- sp::SpatialPoints(coords = cbind(x.i, y.i), 
                                  proj4string = sp::CRS(raster::projection(DEM)))
        
        ## interpolate xy by DEM
        z.i <- sp::over(x = xy.i, y = DEM)[,1]
        
        ## calculate direct line elevantion
        z.d <- seq(from = z[i], 
                   to = DEM@data[[1]][j], 
                   length.out = length(z.i))
        
        ## calculate difference of DEM to direct elevation change
        d.e <- z.d - z.i
        
        ## calculate elevation path
        z.e <- ifelse(d.e < 0, z.i, z.d)
        
        ## calculate path length
        l <- sqrt((x.i[length(x.i)] - x.i[1])^2 + 
                  (y.i[length(x.i)] - y.i[1])^2 +
                  sum(diff(z.e))^2)
        
        ## assign topography-corrected distance to grid data set
        map.i@data[[1]][j] <- l
      }
      maps[[length(maps) + 1]] <- map.i
    }

  ## remove dummy varibale
  maps[[1]] <- NULL
  
  ## return distance matrices
  return(maps)
  ### A list with numeric matrices with pixel distances to stations
  
  ##keyword<<
  ## environmental seismology
  
}, ex = function(){
  ## no examples present!
})
