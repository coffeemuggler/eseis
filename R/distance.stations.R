distance.stations <-
structure(function(# Function to calculate interstation distances
  ### This function calculates the distances between seismic stations. It 
  ### supports topography-corrected calculations.
  
  stations,
  ### Numeric \code{matrix} x-, y-, z-coordinates of the seismic stations. If
  ### a DEM is provided station coordinates may be limited to x- and y-values.
  
  DEM
  ### Numeric \code{matrix}, or \code{raster} object, digital elevation model
  ### used for topography correction. if omitted, direct distances between 
  ### stations will be calculated. The coordinate system and projection of the 
  ### station coordinates will be set to those of the DEM. The z-coordinates 
  ### of the stations will be taken from the DEM.
){
  
  ## check input data
  if(is.matrix(stations) == FALSE | ncol(stations) < 2) {
    stop("Data set stations is no matrix with two or three columns!")
  }
  
  if(missing(DEM) == FALSE) {
    if(typeof(DEM) != "S4" | class(DEM) != "RasterLayer") {
      stop("DEM is neither a matrix or raster data set.")
    }
  }
  
  ## create output data set
  distances <- matrix(nrow = nrow(stations),
                      ncol = nrow(stations))
  rownames(distances) <- rownames(stations)
  colnames(distances) <- rownames(stations)
  
  ## case A - no DEM present
  if(missing(DEM) == TRUE) {
    
    ## loop through all stations
    for(i in 1:nrow(stations)) {
      
      ## loop through all stations
      for(j in 1:nrow(stations)) {
        
        if(ncol(stations) == 2) {
          
          ## calculate euclidian distance and assign it to output data set
          distances[i,j] <- sum(sqrt((stations[i,1] - stations[j,1])^2 + 
                                       (stations[i,2] - stations[j,2])^2))
          
        } else if(ncol(stations) == 3) {
          
          ## calculate euclidian distance and assign it to output data set
          distances[i,j] <- sum(sqrt((stations[i,1] - stations[j,1])^2 + 
                                       (stations[i,2] - stations[j,2])^2 + 
                                       (stations[i,3] - stations[j,3])^2))
        }
        
      }
    }
  }
  
  ## case B - DEM present
  if(missing(DEM) == FALSE) {
    
    ## convert DEM to SpatialGridDataFrame
    DEM <- as(DEM, "SpatialGridDataFrame")
    
    ## convert xy-coordinates of statiions to SpatialPoints
    xy <- sp::SpatialPoints(coords = stations[,1:2], 
                            proj4string = sp::CRS(raster::projection(DEM)))
    
    ## loop through all stations
    for(i in 1:length(xy)) {
      
      ## calculate euclidian xy-distances between stations
      dx.stations <- sp::coordinates(xy)[,1] - sp::coordinates(xy)[i,1]
      dy.stations <- sp::coordinates(xy)[,2] - sp::coordinates(xy)[i,2]
      dt.stations <- sqrt(dx.stations^2 + dy.stations^2)
      
      ## assign DEM z-value to stations
      z <- sp::over(x = xy, y = DEM)
      
      ## loop through all stations
      for(j in 1:length(dt.stations)) {
        
        ## calculate number of points to interpolate
        n.i <- round(x = dt.stations[j] / (0.1 * mean(DEM@grid@cellsize)), 
                     digits = 0)
        
        ## correct for zero points
        n.i <- ifelse(n.i == 0, 1, n.i)
        
        ## create x-vector
        x.i <- seq(from = sp::coordinates(xy)[i,1], 
                   to = sp::coordinates(xy)[j,1],
                   length.out = n.i)
        
        ## create y-vector
        y.i <- seq(from = sp::coordinates(xy)[i,2], 
                   to = sp::coordinates(xy)[j,2],
                   length.out = length(x.i))
        
        ## convert x and y vector to SpatialPoints coordinates
        xy.i <- sp::SpatialPoints(coords = cbind(x.i, y.i), 
                                  proj4string = sp::CRS(raster::projection(DEM)))
        
        ## interpolate xy by DEM
        z.i <- as.numeric(unlist(sp::over(x = xy.i, y = DEM)))
        
        ## calculate direct line elevantion
        z.d <- seq(from = z[i,], to = z[j,], length.out = length(z.i))
        
        ## calculate difference of DEm to direct elevation change
        d.e <- z.d - z.i
        
        ## calculate elevation path
        z.e <- ifelse(d.e < 0, z.i, z.d)
        
        ## calculate path length and assign it to output data set
        distances[i,j] <- sum(sqrt(diff(x.i)^2 + diff(y.i)^2 + diff(z.e)^2))
      }
    }
    
  }
  
  ## return distance matrix
  return(distances)
  ### A numeric matrix with station distances
  
  ##keyword<<
  ## environmental seismology
  
}, ex = function(){
  ## no examples present!
})
