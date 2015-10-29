tilt.DEM <-
structure(function(# Function to tilt a DEM.
  ### This function tilts a DEM in x- and y-direction.
  
  DEM,
  ### Raster object, the DEM to be processed
  
  points,
  ### Numeric \code{matrix}, with x-y-z-coordinates of points (e.g. 
  ### seismic station locations) to be tilted along.
  
  angle.x,
  ### Numeric \code{scalar}, tilt angle in x-direction, in degree 
  
  angle.y,
  ### Numeric \code{scalar}, tilt angle in y-direction, in degree 
  
  resolution,
  ### Numeric \code{scalar}, resolution of the output DEM, same as input if
  ### omitted
  
  projection,
  ### Numeric \code{scalar}, projection of the output DEM, same as input if
  ### omitted
  
  shift = TRUE,
  ### Logical \code{scalar}, option to shift the tilted DEM to new
  ### coordinate origin after tilting and interpolation.
  
  reshift = TRUE
  ### Logical \code{scalar}, option to add shift back after tilting.
  
){  
  
  ## check/set data structure
  if(class(DEM) != "RasterLayer") {
    stop("DEM must be a raster object")
  }
  
  ## check/set output resolution
  if(missing(resolution) == TRUE) {
    resolution <- mean(raster::res(DEM))
  }
  
  ## check/set output resolution
  if(missing(projection) == TRUE) {
    projection <- raster::projection(DEM)
  }
  
  ## check points object
  if(missing(points) == FALSE) {
    if(is.matrix(points) == FALSE | ncol(points) != 3) {
      stop("Points must be given as matrix with 3 columns")
    }
  }
  
  ## Convert angles to radians
  if(missing(angle.x) == FALSE) {
    angle.x <- (angle.x * pi) / 180
  }
  
  if(missing(angle.y) == FALSE) {
    angle.y <- (angle.y * pi) / 180
  }
  ## convert DEM to SpatialPoints
  DEM.xyz <- sp::SpatialPoints(DEM)
  
  ## isolate coordinate values
  DEM.x <- DEM.xyz@coords[,1]
  DEM.y <- DEM.xyz@coords[,2]
  DEM.z <- raster::values(DEM)
  
  ## shift DEM to new coordinate origin, obligatory shift I
  ## query minimum coordinates
  shift.I.x <- min(DEM.x, na.rm = TRUE)
  shift.I.y <- min(DEM.y, na.rm = TRUE)
  shift.I.z <- min(DEM.z, na.rm = TRUE)
  
  ## shift DEM
  DEM.x <- DEM.x - shift.I.x
  DEM.y <- DEM.y - shift.I.y
  DEM.z <- DEM.z - shift.I.z
  
  if(missing(points) == FALSE) {
    ## shift points
    points[,1] <- points[,1] - shift.I.x
    points[,2] <- points[,2] - shift.I.y
    points[,3] <- points[,3] - shift.I.z
  }
  
  ## tilt in x-direction
  if(missing(angle.x) == FALSE) {
    
    ## calculate arcus tangent of DEM
    DEM.atan.x <- atan(DEM.z / DEM.x)
    
    if(missing(points) == FALSE) {
      points.atan.x <- atan(points[,3] / points[,1])
    }
    
    ## transform data sets
    DEM.x <- sqrt(DEM.x^2 + DEM.z^2) * cos((angle.x * pi) / 180 + DEM.atan.x)
    DEM.y <- DEM.y
    DEM.z <- sqrt(DEM.x^2 + DEM.z^2) * sin((angle.x * pi) / 180 + DEM.atan.x)
    
    if(missing(points) == FALSE) {
      points[,1] <- sqrt(points[,1]^2 + points[,3]^2) * 
        cos((angle.x * pi) / 180 + points.atan.x)
      points[,2] <- points[,2]
      points[,3] <- sqrt(points[,1]^2 + points[,3]^2) * 
        sin((angle.x * pi) / 180 + points.atan.x)
    }
  }
  
  ## tilt in y-direction
  if(missing(angle.y) == FALSE) {
    ## calculate arcus tangent of DEM
    DEM.atan.y <- atan(DEM.z / DEM.y)
    
    if(missing(points) == FALSE) {
      points.atan.y <- atan(points[,3] / points[,2])
    }
    
    ## transform data sets
    DEM.x <- DEM.x
    DEM.y <- sqrt(DEM.y^2 + DEM.z^2) * 
      cos((angle.y * pi) / 180 + DEM.atan.y)
    DEM.z <- sqrt(DEM.y^2 + DEM.z^2) * 
      sin((angle.y * pi) / 180 + DEM.atan.y)
    
    if(missing(points) == FALSE) {
      points[,1] <- points[,1]
      points[,2] <- sqrt(points[,2]^2 + points[,3]^2) * 
        cos((angle.y * pi) / 180 + points.atan.y)
      points[,3] <- sqrt(points[,2]^2 + points[,3]^2) * 
        sin((angle.y * pi) / 180 + points.atan.y)
    }
  }
  
  ## interpolate DEM
  DEM.int <- akima::interp(x = DEM.x, 
                           y = DEM.y, 
                           z = DEM.z, 
                           xo = seq(min(DEM.x, na.rm = TRUE), 
                                    max(DEM.x, na.rm = TRUE), 
                                    by = resolution),
                           yo = seq(min(DEM.y, na.rm = TRUE), 
                                    max(DEM.y, na.rm = TRUE), 
                                    by = resolution))
  
  ## optionally shift DEM to new coordinate origin
  if(shift == TRUE) {
    
    ## query minimum coordinates
    shift.II.x <- min(DEM.int$x, na.rm = TRUE)
    shift.II.y <- min(DEM.int$y, na.rm = TRUE)
    shift.II.z <- min(DEM.int$z, na.rm = TRUE)
    
    ## shift DEM
    DEM.int$x <- DEM.int$x - shift.II.x
    DEM.int$y <- DEM.int$y - shift.II.y
    DEM.int$z <- DEM.int$z - shift.II.z
    
    if(missing(points) == FALSE) {
      ## shift points
      points[,1] <- points[,1] - shift.II.x
      points[,2] <- points[,2] - shift.II.y
      points[,3] <- points[,3] - shift.II.z
    }
  } else {
    shift.II.x <- 0
    shift.II.y <- 0
    shift.II.z <- 0
  }
  
  if(reshift == TRUE) {
    DEM.int$x <- DEM.int$x + shift.II.x + shift.I.x
    DEM.int$y <- DEM.int$y + shift.II.y + shift.I.y
    DEM.int$z <- DEM.int$z + shift.II.z + shift.I.z
    
    if(missing(points) == FALSE) {
      points[,1] <- points[,1] + shift.II.x + shift.I.x
      points[,2] <- points[,2] + shift.II.y + shift.I.y
      points[,3] <- points[,3] + shift.II.z + shift.I.z
    }
  }
  
  
  ## convert data set to raster
  DEM.int <- raster::raster(DEM.int)
  raster::projection(DEM.int) <- projection
  
  if(missing(points) == TRUE) {
    return(list(
      DEM = DEM.int,
      shift.I.x = shift.I.x,
      shift.I.y = shift.I.y,
      shift.I.z = shift.I.z,
      shift.II.x = shift.II.x,
      shift.II.y = shift.II.y,
      shift.II.z = shift.II.z
    ))
  } else {
    return(list(
      DEM = DEM.int,
      points = points,
      shift.I.x = shift.I.x,
      shift.I.y = shift.I.y,
      shift.I.z = shift.I.z,
      shift.II.x = shift.II.x,
      shift.II.y = shift.II.y,
      shift.II.z = shift.II.z
    ))
  }
  ### List object with tilted DEM and applied parameters
  
  ##keyword<<
  ## environmental seismology
}, ex = function(){
  

  ## load volacano data set and convert to raster
  DEM <- raster::raster(datasets::volcano)
  
  ## generate some arbitrary points in space
  points <- cbind(c(0.55, 0.4), 
                  c(0.655, 0.45), 
                  c(150, 175))
  
  ## set plot parameters
  par(mfcol = c(1, 3))
  
  ## plot volcano DEM and points
  plot(DEM)
  points(points, 
         pch = 20, 
         col = 4)
  contour(DEM, 
          add = TRUE)
  
  ## tilt DEM and points by 0.1 rad
  DEM.tilt.x <- tilt.DEM(DEM = DEM, 
                         points = points, 
                         angle.x = 10)
  plot(DEM.tilt.x$DEM)
  points(DEM.tilt.x$points, 
         pch = 20, 
         col = 4)
  contour(DEM.tilt.x$DEM, 
          add = TRUE)
  
  ## crop DEM to remove NA-values
  DEM.tilt.x.crop <- raster::crop(x = DEM.tilt.x$DEM, 
                                  y = extent(c(0.04, 0.96, 0, 1)))
  
  ## re-tilt DEM and points back to initial conditions
  DEM.tilt.x.back <- tilt.DEM(DEM = DEM.tilt.x.crop, 
                              points = DEM.tilt.x$points, 
                              angle.x = -10)
  plot(DEM.tilt.x.back$DEM)
  points(DEM.tilt.x.back$points, 
         pch = 20, 
         col = 4)
  contour(DEM.tilt.x.back$DEM, 
          add = TRUE)
})
