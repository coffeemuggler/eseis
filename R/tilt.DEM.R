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
  
  shift = TRUE
  ### Logical \code{scalar}, option to shift the tilted DEM to new
  ### coordinate origin after tilting and interpolation.
  
){  
  
  ## check/set data structure
  if(class(DEM) != "RasterLayer") {
    stop("DEM must be a raster object")
  }
  
  ## check/set output resolution
  if(missing(resolution) == TRUE) {
    resolution <- mean(res(DEM))
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
    
    points.t <- points
  }
  
  ## convert DEM to SpatialPoints
  DEM.xyz <- SpatialPoints(DEM)
  
  ## isolate coordinate values
  DEM.x <- DEM.xyz@coords[,1]
  DEM.y <- DEM.xyz@coords[,2]
  DEM.z <- values(DEM)
  
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
      points.t[,1] <- sqrt(points[,1]^2 + points[,3]^2) * 
        cos((angle.x * pi) / 180 + points.atan.x)
      points.t[,2] <- points.t[,2]
      points.t[,3] <- sqrt(points[,1]^2 + points[,3]^2) * 
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
      points.t[,1] <- points.t[,1]
      points.t[,2] <- sqrt(points[,2]^2 + points[,3]^2) * 
        cos((angle.y * pi) / 180 + points.atan.y)
      points.t[,3] <- sqrt(points[,2]^2 + points[,3]^2) * 
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
  
  
  ## convert data set to raster
  DEM.int <- raster(DEM.int)
  projection(DEM.int) <- projection
  
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
  DEM <- raster::raster((datasets::volcano))
  
  ## generate some arbitrary points in space
  points <- cbind(c(0.2, 0.4), c(0.3, 0.5), c(140, 170))
  
  ## plot volcano DEM and points
  plot(DEM)
  points(points)
  
  ## tilt DEM and points by 0.1 rad
  DEM.tilt.x <- tilt.DEM(DEM = DEM, points = points, angle.x = 0.1)
  plot(DEM.tilt.x$DEM)
  points(DEM.tilt.x$points)
  
  ## re-tilt DEM and points back to initial conditions
  DEM.tilt.xy <- tilt.DEM(DEM = DEM, angle.x = 20, angle.y = -0.1)[[1]]
  plot(DEM.tilt.xy)
})
