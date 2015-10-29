convert.coordinates <-
structure(function(# Convert coordinates between reference systems.
  ### This function converts coordinates between different reference systems.

  data,
  ### \code{Numeric} matrix or data frame, x-, y-coordinates to be converted.
  
  from,
  ### \code{Character} scalar, proj4 string of the current reference system.
  
  to
  ### \code{Character} scalar, proj4 string of the target reference system.
) {
  
  ## check input data
  if(ncol(data) != 2) {
    
    stop("Coordinate data must contain only 2 columns")
  }
  
  ## convert coordinates to SpatialPoints object
  data <- sp::SpatialPoints(data)
  
  ## assign CRS to SpatialPoints object
  sp::proj4string(data) <- sp::CRS(projargs = from)
  
  ## convert CRS
  data <-sp::spTransform(x = data, CRS = to)
  
  ## convert output to data frame
  data <- as.data.frame(sp::coordinates(data))
  
  ## return data frame
  return(data)
  ### A \code{Data frame} with transformed coordinates.
  
   ##keyword<<
  ## environmental seismology
  
}, ex = function(){
 
  xy <- cbind(13, 55)
  
  proj.in <- "+proj=longlat + ellps=WGS84"
  
  proj.out <- "+proj=utm +zone=32 +datum=WGS84"
  
  convert.coordinates(data = xy, 
                      from = proj.in, 
                      to = proj.out)
})
