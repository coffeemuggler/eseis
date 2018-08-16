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
#' @param depth \code{Numeric} vector, depth below surface for which to 
#' calculate the wave travel path lengths for each pixel. Default is 
#' \code{0} (at the surface). Useful to test whether a source location is at 
#' depth or at the surface. 
#' 
#' @param topography \code{Logical} value, option to enable topography
#' correction, default is \code{TRUE}.
#' 
#' @param cpu \code{Numeric} value, fraction of CPUs to use for parallel 
#' processing. If omitted, one CPU is used.
#' 
#' @param dmap \code{Logical} value, option to enable/disable calculation
#' of distance maps. Default is \code{TRUE}.
#' 
#' @param dstation \code{Logical} value, option to enable/disable calculation
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
  depth = 0,
  topography = TRUE,
  cpu,
  dmap = TRUE,
  dstation = TRUE,
  aoi
) {
  
  ## PART 1 - calculate distance maps -----------------------------------------

  ## check/set fraction of CPUs to use
  if(missing(cpu) == TRUE) {
    
    cpu <- NA
  }
  
  ## detect and adjust number of cores to use
  cores <- parallel::detectCores()

  if(is.na(cpu) == FALSE) {
    
    n_cpu <- floor(cores * cpu)
    cores <- ifelse(cores < n_cpu, cores, n_cpu)
  } else {
    
    cores <- 1
  }

  ## aoi assignment
  if(missing(aoi) == FALSE) {
    
    dem <- raster::mask(x = dem, 
                        mask = aoi, 
                        inverse = TRUE)
  }
  
  ## extract dem coordinates
  dem_xyz <- cbind(sp::coordinates(dem), 
                   raster::values(dem))
  
  ## convert raster to SpatialPixelsDataFrame
  dem_spdf <- as(object = dem, 
                 Class = "SpatialPixelsDataFrame")
  
  ## get mean dem resolution
  res_mean <- mean(raster::res(dem))
  
  ## extract elevation at station
  station_z <- sp::over(
    x = sp::SpatialPointsDataFrame(coords = cbind(stations[,1],
                                                  stations[,2]), 
                                   data = data.frame(id = 1:nrow(stations))),
    y = dem_spdf)
  
  ## create station xyz coordinates
  station_xyz <- cbind(stations, station_z)
  colnames(station_xyz) <- c("x", "y", "z")
  
  ## create parameter list
  pars <- list(res_mean = res_mean,
               depth = depth,
               topography = topography)

  ## PART 2 - CALCULATE WAVE LENGTHS FOR EACH GRID CELL -----------------------
  if(dmap == TRUE) {
    
    ## print progress
    print("Processing distance matrices")
    
    ## create output object
    D_map <- vector(mode = "list", 
                    length = nrow(stations))
    
    ## initiate cluster
    cl <- parallel::makeCluster(getOption("mc.cores", cores))
    
    for(i in 1:length(D_map)) {
      
      ## get station coordinates to process
      station_xyz_i <- as.numeric(station_xyz[i,])
      
      ## convert dem coordinates to list
      dem_xyz_list <- as.list(as.data.frame(t(dem_xyz)))
      
      ## calculate wave path lengths for each pixel
      d <- parallel::parLapply(
        cl = cl,
        X = dem_xyz_list, 
        fun = function(dem_xyz_list,
                       dem_xyz,
                       station_xyz_i,
                       pars) {
          
          ## check if pixel to process is in AOI
          if(sum(is.na(dem_xyz_list)) == 0) {
            
            ## create line end points
            xy_0 <- as.numeric(dem_xyz_list)
            xy_1 <- as.numeric(station_xyz_i[1:2])
            
            ## calculate line length
            l_line <- sqrt((xy_0[1] - xy_1[1])^2 + (xy_0[2] - xy_1[2])^2)
            
            ## calculate number of nodes along which to interpolate
            n_line <- as.numeric(ceiling(l_line / pars$res_mean))
            
            ## account for legth zero
            if(n_line == 0) {
              
              n_line <- 2
            }
            
            ## interpolate direct distance from pixel to station
            z_direct <- seq(from = dem_xyz_list[3] - pars$depth, 
                            to = station_xyz_i[3], 
                            length.out = n_line)
            
            ## calculate wave path height
            z_line <- z_direct
            
            ## optionally, correct for topography effect
            if(pars$topography == TRUE) {

              ## calculate node coordinates
              x_line <- seq(from = xy_0[1], 
                            to = xy_1[1], 
                            length.out = n_line)  
              
              y_line <- seq(from = xy_0[2], 
                            to = xy_1[2], 
                            length.out = n_line)  
              
              xy_line <- sp::SpatialPoints(coords = cbind(x_line, y_line))
              
              # ## extract elevation along line
              z_surface <- sp::over(x = xy_line, 
                                    y = dem_spdf)[,1] 
              
              ## get elevation values above surface
              z_above <- z_direct > z_surface
              
              ## correct for topography
              z_line[z_above == TRUE] <- z_surface[z_above]
            }
            
            ## get absolute path length
            xyz_length <- sqrt((x_line[n_line] - x_line[1])^2 +
                                 (y_line[n_line] - y_line[1])^2 +
                                 sum(diff(z_line))^2)
            
          } else {
            
            xyz_length <- NA
            
          }
          
          ## return output
          return(xyz_length)
          
        }, dem_xyz, station_xyz_i, pars)
      
      ## convert list to vector
      d <- as.numeric(do.call(c, d))
      
      ## create output object
      D <- dem
      
      ## assign output values
      raster::values(D) <- d
      
      ## assign distane map to output data set
      D_map[[i]] <- D
      
      ## print progress
      print(paste("  Map", i, "of", length(D_map), "done."))
    }
    
    ## stop cluster
    parallel::stopCluster(cl = cl)
  } else {
    
    D_map <- vector(mode = "list",
                    length = nrow(stations))
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
    
    ## loop through all stations
    for(i in 1:nrow(distances)) {
      
      ## loop through all stations
      for(j in 1:ncol(distances)) {
        
        ## create line end points
        xy_0 <- as.numeric(station_xyz[i,1:2])
        xy_1 <- as.numeric(station_xyz[j,1:2])
        
        ## calculate line length
        l_line <- sqrt((xy_0[1] - xy_1[1])^2 + 
                         (xy_0[2] - xy_1[2])^2)
        
        ## calculate number of nodes along which to interpolate
        n_line <- as.numeric(ceiling(l_line / pars$res_mean))
        
        ## account for legth zero
        if(n_line == 0) {
          
          n_line <- 1
        }
        
        ## interpolate direct distance from pixel to station
        z_direct <- seq(from = station_xyz[i,3], 
                        to = station_xyz[j,3], 
                        length.out = n_line)
        
        ## calculate wave path height
        z_line <- z_direct
        
        ## optionally, correct for topography effect
        if(pars$topography == TRUE) {
          
          ## calculate node coordinates
          x_line <- seq(from = xy_0[1], 
                        to = xy_1[1], 
                        length.out = n_line)  
          
          y_line <- seq(from = xy_0[2], 
                        to = xy_1[2], 
                        length.out = n_line)  
          
          xy_line <- sp::SpatialPoints(coords = cbind(x_line, y_line))
          
          # ## extract elevation along line
          z_surface <- sp::over(x = xy_line, 
                                y = dem_spdf)[,1] 
          
          ## get elevation values above surface
          z_above <- z_direct > z_surface
          
          ## correct for topography
          z_line[z_above == TRUE] <- z_surface[z_above]
        }
        
        ## get absolute path length
        distances[i,j] <- sqrt((x_line[n_line] - x_line[1])^2 +
                                 (y_line[n_line] - y_line[1])^2 +
                                 sum(diff(z_line))^2)
        
      }
    }
  } else {
    
    distances <- matrix(nrow = nrow(stations),
                        ncol = nrow(stations))
    rownames(distances) <- rownames(stations)
    colnames(distances) <- rownames(stations)
  }
  
  ## return distance matrices
  return(list(maps = D_map,
              stations = distances))
}