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
#' @param dem \code{SpatRaster} object, the digital elevation model (DEM) to 
#' be processed. The DEM must be in metric units, such as the UTM system and 
#' match with the reference system of the coordinates of \code{stations}. 
#' See \code{terra} for supported types and how to read these to R.
#' 
#' @param topography \code{Logical} scalar, option to enable topography
#' correction, default is \code{TRUE}.
#' 
#' @param maps \code{Logical} scalar, option to enable/disable calculation
#' of distance maps. Default is \code{TRUE}.
#' 
#' @param matrix \code{Logical} scalar, option to enable/disable calculation
#' of interstation distances. Default is \code{TRUE}.
#' 
#' @param aoi \code{Numeric} vector of length four, bounding coordinates of 
#' the area of interest to process, in the form \code{c(x0, x1, y0, y1)}.
#' 
#' @param verbose \code{Logical} value, option to show extended function 
#' information as the function is running. Default is \code{FALSE}.
#' 
#' @return \code{List} object with distance maps (list of 
#' \code{SpatRaster} objects from \code{terra} package) and station distance 
#' matrix (\code{data.frame}).
#' 
#' @author Michael Dietze
#' 
#' @keywords eseis
#' 
#' @examples
#' 
#' \dontrun{
#' 
#' data("volcano")
#' dem <- terra::rast(volcano)
#' dem <- dem * 10
#' terra::ext(dem) <- terra::ext(dem) * 10
#' terra::ext(dem) <-terra::ext(dem) + c(510, 510, 510, 510)
#' 
#' ## define example stations
#' stations <- cbind(c(200, 700), c(220, 700))
#' 
#' ## plot example data
#' terra::plot(dem)
#' points(stations[,1], stations[,2])
#' 
#' ## calculate distance matrices and stations distances
#' D <- spatial_distance(stations = stations, 
#'                       dem = dem)
#' 
## restore SpatRaster object for plotting purpose
#' D_map_1 <- terra::rast(crs = D$maps[[1]]$crs,
#'                        ext = D$maps[[1]]$ext,
#'                        res = D$maps[[1]]$res,
#'                        val = D$maps[[1]]$val)
#' 
#' ## plot distance map
#' terra::plot(D_map_1) 
#' 
#' ## show station distance matrix
#' print(D$matrix)
#' 
#' ## calculate with AOI and in verbose mode
#' D <- spatial_distance(stations = stations, 
#'                       dem = dem, 
#'                       verbose = TRUE,
#'                       aoi = c(0, 200, 0, 200))
#' 
#' ## plot distance map for station 2
#' terra::plot(D$maps[[1]])
#' 
#' } 
#'                                           
#' @export spatial_distance
spatial_distance <- function(
    stations,
    dem, 
    topography = TRUE,
    maps = TRUE,
    matrix = TRUE,
    aoi,
    verbose = FALSE
) {
  
  ## PART 0 - check input data ------------------------------------------------
  
  ## check that DEM is SpatRaster from terra package
  if(class(dem)[1] != "SpatRaster") {
    
    stop("DEM must be a SpatRaster, see package terra!")
  }
  
  if(any(is.na(terra::values(dem))) == TRUE) {
    
    stop("DEM contains NA values!")
  }
  
  ## extract coordinates from DEM
  xy_dem <- terra::crds(dem)
  
  ## check if station coordinates are within DEM extent
  if(any(min(stations[,1]) < min(xy_dem[,1]),
         max(stations[,1]) > max(xy_dem[,1]),
         min(stations[,2]) < min(xy_dem[,2]),
         max(stations[,2]) > max(xy_dem[,2])) == TRUE) {
    
    stop("Some station coordinates are outside DEM extent!")
  }
  
  ## check/set aoi extent
  if(missing(aoi) == TRUE) {
    
    aoi_ext <- c(min(xy_dem[,1]), 
                 max(xy_dem[,1]),
                 min(xy_dem[,2]), 
                 max(xy_dem[,2]))
  } else {
    
    aoi_ext <- aoi
  }
  
  ## check that aoi is within DEM extent
  if(any(aoi_ext[1] < min(xy_dem[,1]),
         aoi_ext[2] > max(xy_dem[,1]),
         aoi_ext[3] < min(xy_dem[,2]),
         aoi_ext[4] > max(xy_dem[,2])) == TRUE) {
    
    stop("AOI extent is beyond DEM extent!")
  }
  
  ## PART 1 - calculate distance maps -----------------------------------------
  
  if(maps == TRUE) {
    
    ## define aoi raster and set values to zero
    aoi_rst <- dem
    terra::values(aoi_rst) <- 0
    
    ## extract aoi coordinates  
    aoi_xy <- terra::crds(aoi_rst)
    
    ## adjust values due to aoi extent
    terra::values(aoi_rst)[aoi_xy[,1] >= aoi_ext[1] & 
                    aoi_xy[,1] <= aoi_ext[2] &
                    aoi_xy[,2] >= aoi_ext[3] &
                    aoi_xy[,2] <= aoi_ext[4]] <- 1
    
    ## create output object
    maps <- vector(mode = "list", length = nrow(stations))
    
    ## process each station
    for(i in 1:nrow(stations)) {
      
      ## optionally print info
      if(verbose == TRUE) {
        
        ## print progress
        print(paste("Processing map for station", i))
      }
      
      ## get station coordinates
      xy_stat <- stations[i,]
      
      ## get distance map entries
      d <- do.call(c, lapply(
        X = 1:nrow(xy_dem), 
        FUN = function(j, xy_stat, xy_dem, dem, aoi_rst, topography) {
          
          if(terra::values(aoi_rst)[j] == 1) {
            
            ## get line length
            l_line <- sqrt((xy_stat[1] - xy_dem[j,1])^2 + 
                             (xy_stat[2] - xy_dem[j,2])^2)
            
            ## get number of points to interpolate along
            n_int <- round(l_line / mean(res(dem)))
            
            ## account for zero (pixel at station)
            if(n_int == 0) {
              n_int <- 1
            }
            
            ## create points along line
            xy_pts <- cbind(seq(from = as.numeric(xy_stat[1]), 
                                to = xy_dem[j,1], 
                                length.out = n_int),
                            seq(from = as.numeric(xy_stat[2]), 
                                to = xy_dem[j,2], 
                                length.out = n_int))
            
            ## create straight line from station to pixel
            xy_line <- terra::vect(x = as.matrix(xy_pts),
                                   type = "points")
            
            ## extract elevation data along line
            z_int <- terra::extract(x = dem, 
                                    y = xy_line,
                                    method = "bilinear",
                                    xy = TRUE)
            
            ## interpolate straight line elevation
            z_dir <- seq(from = z_int[,2][1], 
                         to = tail(z_int[,2], 1),
                         length.out = n_int)
            
            ## optionally calculate along elevation path
            if(topography == TRUE) {
              
              i_dir <- z_dir > z_int[,2] 
              
              z_dir[i_dir] <- z_int[,2][i_dir]
            }
            
            ## calculate path length
            l <- sqrt((xy_stat[1] - xy_dem[j,1])^2 + 
                        (xy_stat[2] - xy_dem[j,2])^2 + 
                        sum(abs(c(0, diff(z_dir))))^2)
          } else {
            
            l <- NA
          }
          
          ## return output
          return(as.numeric(l))
          
        }, xy_stat, xy_dem, dem, aoi_rst, topography))
      
      ## create output map and fill it with distance values
      maps[[i]] <- dem
      terra::values(maps[[i]]) <- d
    }
  } else {
    
    maps <- vector(mode = "list", length = nrow(stations))
  }
  
  ## create output station distance matrix
  M <- matrix(nrow = nrow(stations),
              ncol = nrow(stations))
  rownames(M) <- rownames(stations)
  colnames(M) <- rownames(stations)
  
  if(matrix == TRUE) {
    
    ## optionally, print info
    if(verbose == TRUE) {
      
      ## print progress
      print("Processing station distances")
    }
    
    ## create straight line from station to pixel
    xy_stat <- terra::vect(x = as.matrix(stations),
                           type = "points")
    
    ## extract elevation data along line
    xyz_stat <- terra::extract(x = dem, 
                               y = xy_stat,
                               method = "bilinear",
                               xy = TRUE)[,c(3, 4, 2)]
    names(xyz_stat)[3] <- "z"
    
    ## loop through all stations
    for(i in 1:nrow(xyz_stat)) {
      
      ## calculate euclidian xy-distances between stations
      dx_stations <- xyz_stat$x - xyz_stat$x[i]
      dy_stations <- xyz_stat$y - xyz_stat$y[i]
      dt_stations <- sqrt(dx_stations^2 + dy_stations^2)
      
      ## loop through all stations
      for(j in 1:length(dt_stations)) {
        
        
        ## get number of points to interpolate along
        n_int <- round(dt_stations[j] / mean(res(dem)))
        
        ## account for zero (pixel at station)
        if(n_int == 0) {
          n_int <- 1
        }
        
        ## create points along line
        xy_pts <- cbind(seq(from = xyz_stat$x[i], 
                            to = xyz_stat$x[j], 
                            length.out = n_int),
                        seq(from = xyz_stat$y[i], 
                            to = xyz_stat$y[j], 
                            length.out = n_int))
        
        ## create straight line from station to pixel
        xy_line <- terra::vect(x = xy_pts,
                               type = "points")
        
        ## extract elevation data along line
        z_int <- terra::extract(x = dem, 
                                y = xy_line,
                                method = "bilinear",
                                xy = TRUE)
        
        ## interpolate straight line elevation
        z_dir <- seq(from = z_int[,2][1], 
                     to = tail(z_int[,2], 1), 
                     length.out = n_int)
        
        ## optionally calculate along elevation path
        if(topography == TRUE) {
          
          i_dir <- z_dir > z_int[,2] 
          
          z_dir[i_dir] <- z_int[,2][i_dir]
        }
        
        
        ## calculate path length and assign it to output data set
        M[i,j] <- sum(sqrt(diff(xy_pts[,1])^2 +
                             diff(xy_pts[,2])^2 + 
                             diff(z_dir)^2))
      }
    }
  }
  
  ## convert maps to lists of raster meta data
  maps <- lapply(X = maps, FUN = function(maps) {
    
    list(crs = terra::crs(maps),
         ext = as.numeric(terra::ext(maps)[1:4]),
         res = as.numeric(terra::res(maps)),
         val = as.numeric(terra::values(maps)))
  })
  
  ## return output
  return(list(maps = maps,
              matrix = M))
}