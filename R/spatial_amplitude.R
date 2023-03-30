#' Locate the source of a seismic event by modelling amplutide attenuation
#'
#' The function fits a model of signal amplitude attenuation for all grid
#' cells of the distance data sets and returns the residual sum as measure
#' of the most likely source location of an event.
#'
#' @param data \code{Numeric} matrix or \code{eseis} object, seismic signals
#' to work with. Since the function will calculate the maxima of the data it
#' is usually the envelopes of the data that should be used here.
#'
#' @param coupling \code{Numeric} vector, coupling efficiency factors for each
#' seismic station. The best coupled station (or the one with the highest
#' amplification) must receive \code{1}, the others must be scaled relatively
#' to this one.
#'
#' @param d_map \code{List} object, distance maps for each station. Output 
#' of \code{spatial_distance}.
#'
#' @param aoi \code{raster} object that defines which pixels are used to 
#' locate the source. If omitted, the entire distance map extent is used. 
#' \code{aoi} and \code{d_map} objects must have the same extents, 
#' projections and pixel sizes. The \code{aoi} map must be of logical values.
#'
#' @param v \code{Numeric} value, mean velocity of seismic waves (m/s).
#'
#' @param q \code{Numeric} value, quality factor of the ground.
#'
#' @param f \code{Numeric} value, frequency for which to model the
#' attenuation.
#'
#' @param a_0 \code{Logical} value, start parameter of the source amplitude,
#' if not provided, a best guess is made as 100 times the maximum amplitude
#' value of the data set.
#'
#' @param normalise \code{Logical} value, option to normalise sum of
#' residuals between 0 and 1. Default is \code{TRUE}.
#'
#' @param output \code{Character} value, type of metric the function returns.
#' One out of \code{"residuals"} (sums of the squared model residuals) or
#' \code{"variance"} (variance reduction, cf. Walter et al. (2017)). Default
#' is \code{"variance"}.
#'
#' @param cpu \code{Numeric} value, fraction of CPUs to use. If omitted, 
#' only one CPU will be used.
#'
#' @return A raster object with the location output metrics for each
#' grid cell.
#'
#' @author Michael Dietze
#'
#' @examples
#'
#' \dontrun{
#'
#'## create synthetic DEM
#'dem <- terra::rast(xmin = 0, xmax = 10000, 
#'                   ymin= 0, ymax = 10000, 
#'                   res = c(500, 500),
#'                   vals = rep(0, 400))
#'
#'## define station coordinates
#'sta <- data.frame(x = c(1000, 9000, 5000),
#'                  y = c(1000, 1000, 9000),
#'                  ID = c("A", "B", "C"))
#'
#'## create synthetic signal (source in towards lower left corner of the DEM)
#'s <- rbind(dnorm(x = 1:1000, mean = 500, sd = 50) * 100,
#'           dnorm(x = 1:1000, mean = 500, sd = 50) * 2,
#'           dnorm(x = 1:1000, mean = 500, sd = 50) * 1)
#'
#'## plot DEM and stations
#'terra::plot(dem)
#'text(x = sta$x, 
#'     y = sta$y, 
#'     labels = sta$ID)
#'
#'## calculate spatial distance maps and inter-station distances
#'D <- eseis::spatial_distance(stations = sta[,1:2],
#'                             dem = dem)
#'
#'## locate signal
#'e <- eseis::spatial_amplitude(data = s, 
#'                              d_map = D$maps, 
#'                              v = 500, 
#'                              q = 50, 
#'                              f = 10)
#'
#'## get most likely location coordinates (example contains two equal points)
#'e_max <- spatial_pmax(data = e)
#'
#'## plot output
#'terra::plot(e)
#'points(e_max[1], 
#'       e_max[2],
#'       pch = 20)
#'points(sta[,1:2])
#'
#' }
#'
#' @export spatial_amplitude

spatial_amplitude <- function (
    
  data,
  coupling,
  d_map,
  aoi,
  v,
  q,
  f,
  a_0,
  normalise = TRUE,
  output = "variance",
  cpu
) {
  
  ## check/format input data
  if(class(data)[1] != "list") {
    
    data_mat <- data
    data <- vector(mode = "list")
    
    for(i in 1:nrow(data_mat)) {
      
      data[[i]] <- list(signal = data_mat[i,])
    }
  }
  
  ## check/set coupling factors
  if(missing(coupling) == TRUE) {
    
    coupling <- rep(1, length(data))
  }
  
  ## get maximum amplitude
  a_d <- do.call(c, lapply(X = data, FUN = function(data) {
    
    max(data$signal)
  }))
  
  ## normalise by coupling efficiency
  a_d <- a_d * 1 / coupling
  
  ## check/set source amplitude
  if(missing(a_0) == TRUE) {
    
    a_0 <- 100 * max(a_d)
  }
  
  ## build raster sets from map meta data
  d_map <- lapply(X = d_map, FUN = function(x) {
    
    terra::rast(extent = x$ext,
                res = x$res, 
                crs = x$crs,
                vals = x$val)
  })
  
  ## convert distance data sets to matrix with distance values
  d <- do.call(cbind, lapply(X = d_map, FUN = terra::values))
  
  ## check if aoi is provided and create aoi index vector
  if(missing(aoi) == FALSE) {
    
    px_ok <- terra::values(aoi)
  } else {
    
    px_ok <- rep(1, nrow(d))
  }
  
  ## combine aoi flag and distance map values
  d <- cbind(px_ok, d)
  
  ## convert distance data to list
  d <- as.list(as.data.frame(t(d)))
  
  ## define amplitude function
  model_fun <- a_d ~ a_0 / sqrt(d) * exp(-((pi * f * d) / (q * v)))
  
  ## create model parameter list
  model_par <- list(a_d = a_d,
                    d = rep(NA, length(a_d)),
                    f = f,
                    q = q,
                    v = v)
  
  ## create model start parameter list
  model_start <- list(a_0 = a_0)
  
  ## initiate cluster
  cores <- parallel::detectCores()
  
  if(missing(cpu) == FALSE) {
    
    n_cpu <- floor(cores * cpu)
    cores <- ifelse(cores < n_cpu, cores, n_cpu)
  } else {
    
    cores <- 1
  }
  cl <- parallel::makeCluster(getOption("mc.cores", cores))
  
  ## model event amplitude as function of distance
  r <- parallel::parLapply(
    cl = cl,
    X = d,
    fun = function(d,
                   model_fun,
                   model_par,
                   model_start,
                   output) {
      
      process <- d[1]
      
      if(process == 1) {
        
        model_par$d <- d[-1]
        
        mod <- try(nls(formula = model_fun,
                       data = model_par,
                       start = model_start),
                   silent = TRUE)
        
        if(output == "variance") {
          res <- try(1 - (sum(residuals(object = mod)^2,
                              na.rm = TRUE) /
                            sum(model_par$a_d^2,
                                na.rm = TRUE)),
                     silent = TRUE)
          
        }
        
        if(output == "residuals") {
          
          res <- try(sum(residuals(mod)^2),
                     silent = TRUE)
        }
        
        if(class(res)[1] == "try-error") {
          
          res <- NA
        }
        
        return(res)
      } else {
        
        return(NA)
      }
    }, model_fun, model_par, model_start, output)
  
  ## stop cluster
  parallel::stopCluster(cl = cl)
  
  ## convert list to vector
  r <- do.call(c, r)
  
  ## optionally normalise data
  if(normalise == TRUE) {
    
    r <- try((r - min(r, na.rm = TRUE)) /
               (max(r, na.rm = TRUE) - min(r, na.rm = TRUE)))
  }
  
  ## convert data structure to raster object
  l <- try(d_map[[1]])
  try(terra::values(l) <- r)

  ## return output
  return(l)
}
