#' Track a spatially mobile seismic source
#' 
#' This function allows tracking a spatially mobile seismic source and 
#' thereby estimating the source amplitude and the model's variance 
#' reduction as a measure of quality or robustness of the time-resolved 
#' estimates.
#' 
#' The method is based on ideas published by Burtin et al. (2016), 
#' Walter et al. 82017) and Perez-Guillen et al. (2019) and implemented 
#' in the R package eseis by Dietze (2018). It is related to the function
#' \code{spatial_amplitude}, which can be used to locate spatially 
#' stable seismic sources by the same technique, and it resuires 
#' prepared input data as delivered by the function 
#' \code{spatial_distance}.
#' 
#' The input data (\code{data}) should ideally be a list of eseis 
#' objects (alternatively a matrix with seismic signal traces) containing 
#' the envelopes of the seismic event to track (i.e., describe by its 
#' location and amplitude as a function of propagation time). The temporal 
#' resolution of the track is defined by the arguments \code{window} and 
#' \code{overlap} (as a fraction between 0 and 1). The approach is based on 
#' fitting known amplitude-distance functions (for an overview of available 
#' functions see \code{model_amplitude}) to the envelope time snippets 
#' for each pixel of a grid, which provides the distance from a pixel to 
#' each seismic station, i.e., the distance map set \code{d_map}. To avoid 
#' fitting each of the pixels of the distance map, one can provide an area
#' of interest, AOI (\code{aoi}), which has the same extent and resolution 
#' as the distance map set and pixel values are either \code{TRUE} or 
#' \code{FALSE}. Depending on which amplitude-distance function is chosen, 
#' further arguments need to be provided (ground quality factor \code{q},
#' center frequency of the signal \code{f}). The apparent seismic wave 
#' velocity \code{v} is required regardless, either as fit model parameter 
#' or to correct the amplitude time snippets for the travel time delay from 
#' the source to the respective pixel of the distance map set. The output 
#' of the function can be provided with uncertainty estimates on all output 
#' values. The uncertainty is based on the size of accepted location 
#' estimates per time step, as defined by the variance reduction quantile 
#' threshold \code{qt} (i.e., all locations above this quantile will be 
#' assumed to be valid location estimates, whose parameters will be used 
#' to estimate the uncertainty). Note that usually, \code{qt} should be 
#' set to around 0.99, a value that depends on the number of pixels in 
#' the distance map set and that affects the location uncertainty, which 
#' in many cases is about 10 % of the average seismic station spacing. 
#' Note however, that this value is purely arbitrary and should be based 
#' on field-based control data. It is possible to run the function in a 
#' multi-CPU mode, to speed up computational time, using the argument 
#' \code{cpu}. Also, the function can generate generic plot output of 
#' the results, a panel of three plots: source trajectory, source 
#' amplitude and variance reduction.
#' 
#' Note that depending on the resolution of the distance map set, number 
#' of included seismic stations, and number of time windows, the 
#' function can take significant processing time. 50 time steps for 5 
#' stations and 5000 pixels per distance map requires about 10 minutes 
#' time on a normal grade computer using a single CPU.
#' 
#' @section References:
#' 
#' Burtin, A., Hovius, N., and Turowski, J. M.: Seismic monitoring of 
#' torrential and fluvial processes, Earth Surf. Dynam., 4, 285–307, 
#' https://doi.org/10.5194/esurf-4-285-2016, 2016. 
#' 
#' Dietze, M.: The R package 'eseis' – a software toolbox for environmental
#'  seismology, Earth Surf. Dynam., 6, 669–686, 
#'  https://doi.org/10.5194/esurf-6-669-2018, 2018. 
#' 
#' Perez-Guillen, C., Tsunematsu, K., Nishimura, K., and Issler, D.: Seismic 
#' location and tracking of snow avalanches and slush flows on Mt. Fuji, 
#' Japan, Earth Surf. Dynam., 7, 989–1007, 
#' https://doi.org/10.5194/esurf-7-989-2019, 2019. 
#' 
#' Walter, F., Burtin, A., McArdell, B. W., Hovius, N., Weder, B., and 
#' Turowski, J. M.: Testing seismic amplitude source location for fast
#' debris-flow detection at Illgraben, Switzerland, Nat. Hazards Earth 
#' Syst. Sci., 17, 939–955, https://doi.org/10.5194/nhess-17-939-2017, 2017. 
#' 
#' @param data \code{Numeric} matrix or \code{eseis} object, seismic signals 
#' used for source tracking. Note that the function will start tracking 
#' within a smaller time window, narrows be the maximum signal arrival time 
#' differences as defined by the maximum inter station distance and the 
#' seismic velocity. The signals should be the envelopes of waveforms.
#' 
#' @param window \code{Numeric} value, time window for which the source is 
#' tracked. If omitted, ten time steps are generated.
#' 
#' @param overlap \code{Numeric} value between 0 and 1, fraction of overlap 
#' of time windows used for source tracking. Default is \code{0}.
#' 
#' @param coupling \code{Numeric}  vector, coupling efficiency factors for 
#' each seismic station. The best coupled station (or the one with the 
#' highest amplification) must receive 1, the others must be scaled 
#' relatively to this one.Numeric vector, coupling efficiency factors for 
#' each seismic station. The best coupled station (or the one with the 
#' highest amplification) must receive 1, the others must be scaled 
#' relatively to this one.
#' 
#' @param d_map \code{List} object, distance maps for each station (i.e., 
#' \code{SpatialGridDataFrame} objects). Output of \code{spatial_distance}.
#' 
#' @param aoi \code{Raster} object (optional) that defines which pixels are 
#' used to locate the source. If omitted, the entire distance map extent is  
#' used. \code{aoi} and \code{d_map} objects must have the same extents, 
#' projections and pixel sizes. The aoi map must be of logical values.
#' 
#' @param v \code{Numeric} value, mean velocity of seismic waves (m/s).
#' 
#' @param q \code{Numeric} value, quality factor of the ground.
#' 
#' @param f \code{Numeric} value, frequency for which to model the 
#' attenuation.
#' 
#' @param k \code{Numeric} value, fraction of surface wave contribution to 
#' signals. Only relevant for models that include mixture of surface and 
#' body waves (see \code{model_amplitude}).
#' 
#' @param qt \code{Numeric} value, quantile threshold that defines acceptable
#' location estimates. Default is \code{1} (only single best estimate is 
#' kept). 
#' 
#' @param dt \code{Numeric} value, sampling frequency. Only required if 
#' input signals are no eseis objects.
#' 
#' @param model \code{Character} value,
#' 
#' @param cpu \code{Numeric} value, fraction of CPUs to use for parallel 
#' processing. If omitted, one CPU is used
#' 
#' @param verbose \code{Logical} value, optional screen output of processing
#' progress. Default is FALSE.
#' 
#' @param plot \code{Logical} value, enable graphical output of key results. 
#' Default is \code{FALSE}.
#' 
#' @return A \code{List} object with summarising statistics of the fits.
#' 
#' @examples 
#' 
#' \dontrun{
#' 
#' x <- spatial_track(data = data, 
#'                    window = 5, 
#'                    overlap = 0.5,
#'                    d_map = D$maps, 
#'                    aoi = aoi, 
#'                    v = 800, 
#'                    q = 40, 
#'                    f = 12, 
#'                    qt = 0.99)
#' 
#' }
#' 
#' @export spatial_track

spatial_track <- function(
  
  data,
  coupling,
  window,
  overlap = 0,
  d_map,
  aoi,
  v,
  q,
  f,
  k,
  qt = 1,
  dt,
  model = "SurfSpreadAtten",
  cpu,
  verbose = FALSE,
  plot = FALSE
  
) {
  
  ## check/convert input signal structure
  if (class(data)[1] != "list") {
    
    ## check for presence of dt
    if(missing(dt) == TRUE) {
      
      stop("Input data no eseis object and dt is missing!")
    }
    
    ## create artificial time vector
    t_s <- seq(from = Sys.time(),
               by = dt, 
               length.out = ncol(data))
    
    ## store backup
    data_mat <- data
    
    ## convert matrix to list
    data <- vector(mode = "list")
    for (i in 1:nrow(data_mat)) {
      data[[i]] <- list(signal = data_mat[i, ],
                        meta = list(dt = dt))
    }
  } else {
    
    ## get dt from eseis object's meta data
    dt <- do.call(c, lapply(X = data, FUN = function(x) {
      
      x$meta$dt
    }))
    
    ## check/assign dt
    if(sd(dt) > 0) {
      
      stop("Sampling frequencies of data sets are different!")
    } else {
      
      dt <- mean(dt)
    }
    
    ## create time vector
    t_s <- seq(from = data[[1]]$meta$starttime, 
               by = dt,
               length.out = data[[1]]$meta$n)
  }
  
  ## check/set coupling factors
  if (missing(coupling) == TRUE) {
    coupling <- rep(1, length(data))
  }
  
  ## check/create aoi
  if(missing(aoi) == TRUE) {
    
    aoi <- d_map[[1]] * 0 + 1
  } else {if(class(aoi)[1] != "SpatRaster") {
    
    stop("AOI must be of class SpatRaster!")
  }}
  
  ## get maximum possible time delay across distance maps
  d_all <- do.call(c, lapply(X = d_map, FUN = function(d_map, aoi) {
    
    values(d_map * aoi)
  }, aoi))
  t_diff <- max(d_all, na.rm = TRUE) / v
  
  ## optionally print verbose output
  if(verbose == TRUE) {
    
    print(paste0("Maximum possible time delay: ", t_diff, " s."))
  }
  
  ## create time window vector
  t_step <- seq(from = min(t_s) + t_diff, 
                to = max(t_s) - t_diff - window,
                by = window * (1 - overlap))
  
  ## optionally print verbose output
  if(verbose == TRUE) {
    
    print(paste0(length(t_step), " windows of ", window, 
                 " s length to process."))
  }
  
  ## convert distance data sets to matrix with distance values
  d <- do.call(cbind, lapply(X = d_map,
                             FUN = function(d_map) {d_map@data[,1]}))
  
  ## create and append aoi index vector
  d <- cbind(terra::values(aoi), d)
  
  ## convert distance data to list
  d <- as.list(as.data.frame(t(d)))
  
  ## optionally print verbose output
  if(verbose == TRUE) {
    
    print(paste0(sum(terra::values(aoi)), 
                 " pixels in AOI to process (total number of pixels: ", 
                 length(terra::values(aoi)), ")."))
  }
  
  ## define amplitude function and model parameters
  if(model == "SurfSpreadAtten") {
    
    model_fun <- a_d ~ a_0 / sqrt(d) * exp(-((pi * f * d) / (q * v)))
    model_par <- list(f = f,
                      q = q,
                      v = v)
    
  } else if(model == "BodySpreadAtten") {
    
    model_fun <- a_d ~ a_0 / d * exp(-((pi * f * d) / (q * v)))
    model_par <- list(f = f,
                      q = q,
                      v = v)
    
  } else if(model == "SurfBodySpreadAtten") {
    
    model_fun <- a_d ~ (k * a_0 / sqrt(d) * exp(-((pi * f * d) / (q * v)))) + 
      ((1 - k) * a_0 / d * exp(-((pi * f * d) / (q * v))))
    model_par <- list(f = f,
                      q = q,
                      v = v,
                      k = k)
    
  } else if(model == "BodySpread") {
    
    model_fun <- a_d ~ a_0 / sqrt(d)
    model_par <- list()
    
  } else if(model == "BodySpread") {
    
    model_fun <- a_d ~ a_0 / d
    model_par <- list()
    
  } else if(model == "SurfBodySpread") {
    
    model_fun <- a_d ~ k * a_0 / sqrt(d) + (1 - k) * a_0 / d
    model_par <- list(k = k)
  } 
  
  ## optionally print verbose output
  if(verbose == TRUE) {
    
    print(paste0("Using model ", model, " (", deparse(model_fun), ")"))
  }
  
  ## detect and adjust number of cores to use
  cores <- parallel::detectCores()
  
  if(missing(cpu) == FALSE) {
    
    n_cpu <- floor(cores * cpu)
    cores <- ifelse(cores < n_cpu, cores, n_cpu)
  } else {
    
    cores <- 1
  }
  
  ## optionally print verbose output
  if(verbose == TRUE) {
    
    print(paste(cores, "cores are used."))
  }
  
  ## initiate cluster
  cl <- parallel::makeCluster(getOption("mc.cores", cores))
  
  ## create output object
  l_step <- vector(mode = "list", 
                   length = length(t_step))
  
  ## optionally print verbose output
  if(verbose == TRUE) {
    
    print("start processing...")
  }
  
  ## loop through time windows
  for(j in 1:length(l_step)) {
    
    t_0 <- t_step[j]
    
    ## do grid processing
    r <- parallel::parLapply(
      cl = cl, 
      X = d, 
      fun = function(d_i, model_fun, model_par, t_0, data, window, verbose) {
        
        ## check if location is in AOI
        process <- d_i[1]
        
        if(process == 1) {
          
          ## convert distance to time shifts
          t_shift <- try(t_0 + d_i[-1] / model_par$v, 
                         silent = !verbose)
          
          ## clip signals to adjusted time windows and get maximum amplitude
          a_window <- rep(NA, length(data))
          try(for(i in 1:length(a_window)) {
            
            a_window[i] <- 
              try(max(eseis::signal_clip(data = data[[i]], 
                                         limits = c(t_shift[i] + dt,
                                                    t_shift[i] + 
                                                      window - dt))$signal), 
                  silent = !verbose)
          })
          
          ## update model parameters
          model_par$a_d = a_window
          
          ## assign amplitudes and distances for location of interest
          model_par$d <- d_i[-1]
          
          ## define start parameter based on amplitudes
          model_start = try(list(a_0 = 100 * max(model_par$a_d)),
                            silent = !verbose)
          
          ## model source amplitude
          mod <- try(minpack.lm::nlsLM(formula = model_fun,
                                       data = model_par,
                                       start = model_start),
                     silent = !verbose)
          
          ## get source amplitude
          mod_a_0 <- try(coefficients(mod), silent = !verbose)
          mod_a_0 <- ifelse(test = class(mod_a_0)[1] != "try-error", 
                            yes = mod_a_0, 
                            no = NA)
          
          ## get residuals
          mod_res <- try(sqrt(mean(residuals(mod)^2, 
                                   na.rm = TRUE)),
                         silent = !verbose)
          mod_res <- ifelse(test = class(mod_res)[1] != "try-error", 
                            yes = mod_res, 
                            no = NA)
          
          ## get variance reduction
          mod_var <- try(1 - (sum(residuals(object = mod)^2,
                                  na.rm = TRUE) /
                                sum(model_par$a_d^2,
                                    na.rm = TRUE)),
                         silent = !verbose)
          mod_var <- ifelse(test = class(mod_var)[1] != "try-error", 
                            yes = mod_var, 
                            no = NA)
          
          ## return output
          return(list(mod_a_0 = mod_a_0,
                      mod_res = mod_res,
                      mod_var = mod_var))
        } else {
          
          return(list(mod_a_0 = NA,
                      mod_res = NA,
                      mod_var = NA))
        }
        
      }, model_fun, model_par, t_0, data, window, verbose)
    
    ## convert list to output vectors
    mod_a_0 <- try(do.call(c, lapply(X = r, FUN = function(r){r$mod_a_0})), 
                   silent = !verbose)
    mod_res <- try(do.call(c, lapply(X = r, FUN = function(r){r$mod_res})), 
                   silent = !verbose)
    mod_var <- try(do.call(c, lapply(X = r, FUN = function(r){r$mod_var})), 
                   silent = !verbose)
    
    ## get grid coordinates
    xy <- try(terra::crds(d_map[[1]]), silent = !verbose)
    
    ## combine output
    l_i <- try(data.frame(x = xy[,1], 
                          y = xy[,2], 
                          a_0 = mod_a_0, 
                          res = mod_res, 
                          var = mod_var), silent = !verbose)
    
    ## sort by variance reduction
    l_i <- try(l_i[order(l_i$var, decreasing = TRUE),], silent = !verbose)
    
    ## isolate quantile of interest
    l_qt <- try(stats::na.omit(l_i), silent = !verbose)
    l_qt <- try(l_qt[l_qt$var >= quantile(x = l_qt$var, 
                                          qt, 
                                          na.rm = TRUE),], 
                silent = !verbose)
    
    try(if(nrow(l_qt) == 1) {
      
      l_qt <- rbind(l_qt, l_qt)
    }, silent = !verbose)
    
    ## calculate summary statistics for l_qt values
    l_qt_mean <- try(apply(X = l_qt, MARGIN = 2, FUN = mean), 
                     silent = !verbose)
    l_qt_sd <- try(apply(X = l_qt, MARGIN = 2, FUN = sd),
                   silent = !verbose)
    l_qt_q05 <- try(apply(X = l_qt, MARGIN = 2, FUN = quantile, 0.05), 
                    silent = !verbose)
    l_qt_q25 <- try(apply(X = l_qt, MARGIN = 2, FUN = quantile, 0.25), 
                    silent = !verbose)
    l_qt_q50 <- try(apply(X = l_qt, MARGIN = 2, FUN = quantile, 0.50), 
                    silent = !verbose)
    l_qt_q75 <- try(apply(X = l_qt, MARGIN = 2, FUN = quantile, 0.75),
                    silent = !verbose)
    l_qt_q95 <- try(apply(X = l_qt, MARGIN = 2, FUN = quantile, 0.95), 
                    silent = !verbose)
    
    ## return output
    l_step[[j]] <- try(list(mean = l_qt_mean,
                            sd = l_qt_sd,
                            q05 = l_qt_q05,
                            q25 = l_qt_q25,
                            q50 = l_qt_q50,
                            q75 = l_qt_q75,
                            q95 = l_qt_q95), 
                       silent = !verbose)
    
    ## optionally print verbose output
    if(verbose == TRUE) {
      
      print(paste0("  Time step ", j, " finished."))
    }
  }
  
  ## stop cluster
  parallel::stopCluster(cl = cl)
  
  ## restructure output
  l_out <- l_step[[1]]
  
  if(inherits(x = l_out, what = "try-error") == FALSE) {
    
    l_out$mean <- lapply(X = l_step, FUN = function(x) {
      
      x$mean
    })
    l_out$mean <- as.data.frame(do.call(rbind, l_out$mean))
    
    l_out$sd <- lapply(X = l_step, FUN = function(x) {
      
      x$sd
    })
    l_out$sd <- as.data.frame(do.call(rbind, l_out$sd))
    
    l_out$q05 <- lapply(X = l_step, FUN = function(x) {
      
      x$q05
    })
    l_out$q05 <- as.data.frame(do.call(rbind, l_out$q05))
    
    l_out$q25 <- lapply(X = l_step, FUN = function(x) {
      
      x$q25
    })
    l_out$q25 <- as.data.frame(do.call(rbind, l_out$q25))
    
    l_out$q50 <- lapply(X = l_step, FUN = function(x) {
      
      x$q50
    })
    l_out$q50 <- as.data.frame(do.call(rbind, l_out$q50))
    
    l_out$q75 <- lapply(X = l_step, FUN = function(x) {
      
      x$q75
    })
    l_out$q75 <- as.data.frame(do.call(rbind, l_out$q75))
    
    l_out$q95 <- lapply(X = l_step, FUN = function(x) {
      
      x$q95
    })
    l_out$q95 <- as.data.frame(do.call(rbind, l_out$q95))
    
    l_out$time <- t_step
  }
  
  ## optionally generate plot output
  if(plot == TRUE)  {
    
    ## save old parameters
    par_old <- par(no.readonly = TRUE)
    
    ## define new plot parameters
    par(mfcol = c(1, 3))
    
    ## define plot colours
    cl_plot <- grDevices::heat.colors(length(l_out$mean$x))
    
    ## plot trajectory
    plot(NA,
         xlim = range(c(l_out$mean$x - l_out$sd$x,
                        l_out$mean$x + l_out$sd$x)),
         ylim = range(c(l_out$mean$y - l_out$sd$y,
                        l_out$mean$y + l_out$sd$y)),
         main = "Trajectory",
         xlab = "Easting",
         ylab = "Northing")
    
    lines(x = l_out$mean$x, 
          y = l_out$mean$y)
    
    points(x = l_out$mean$x,
           y = l_out$mean$y,
           pch = 20,
           col = cl_plot)
    segments(x0 = l_out$mean$x - l_out$sd$x, 
             y0 = l_out$mean$y,
             x1 = l_out$mean$x + l_out$sd$x, 
             y1 = l_out$mean$y,
             col = cl_plot)
    segments(x0 = l_out$mean$x, 
             y0 = l_out$mean$y - l_out$sd$y,
             x1 = l_out$mean$x, 
             y1 = l_out$mean$y + l_out$sd$y,
             col = cl_plot)
    
    ## plot source amplitude
    plot(x = l_out$time, 
         y = rep(NA, length(l_out$time)),
         ylim = range(c(l_out$mean$a_0 + l_out$sd$a_0,
                        l_out$mean$a_0 - l_out$sd$a_0)),
         main = "Source amplitude",
         xlab = "Time",
         ylab = "amplitude")
    
    polygon(x = c(l_out$time, rev(l_out$time)),
            y = c(l_out$mean$a_0 + l_out$sd$a_0,
                  rev(l_out$mean$a_0 - l_out$sd$a_0)),
            border = NA, 
            col = "grey80")
    
    lines(x = l_out$time, 
          y = l_out$mean$a_0)
    
    points(x = l_out$time,
           y = l_out$mean$a_0,
           pch = 20,
           col = cl_plot)
    
    ## plot variance reduction
    plot(x = l_out$time, 
         y = rep(NA, length(l_out$time)),
         ylim = 100 * range(c(l_out$mean$var + l_out$sd$var,
                              l_out$mean$var - l_out$sd$var)),
         main = "Variance reduction",
         xlab = "Time",
         ylab = "%")
    
    polygon(x = c(l_out$time, rev(l_out$time)),
            y = 100 * c(l_out$mean$var + l_out$sd$var,
                        rev(l_out$mean$var - l_out$sd$var)),
            border = NA, 
            col = "grey80")
    
    lines(x = l_out$time, 
          y = 100 * l_out$mean$var)
    
    points(x = l_out$time,
           y = 100 * l_out$mean$var,
           pch = 20,
           col = cl_plot)
    
    ## restore initial plot parameters
    par(par_old)
  }
  
  return(l_out)
}