#' Model source amplitude by amplitude-distance model fitting
#'
#' The function fits one of several models of signal amplitude attenuation and
#' returns a set of model parameters, including the source amplitude (a_0).
#' 
#' Depending on the choice of the model to fit, several parameters can 
#' (or should) be provided, e.g. \code{f},\code{q}, \code{v}, \code{k}, 
#' and most importantly, \code{a_0}. 
#' If more signals than free parameters are available, the missing 
#' parameters may be estimated during the fit, but without any checks 
#' of quality and meaningfulness. The parameter \code{a_0} will be 
#' defined as 100 times the maximum input amplitude, by default. The 
#' parameters \code{f} will be set to 10 Hz, \code{q} to 50, \code{v} 
#' to 1000 m/s and \code{k} to 0.5.
#' 
#' ISSUES: account for non-fixed parameters, especially k
#' 
#' The following amplitude-distance models are available:
#' 
#' \itemize{
#'   \item \code{"SurfSpreadAtten"}, Surface waves including geometric 
#'   spreading and unelastic attenuation
#'   \item \code{"BodySpreadAtten"}, Body waves including geometric 
#'   spreading and unelastic attenuation
#'   \item \code{"SurfBodySpreadAtten"}, Surface and body waves including  
#'   geometric spreading and unelastic attenuation
#'   \item \code{"SurfSpread"}, Surface waves including geometric 
#'   spreading, only
#'   \item \code{"BodySpread"}, Body waves including geometric 
#'   spreading, only
#'   \item \code{"SurfBodySpread"}, Surface and body waves including  
#'   geometric spreading, only
#' }
#' 
#' **SurfSpreadAtten**
#' The model is based on Eq. 17 from Burtin et al. (2016):
#' 
#' \deqn{a_d = a_0 / sqrt(d) * exp(-(pi * f * d) / (q * v))}
#' 
#' where a_0 is the source amplitude, a_d the amplitude as recorded by a sensor 
#' at distance d, f is the center frequency of the signal, q the ground quality 
#' factor and v the seismic wave velocity.
#' 
#' **BodySpreadAtten**
#' The model is based on Eq. 16 from Burtin et al. (2016):
#' 
#' \deqn{a_d = a_0 / d * exp(-(pi * f * d) / (q * v))}
#' 
#' where a_0 is the source amplitude, a_d the amplitude as recorded by a sensor 
#' at distance d, f is the center frequency of the signal, q the ground quality 
#' factor and v the seismic wave velocity.
#' 
#' **SurfBodySpreadAtten**
#' The model based on Eqs. 16 and 17 from Burtin et al. (2016):
#' 
#' \deqn{a_d = k * a_0 / sqrt(d) * exp(-(pi * f * d) / (q * v)) + (1 - k) * a_0 / d * exp(-(pi * f * d) / (q * v))}
#' 
#' where a_0 is the source amplitude, a_d the amplitude as recorded by a sensor 
#' at distance d, f is the center frequency of the signal, q the ground quality 
#' factor, v the seismic wave velocity, and n and m two factors determining the 
#' relative contributions of the two wave types, thus summing to 1.
#' 
#' **BodySpread**
#' The model is simply accounting for geometric spreading
#' 
#' \deqn{a_d = a_0 / d}
#' 
#' where a_0 is the source amplitude, a_d the amplitude as recorded by a sensor 
#' at distance d.
#' 
#' **SurfSpread**
#' The model is simply accounting for geometric spreading
#' 
#' \deqn{a_d = a_0 / sqrt(d)}
#' 
#' where a_0 is the source amplitude, a_d the amplitude as recorded by a sensor 
#' at distance d.
#' 
#' **SurfBodySpread**
#' The model is simply accounting for geometric spreading
#' 
#' \deqn{a_d = k * (a_0 / d) + (1 - k) * a_d / sqrt(d)}
#' 
#' where a_0 is the source amplitude, a_d the amplitude as recorded by a sensor 
#' at distance d, and n and m two factors determining the relative 
#' contributions of the two wave types, thus summing to 1.
#' 
#' **References**
#' - Burtin, A., Hovius, N., and Turowski, J. M.: Seismic monitoring of 
#'   torrential and fluvial processes, Earth Surf. Dynam., 4, 285–307, 
#'   https://doi.org/10.5194/esurf-4-285-2016, 2016. 
#'
#' @param data \code{Numeric} matrix or \code{eseis} object, seismic signals
#' to work with. Since the function will calculate the maxima of the data it
#' is usually the envolopes of the data that should be used here. In an 
#' extreme case, a vector with just the maximum amplitudes recorded at each 
#' station can be provided, as well.
#' 
#' @param model \code{Character} value, model to fit the data. One out of the 
#' list in the details section. Default is \code{"SurfSpreadAtten"}.
#' 
#' @param distance \code{Numeric} vector with distance of station locations
#' to source. Alternatively, the distance can be calculated by providing the 
#' source coordinates (\code{xy}) and distance maps (\code{d_map})
#' 
#' @param source \code{Numeric} vector of length two, location of the seismic 
#' source to model (x and y coordinates).
#' 
#' @param d_map \code{List} object, distance maps for each station (i.e.,
#' \code{SpatialGridDataFrame} objects). Output of \code{distance_map}.
#'
#' @param coupling \code{Numeric} vector, coupling efficiency factors for each
#' seismic station. The best coupled station (or the one with the highest
#' amplification) must receive \code{1}, the others must be scaled relatively
#' to this one.
#'
#' @param v \code{Numeric} value, mean velocity of seismic waves (m/s). 
#' Only relevant for models accounting for unelastic attenuation (see 
#' details).
#'
#' @param q \code{Numeric} value, quality factor of the ground.
#' Only relevant for models accounting for unelastic attenuation (see 
#' details).
#'
#' @param f \code{Numeric} value, frequency for which to model the
#' attenuation. Only relevant for models accounting for unelastic attenuation 
#' (see details).
#' 
#' @param k \code{Numeric} value, fraction of surface wave contribution to 
#' signals. Only relevant for models that include mixture of surface and 
#' body waves (see details).
#'
#' @param a_0 \code{Logical} value, start parameter of the source amplitude,
#' if not provided, a best guess is made as 100 times the maximum amplitude
#' value of the data set.
#'
#'
#' @return \code{List} with model results, including \code{a_0} (source 
#' amplitude), \code{residuals} (model residuals), \code{coefficients} 
#' model coefficients.
#'
#' @author Michael Dietze
#'
#' @examples
#'
#' \dontrun{
#' 
#'  ## create synthetic DEM
#' dem <- terra::rast(nrows = 20, ncols = 20, 
#'                    xmin = 0, xmax = 10000, 
#'                    ymin= 0, ymax = 10000, 
#'                    vals = rep(0, 400))
#' 
#' ## define station coordinates
#' sta <- data.frame(x = c(1000, 9000, 5000, 9000),
#'                   y = c(1000, 1000, 9000, 9000),
#'                   ID = c("A", "B", "C", "D"))
#'  
#' ## create synthetic signal (source in towards lower left corner of the DEM)
#' s <- rbind(dnorm(x = 1:1000, mean = 500, sd = 50) * 50,
#'            dnorm(x = 1:1000, mean = 500, sd = 50) * 2,
#'            dnorm(x = 1:1000, mean = 500, sd = 50) * 1,
#'            dnorm(x = 1:1000, mean = 500, sd = 50) * 0.5)
#' 
#' ## calculate spatial distance maps and inter-station distances
#' D <- eseis::spatial_distance(stations = sta[,1:2],
#'                              dem = dem)
#' 
#' model_amplitude(data = s, 
#'                 source = c(500, 600), 
#'                 d_map = D$maps, 
#'                 v = 500, 
#'                 q = 50, 
#'                 f = 10)
#' 
#' model_amplitude(data = s, 
#'                 distance = c(254, 8254, 9280, 11667),
#'                 model = "SurfBodySpreadAtten", 
#'                 v = 500, 
#'                 q = 50, 
#'                 f = 10, 
#'                 k = 0.5)
#'
#' }
#'
#' @export model_amplitude

model_amplitude <- function (
  
  data,
  model = "SurfSpreadAtten",
  distance,
  source,
  d_map,
  coupling,
  v,
  q,
  f,
  k, 
  a_0
) {
  
  ## check/format input data
  if(class(data)[1] != "list") {
    
    if(class(data)[1] == "matrix") {
      
      data_mat <- data
      data <- vector(mode = "list")
      
      for(i in 1:nrow(data_mat)) {
        
        data[[i]] <- list(signal = data_mat[i,])
      }
    } else if(class(data)[1] == "vector") {
      
      data_mat <- data
      data <- vector(mode = "list")
      
      for(i in 1:length(data_mat)) {
        
        data[[i]] <- list(signal = data_mat[i,])
      }
    }
  }
  
  ## get maximum amplitude
  a_d <- do.call(c, lapply(X = data, FUN = function(data) {
    
    max(data$signal)
  }))
  
  ## check/prepare distance data
  if(missing(distance) == TRUE) {
    
    ## check presence of alternative data
    if(missing(source) | missing (d_map)) {
      
      stop("Either coordinates of distance map is missing!")
    }
    
    ## convert source locations to coordinate object
    xy <- terra::vect(x = t(matrix(source)), type = "points")
    
    ## extract distances from distance maps
    distance <- lapply(X = d_map, FUN = function(d_map, xy) {
      
      terra::extract(x = d_map, y = xy)$lyr.1
    }, xy)
    distance <- do.call(c, distance)
  }
  
  ## check/set coupling factors
  if(missing(coupling) == TRUE) {
    
    coupling <- rep(1, length(data))
  }

  ## normalise by coupling efficiency
  a_d <- a_d * 1 / coupling

  ## define amplitude function and model parameters
  if(model == "SurfSpreadAtten") {
    
    model_fun <- a_d ~ a_0 / sqrt(d) * exp(-((pi * f * d) / (q * v)))
    model_par <- list(a_d = a_d,
                      d = distance,
                      f = f,
                      q = q,
                      v = v)
    model_start <- list(a_0 = 100 * max(a_d)) 
    
  } else if(model == "BodySpreadAtten") {
    
    model_fun <- a_d ~ a_0 / d * exp(-((pi * f * d) / (q * v)))
    model_par <- list(a_d = a_d,
                      d = distance,
                      f = f,
                      q = q,
                      v = v)
    model_start <- list(a_0 = 100 * max(a_d))
    
  } else if(model == "SurfBodySpreadAtten") {
    
    model_fun <- a_d ~ (k * a_0 / sqrt(d) * exp(-((pi * f * d) / (q * v)))) + 
      ((1 - k) * a_0 / d * exp(-((pi * f * d) / (q * v))))
    model_par <- list(a_d = a_d,
                      d = distance,
                      f = f,
                      q = q,
                      v = v,
                      k = k)
    model_start <- list(a_0 = 100 * max(a_d))
    
  } else if(model == "BodySpread") {
    
    model_fun <- a_d ~ a_0 / sqrt(d)
    model_par <- list(a_d = a_d,
                      d = distance)
    model_start <- list(a_0 = 100 * max(a_d))
    
  } else if(model == "BodySpread") {
    
    model_fun <- a_d ~ a_0 / d
    model_par <- list(a_d = a_d,
                      d = distance)
    model_start <- list(a_0 = 100 * max(a_d))

  } else if(model == "SurfBodySpread") {
    
    model_fun <- a_d ~ k * a_0 / sqrt(d) + (1 - k) * a_0 / d
    model_par <- list(a_d = a_d,
                      d = distance,
                      k = k)
    model_start <- list(a_0 = 100 * max(a_d))
  } 
  
  ## model event amplitude as function of distance
  mod <- try(nls(formula = model_fun,
                 data = model_par,
                 start = model_start),
             silent = TRUE)
  
  ## check modelling success and assign output objects
  if(class(mod)[1] == "try-error") {
    
    warning("Model could not be evaluated!")
    
    mod_a_0 <- NA
    mod_res <- NA
    mod_var <- NA
  } else {
    
    mod_a_0 <- try(coefficients(mod)[1])
    mod_res <- try(sum(residuals(mod)^2),
                   silent = TRUE)
    mod_var <- try(1 - (sum(residuals(object = mod)^2,
                            na.rm = TRUE) /
                          sum(model_par$a_d^2,
                              na.rm = TRUE)),
                   silent = TRUE)
  }

  ## return output
  return(list(a_0 = mod_a_0,
              residuals = mod_res,
              var_reduction = mod_var))
}