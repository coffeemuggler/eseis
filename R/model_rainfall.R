#' Model the seismic spectrum due to rainfall
#' 
#' The function calculates the seismic spectrum as predicted by the model of 
#' Porter et al. (2024). The code is adopted from a Matlab script by R. Porter
#' and integrated to the R package 'eseis' by Michael Dietze. The function 
#' calculates a rain drop size distribution with the Gamma function approach 
#' by Uijlenhoet and Stricker (1999). Alternatively, the user can provide an 
#' empirical drop size distribution and corresponing fall velocities (see 
#' details). From the input data, the function derives an impulse flux. The 
#' integrated impulse flux is corrected by the Greens function and returned 
#' as the seismic power spectrum (\code{eseis} object by default).
#' 
#' The model uses by default the approach by Uijlenhoet and Stricker (1999) to 
#' convert the bulk rain intensity (\code{I}) to a rain drop size and velocity 
#' distribution. This requires a vector of drop size limits (\code{D_size}), 
#' which is by default calculated automatically as a linear spaced sequence of 
#' values between the limits (\code{D_lim = c(0.1, 10)} (mm)) with a resolution
#' of 100 values (\code{D_res = 100}). The approach further uses the empirical 
#' values of Uijlenhoet and Stricker (1999), i.e. \code{a = 3.778} and
#' \code{B = 0.67}. See list below for details and note that those four  
#' parameters can also be changed by the user. Then, the  following steps are 
#' implemented specifically: (1) calculate a factor Gamma following the Gamma 
#' function with the empirical fit parameter B 
#' (\code{gamma <- pracma::gammaz(z = 1 + B)}), (2) calculate a factor Delta 
#' from the rain intensity (\code{delta <- 4.23*I^(-0.214)}), (3) derive a drop 
#' size distribution function 
#' (\code{D_prob <- (delta^(1 + B) / gamma) * (D_size^B * exp(-delta * D_size))}), 
#' (4) estimate the fall velocity for each drop size class 
#' (\code{D_velo <- a * D_size^B}) ans set velocities above a pre-defined 
#' maximum fall velocity (\code{v_max = 9.5} m/s) to that maximum value.
#' 
#' Alternatively, a user can also provide empirical information on these rain
#' parameters. If the following vectors are provided, the 
#' function will operate with these values instead of estimating them following
#' the above approach. Note that all vectors must have the same length and that 
#' no fall velocity correction according to \code{v_max} is performed in that 
#' case.
#' 
#' (1) \code{D_size}, upper drop size limits (mm)
#' 
#' (2) \code{D_prob}, relative probability of that size class
#' 
#' (3) \code{D_velo}, fall velocity of the corresponding size class (m/s)
#' 
#' The model uses a set of further, predefined constants. These can be changed  
#' by the user, using the \code{...}-argument:
#' \itemize{
#'   \item \code{D_lim = c(0.01, 10)}, range of drop diameters (mm),
#'   defined as min and max The drop sizes will be linearly interpolated 
#'   in that range with equal intervals using by default a resolution of 100 
#'   values (see \code{D_res})
#'   \item \code{D_res = 100}, resolution of the drop diameter vector defined 
#'   by the range parameter \code{D}
#'   \item \code{a = 3.778}, fit parameter for drop velocity from diameters, 
#'   see Uijlenhoet and Stricker (1999)
#'   \item \code{B = 0.67}, fit parameter for drop velocity from diameters, 
#'   see Uijlenhoet and Stricker (1999)
#'   \item \code{epsilon = 1}, impact elasticity, \code{1} for liquid water
#'   \item \code{v_max = 9.5}, maximum fall velocity for rain drops (m/s), 
#'   all higher velocities will be set to this value
#'   \item \code{r_w = 1000}, specific density of the rain drops (kg/m^3)
#'   \item \code{f_0 = 1}, reference frequency (Hz), used to adjust frequency
#'   dependent quality factor and wave velocity values
#'   \item \code{alpha = -0.85}, constant for Rayleigh wave particle motion 
#'   attenuation with depth from surface, see Stein and Wysession (2003) 
#'   (Eq. 2.7-9) for details.
#'   \item \code{beta = -0.39}, constant for Rayleigh wave particle motion 
#'   attenuation with depth from surface, see Stein and Wysession (2003) 
#'   (Eq. 2.7-9) for details.
#'   
#' }
#' 
#' @references
#' 
#' Stein S and Wysession M (2003). An Introduction to Seismology, Earthquakes, 
#' and Earth Structure. Blackwell. ISBN-13: 978-0-86542-078-6 
#' 
#' Uijlenhoet R and Stricker JNM (1999). A consistent rainfall parameterization 
#' based on the exponential raindrop size distribution. Journal of Hydrology 
#' 218, 101-127. DOI: 10.1016/S0022-1694(99)00032-3.
#' 
#' Tsai VC and Atiganyanun S (2014). Greens functions for surface waves in a 
#' generic velocity StructureShort note. Bulletin of the Seismological 
#' Society of America, 104(5), 2573-2578. DOI: 10.1785/0120140121.
#' 
#' Tsai VC, Minchew B, Lamb MP, and Ampuero J-P (2012). A physical model for
#' seismic noise generation from sediment transport in rivers, Geophysical
#' Research Letters 39, L02404. DOI: 10.1029/2011GL050255.
#' 
#' @param I \code{Numeric} value, rain intensity (mm/h)
#' 
#' @param depth \code{Numeric} value, sensor depth below surface (m),
#' if omitted, default value is \code{0}.
#' 
#' @param q_0 \code{Numeric} value, ground quality factor at \code{f_0}
#' 
#' @param v_0 \code{Numeric} value, phase velocity at \code{f_0} (m/s)
#' 
#' @param e_0 \code{Numeric} value, variation exponent of quality factor change
#' with frequency.
#' 
#' @param p_0 \code{Numeric} value, variation exponent of Rayleigh wave 
#' velocity with frequency
#' 
#' @param n_0 \code{Numeric} value, Greens function amplitude scaling 
#' coefficient, assumed to be near \code{1} at high frequencies (Tsai and 
#' Atiganyanun, 2014). Default is \code{1}. 
#' 
#' @param r_s \code{Numeric} value, specific density of the ground, default 
#' is \code{2650} kg/m^3 
#' 
#' @param f \code{Numeric} vector, frequency range to be modelled. If of 
#' length two the argument is interpreted as representing the lower and 
#' upper limit and the final length of the frequency vector is set by the 
#' argument \code{res}. If f contains more than two values it is interpreted 
#' as the actual frequency vector and the value of \code{res} is ignored. 
#' Default is \code{c(1, 100)}.
#' 
#' @param res \code{Numeric} value, output resolution, i.e. length of the 
#' spectrum vector. Default is \code{1000}.
#' 
#' @param eseis \code{Logical} value, option to return an eseis object 
#' instead of a data frame. Default is \code{FALSE}.
#' 
#' @param \dots Further arguments passed to the function.
#' 
#' @return \code{eseis} object containing the modelled spectrum.
#' 
#' @author Ryan Porter, Michael Dietze
#' 
#' @keywords eseis
#' 
#' @examples
#' 
#' ## model rainfall spectrum for 40 mm/h intensity, sensor at 0.5 m depth
#' p <- model_rainfall(I = 40, 
#'                     q_0 = 20, 
#'                     v_0 = 595, 
#'                     e_0 = 0.2,
#'                     p_0 = 1, 
#'                     depth = 0.5)
#' 
#' ## plot spectrum
#' plot(p)
#' 
#' ## model rainfall spectrum with manually defined rain properties
#' p <- model_rainfall(I = 40, 
#'                     D_size = c(0.1, 0.2, 0.5, 1, 2, 5, 10),
#'                     D_prob = c(1e-1, 1e-2, 1e-3, 1e-5, 1e-7, 1e-9, 5e-11),
#'                     D_velo = c(0.8, 5.5, 8.5, 9.5, 9.5, 9.5, 9.5),
#'                     q_0 = 20, 
#'                     v_0 = 595, 
#'                     e_0 = 0.2,
#'                     p_0 = 1, 
#'                     depth = 0.5)
#'                     
#' ## plot spectrum
#' plot(p)
#' 
#' @export model_rainfall

model_rainfall <- function(
    
  I,
  depth = 0,
  q_0,
  v_0,
  e_0,
  p_0,
  n_0 = 1,
  r_s = 2650,
  f = c(1, 100),
  res = 1000,
  eseis = TRUE,
  ...
  
) {
  
  ## extract extra arguments
  args <- list(...)
  
  ## assign impact elasticity
  epsilon <- ifelse(test = "epsilon" %in% names(args),
                  yes = args$epsilon,
                  no = 1)
  
  ## D_lim-parameter of D-v-fit
  if("D_lim" %in% names(args) == TRUE) {
    D_lim <- args$D_lim
  } else {
    D_lim <- c(0.1, 10)
  }
  
  ## D_res-parameter of D-v-fit
  D_res <- ifelse(test = "D_res" %in% names(args),
                  yes = args$D_res,
                  no = 100)
  
  ## a-parameter of D-v-fit
  a <- ifelse(test = "a" %in% names(args),
              yes = args$a,
              no = 3.778)
  
  ## B-parameter of D-v-fit
  B <- ifelse(test = "B" %in% names(args),
              yes = args$B,
              no = 0.67)
  
  ## maximum drop fall velocity
  v_max <- ifelse(test = "v_max" %in% names(args),
                  yes = args$v_max,
                  no = 9.5)
  
  ## water density
  r_w <- ifelse(test = "r_w" %in% names(args),
                yes = args$r_w,
                no = 1000)
  
  ## reference frequency
  f_0 <- ifelse(test = "f_0" %in% names(args),
                yes = args$f_0,
                no = 1)
  
  ## empirical rain information - drop size limits
  if("D_size" %in% names(args) == TRUE) {
    D_size <- args$D_size
  } else {
    D_size <- NULL
  }
  
  ## empirical rain information - drop size probability
  if("D_prob" %in% names(args) == TRUE) {
    D_prob <- args$D_prob
  } else {
    D_prob <- NULL
  }
  
  ## empirical rain information - drop velocities
  if("D_velo" %in% names(args) == TRUE) {
    D_velo <- args$D_velo
  } else {
    D_velo <- NULL
  }
  
  ## alpha value used for depth effect estimation
  alpha_val <- ifelse(test = "alpha" %in% names(args),
                yes = args$alpha,
                no = -0.85)
  
  ## beta value used for depth effect estimation
  beta_val <- ifelse(test = "beta" %in% names(args),
                yes = args$beta,
                no = -0.39)
  
  ## ORGANISE ESEIS DATA ------------------------------------------------------
  
  ## get start time
  eseis_t_0 <- Sys.time()
  
  ## collect function arguments
  eseis_arguments <- list(I = I,
                          q_0 = q_0,
                          v_0 = v_0,
                          p_0 = p_0,
                          e_0 = e_0,
                          n_0 = n_0,
                          r_s = r_s,
                          f = f,
                          res = res,
                          epsilon = epsilon,
                          D_lim = D_lim,
                          D_res = D_res,
                          a = a,
                          B = B,
                          D_size = D_size,
                          D_prob = D_prob,
                          D_velo = D_velo,
                          v_max = v_max,
                          r_w = r_w,
                          f_0 = f_0)
  
  ## CALCULATION PART ---------------------------------------------------------
  
  ## define frequency vector
  if(length(f) == 2) {
    
    f_seq <- seq(from = f[1], to = f[2], length.out = res)
  } else {
    
    f_seq <- f
  } 
  
  ## check if D_size, D_prob and D_velo are present or calculate them
  if(any(c(is.null(D_size), is.null(D_prob), is.null(D_velo))) == TRUE) {
    
    ## define diameter vector
    D_size <- seq(from = D_lim[1], to = D_lim[2], length.out = D_res)
    
    ## get diameter class sizes
    D_diff <- c(diff(D_size), 0)
    
    ## calculate gamma value (using simplified version of pracma::gammaz)
    x = 1 + B
    cc <- c(0.999999999999997, 57.1562356658629, -59.5979603554755, 
            14.1360979747417, -0.49191381609762, 3.39946499848119e-05, 
            4.65236289270486e-05, -9.83744753048796e-05, 0.000158088703224912, 
            -0.000210264441724105, 0.000217439618115213, -0.000164318106536764, 
            8.44182239838527e-05, -2.61908384015814e-05, 3.68991826595316e-06)
    xp <- (x - 0.5 + 607/128)^((x - 0.5) * 0.5)
    ss <- 0
    for(k in (length(cc) - 1):1) {ss <- ss + cc[k + 1] / (x -1 + k)}
    gamma <- (2.506628274631 * (cc[1] + ss)) * 
      ((xp * exp(-(x - 0.5 + 607/128))) * xp)
    gamma[x == 0 | gamma == 1] <- 1

    ## calculate delta parameter
    delta <- 4.23*I^(-0.214)
    
    ## calculate rain drop size distribution
    D_prob <- (delta^(1 + B) / gamma) * (D_size^B * exp(-delta * D_size))
    D_prob <- D_prob / sum(D_prob * D_diff) * D_diff

    ## calculate rain drop volume distribution
    p_v = D_prob * (4/3 * pi * (D_size/(2 * 1000))^3) * D_diff
    
    ## calculate number of unit drops per unit time (SI units)
    D_n = I / (sum(p_v) * 3600 * 1000)
    
    ## calculate drop fall velocity
    D_velo <- a * D_size^B
    
    ## reduce drop fall velocity by maximum fall velocity
    D_velo[D_velo > v_max] <- v_max
    
  } else {
    
    ## get diameter class sizes
    D_diff <- c(diff(D_size), 0)
    
    ## normalise size distribution function
    D_prob <- D_prob / sum(D_prob * D_diff) * D_diff
    
    ## calculate rain drop volume distribution
    p_v = D_prob * (4/3 * pi * (D_size/(2 * 1000))^3) * D_diff
    
    ## calculate number of unit drops per unit time (SI units)
    D_n = I / (sum(p_v) * 3600 * 1000)
  }

  ## calculate impulse flux
  J_p <- (4/3 * pi * r_w)^2 * epsilon * 
    sum(D_n * D_prob * D_diff * (D_size/(2 * 1000))^6 * D_velo^2)
  
  ## collect all parameters integration
  pars <- list(q_0 = q_0,
               f_0 = f_0, 
               p_0 = p_0,
               e_0 = e_0,
               n_0 = n_0,
               v_0 = v_0,
               r_s = r_s,
               depth = depth,
               alpha_val = alpha_val,
               beta_val = beta_val)
  
  ## calculate frequency-wise seismic power values
  P <- sapply(X = f_seq, FUN = function(f, pars) {
    
    ## calculate quality factor for each frequency value
    q_f <- pars$q_0 * (f / pars$f_0)^pars$e_0
    
    ## calculate phase and group velocities
    v_phase <- pars$v_0 * (f / pars$f_0)^(-pars$p_0)
    v_group <- v_phase / (1 + pars$p_0)
    
    G <- function(r) {
      
      ## correct distance to sensor by deployment depth
      r_corr <- sqrt(r^2 + depth^2)
      
      term_a <- n_0 * f / (8 * pars$r_s * v_phase^2 * v_group)
      term_b <- (1 + ((pi^2 * f * r_corr) / (v_phase))^3)^(-1/6)
      term_c <- exp((-pi * f * r_corr) / (v_group * q_f))
      
      J_p * (term_a * term_b * term_c)^2 * r_corr
    }
    
    ## integrate Greens function over distance to sensor
    G_f <- stats::integrate(f = G, lower = 0, upper = Inf)$value
    
    ### CORRECTED FOR RAYLEIGH WAVE PARTICLE MOTION ATTENUATION WITH DEPTH
    k_val <- 2 * pi * f / v_phase
    
    d_val <- ((147 * exp(pars$beta_val * k_val * pars$depth)) / 100 +  
            pars$alpha_val * exp(pars$alpha_val * k_val * pars$depth)) /
      ((147 * exp(pars$beta_val * k_val * 0)) / 100 + 
         pars$alpha_val * exp(pars$alpha * k_val * 0))
    ### CORRECTED FOR RAYLEIGH WAVE PARTICLE MOTION ATTENUATION WITH DEPTH
    
    P_f <- 8 *pi^3 * f^2 * G_f * d_val^2
    
    ## return frequency-wise seismic power
    return(P_f)
    
  }, pars = pars)
  
  ## create output data frame
  P <- data.frame(frequency = f_seq, 
                  power = P)
  
  ## optionally create and fill eseis object
  if(eseis == TRUE) {
    
    ## create eseis object
    eseis_data <- aux_initiateeseis()
    
    ## assign aggregated signal vector
    eseis_data$signal <- P
    
    ## rename output list element
    names(eseis_data)[1] <- "spectrum"
    
    ## calculate function call duration
    eseis_duration <- as.numeric(difftime(time1 = Sys.time(), 
                                          time2 = eseis_t_0, 
                                          units = "secs"))
    
    ## update object history
    eseis_data$history[[length(eseis_data$history) + 1]] <- 
      list(time = Sys.time(),
           call = "model_rainfall()",
           arguments = eseis_arguments,
           duration = eseis_duration)
    names(eseis_data$history)[length(eseis_data$history)] <- 
      as.character(length(eseis_data$history))
    
    ## update data type
    eseis_data$meta$type = "spectrum"
    
    ## assign eseis object to output data set
    data_out <- eseis_data
    
  } else {
    
    data_out <- P
  }
  
  ## return output
  return(data_out)
  
}
