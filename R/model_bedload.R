#' Model the seismic spectrum due to bedload transport in rivers
#' 
#' The function calculates a seismic spectrum as predicted by the model 
#' of Tsai et al. (2012) for river bedload transport. The code was written to 
#' R by Sophie Lagarde and integrated to the R package 'eseis' by Michael 
#' Dietze.
#' 
#' The model uses a set of predefined constants. These can also be changed
#' by the user, using the \code{...} argument:
#' \itemize{
#'   \item \code{g = 9.81}, gravitational acceleration (m/s^2)
#'   \item \code{r_w = 1000}, fluid specific density (kg/m^3)
#'   \item \code{k_s = 3 * d_50}, roughness length (m)
#'   \item \code{log_lim = c(0.0001, 100), limits of grain-size distribution 
#'   function template (m)}
#'   \item \code{log_length = 10000, length of grain-size distribution 
#'   function template}
#'   \item \code{nu = 10^(-6)}, specific density of the fluid (kg/m^3)
#'   \item \code{power_d = 3}, grain-size power exponent
#'   \item \code{gamma = 0.9}, gamma parameter, after Parker (1990)
#'   \item \code{s_c = 0.8}, drag coefficient parameter
#'   \item \code{s_p = 3.5}, drag coefficient parameter
#'   \item \code{c_1 = 2 / 3}, inter-impact time scaling, after 
#'   Sklar & Dietrich (2004)
#' }
#' 
#' When no user defined grain-size distribution function is provided,the 
#' function calculates the raised cosine distribution function as defined 
#' in Tsai et al. (2012) using the default range and resolution as specified 
#' by \code{log_lim} and \code{log_length} (see additional arguments list 
#' above). These default values are appropriate for mean sediment sizes 
#' between 0.001 and 10 m and log standard deivations between 0.05 and 1. 
#' When more extreme distributions are to be used, it is necessary to either 
#' adjust the arguments \code{log_lim} and \code{log_length} or use a 
#' user defined distribution function.
#' 
#' @param gsd \code{data frame} grain-size distribution function. Must be 
#' provided as data frame with two variables: grain-size class (first column)
#' and vol- or wgt.-percentage per class (second column). See examples for 
#' details.
#'
#' @param d_s \code{Numeric} value, mean sediment grain diameter (m). 
#' Alternative to \code{gsd}.
#' 
#' @param s_s \code{Numeric} value, standard deviation of sediment grain 
#' diameter (m). Alternative to \code{gsd}.
#' 
#' @param r_s \code{Numeric} value, specific sediment density (kg / m^3)
#' 
#' @param q_s \code{Numeric} value, unit sediment flux (m^2 / s)
#' 
#' @param h_w \code{Numeric} value, fluid flow depth (m)
#' 
#' @param w_w \code{Numeric} value, fluid flow width (m)
#' 
#' @param a_w \code{Numeric} value, fluid flow inclination angle (radians)
#' 
#' @param f \code{Numeric} vector, frequency range to be modelled. 
#' If of length two the argument is interpreted as representing the lower and 
#' upper limit and the final length of the frequency vector is set by the 
#' argument \code{res}. If \code{f} contains more than two values it is 
#' interpreted as the actual frequency vector and the value of \code{res} is 
#' ignored.
#' 
#' @param res \code{Numeric} value, output resolution, i.e. length of the 
#' spectrum vector. Default is 1000.
#' 
#' @param r_0 \code{Numeric} value, distance of seismic station to source
#' 
#' @param f_0 \code{Numeric} value, reference frequency (Hz)
#' 
#' @param q_0 \code{Numeric} value, ground quality factor at \code{f_0}.
#' "Reasonable value may be \code{20}" (Tsai et al. 2012).
#' 
#' @param e_0 \code{Numeric} value, exponent characterizing quality factor 
#' increase with frequency (dimensionless). "Reasonable value may be 
#' \code{0}" (Tsai et al. 2012).
#' 
#' @param v_0 \code{Numeric} value, phase speed of the Rayleigh wave at 
#' \code{f_0} (m/s). Assuming a shear wave velocity of about 2200 m/s, 
#' Tsai et al. (2012) yield a value of 1295 m/s for this parameter.
#' 
#' @param x_0 \code{Numeric} value, exponent of the power law variation of 
#' Rayleigh wave velocities with frequency
#' 
#' @param n_0 \code{Numeric} vector of length two, Greens function 
#' displacement amplitude coefficients. Cf. N_ij in eq. 36 in Gimbert et 
#' al. (2014) 
#' 
#' @param n_c \code{Numeric} value, option to include single particle hops 
#' coherent in time, causing spectrum modulation due to secondary effects. 
#' Omitted is no value is specified, here. Usual values may be between 2 and 
#' 4.  
#' 
#' @param eseis \code{Character} value, option to return an eseis object 
#' instead of a data frame. Default is \code{FALSE}.
#' 
#' @param \dots Further arguments passed to the function.
#' 
#' @return \code{eseis} object containing the modelled spectrum.
#' 
#' @author Sophie Lagarde, Michael Dietze
#' 
#' @keywords eseis
#' 
#' @references
#' Tsai, V. C., B. Minchew, M. P. Lamb, and J.-P. Ampuero (2012), A 
#' physical model for seismic noise generation from sediment transport in 
#' rivers, Geophys. Res. Lett., 39, L02404, doi:10.1029/2011GL050255.
#' 
#' @examples
#' 
#' ## calculate spectrum (i.e., fig. 1b in Tsai et al., 2012)
#' p_bedload <- model_bedload(d_s = 0.7,
#'                            s_s = 0.1,
#'                            r_s = 2650,
#'                            q_s = 0.001,
#'                            h_w = 4,
#'                            w_w = 50,
#'                            a_w = 0.005,
#'                            f = c(0.1, 20),
#'                            r_0 = 600,
#'                            f_0 = 1,
#'                            q_0 = 20,
#'                            e_0 = 0,
#'                            v_0 = 1295,
#'                            x_0 = 0.374,
#'                            n_0 = 1,
#'                            res = 100,
#'                            eseis = TRUE)
#' 
#' ## plot spectrum
#' plot_spectrum(data = p_bedload, 
#'               ylim = c(-170, -110))
#'               
#' ## define empiric grain-size distribution
#' gsd_empiric <- data.frame(d = c(0.70, 0.82, 0.94, 1.06, 1.18, 1.30),
#'                           p = c(0.02, 0.25, 0.45, 0.23, 0.04, 0.00))
#'                   
#' ## calculate spectrum
#' p_bedload <- model_bedload(gsd = gsd_empiric,
#'                            r_s = 2650,
#'                            q_s = 0.001,
#'                            h_w = 4,
#'                            w_w = 50,
#'                            a_w = 0.005,
#'                            f = c(0.1, 20),
#'                            r_0 = 600,
#'                            f_0 = 1,
#'                            q_0 = 20,
#'                            e_0 = 0,
#'                            v_0 = 1295,
#'                            x_0 = 0.374,
#'                            n_0 = 1,
#'                            res = 100,
#'                            eseis = TRUE)
#'                   
#' ## plot spectrum
#' plot_spectrum(data = p_bedload, 
#'               ylim = c(-170, -110))
#'               
#' ## define mean and sigma for parametric distribution function
#' d_50 <- 1
#' sigma <- 0.1
#' 
#' ## define raised cosine distribution function following Tsai et al. (2012)
#' d_1 <- 10^seq(log10(d_50 - 5 * sigma), 
#'               log10(d_50 + 5 * sigma), 
#'               length.out = 20)
#' 
#' sigma_star <- sigma / sqrt(1 / 3 - 2 / pi^2)
#' 
#' p_1 <- (1 / (2 * sigma_star) * 
#'           (1 + cos(pi * (log(d_1) - log(d_50)) / sigma_star))) / d_1
#' p_1[log(d_1) - log(d_50) > sigma_star] <- 0
#' p_1[log(d_1) - log(d_50) < -sigma_star] <- 0
#' p_1 <- p_1 / sum(p_1)
#' 
#' gsd_raised_cos <- data.frame(d = d_1,
#'                              p = p_1)
#'              
#' @export model_bedload

model_bedload <- function(
  
  gsd,
  d_s,
  s_s,
  r_s,
  q_s,
  h_w,
  w_w,
  a_w,
  f = c(1, 100),
  r_0,
  f_0,
  q_0,
  e_0,
  v_0,
  x_0,
  n_0,
  n_c,
  res = 100,
  eseis = FALSE,
  ...
) {
  
  ## CHECK AND SET DEFAULT ARGUMENTS ------------------------------------------
  
  ## use user-defined gsd
  if(missing(gsd) == TRUE) {
    
    ## define log10 sequence of possible grain-size classes
    x_log <- 10^seq(log10(0.0001), 
                    log10(100), 
                    length.out = 10^(4))
    
    ## calculate grain-size scatter in modified units
    s <- s_s / sqrt(1 / 3 - 2 / pi^2)
    
    ## calculate grain-size distribution function
    p_s <- (1 / (2 * s) * (1 + cos(pi * (log(x_log) - log(d_s)) / s))) / x_log
    
    ## set values out of range to zero
    p_s[log(x_log) - log(d_s) > s] <- 0
    p_s[log(x_log) - log(d_s) < -s] <- 0
    
    ## remove grain-size classes with zero contribution
    x_log <- x_log[p_s > 0]
    p_s <- p_s[p_s > 0]
    
    ## rescale grain-size distribution to one
    p_s = p_s / sum(p_s)
    
  } else {
    
    x_log <- gsd[,1]
    
    p_s <- gsd[,2]
    
    d_s <- x_log[abs(cumsum(p_s) - 0.5) == 
                   min(abs(cumsum(p_s) - 0.5))][1]
  }
  
  ## extract additional arguments
  extraArgs <- list(...)
  
  ## assign gravitational acceleration (m/s^2)
  g <- ifelse(test = "g" %in% names(extraArgs),
              yes = extraArgs$g,
              no = 9.81)
  
  ## assign roughness length of the river bed (m)
  k_s <- ifelse(test = "k_s" %in% names(extraArgs),
                yes = extraArgs$k_s,
                no = 3 * d_s)
  
  ## fluid specific density (kg/m^3)
  r_w <- ifelse(test = "r_w" %in% names(extraArgs),
                yes = extraArgs$r_w,
                no = 1000)
  
  ## assign limits of grain-size distribution function template
  if("log_lim" %in% names(extraArgs)) {
    
    log_lim <- extraArgs$log_lim
  } else {
    
    log_lim <- c(0.0001, 100)
  }
  
  ## assign length of grain-size distribution function template
  log_length <- ifelse(test = "log_length" %in% names(extraArgs),
                       yes = extraArgs$log_length,
                       no = 10000)
  
  ## assign dynamic fluid viscosity
  nu <- ifelse(test = "nu" %in% names(extraArgs),
               yes = extraArgs$nu,
               no = 10^(-6))
  
  ## assign grain-size power exponent for eq. (7)
  power_d <- ifelse(test = "power_d" %in% names(extraArgs),
                    yes = extraArgs$power_d,
                    no = 3)
  
  ## assign gamma parameter, after Parker (1990)
  gamma <- ifelse(test = "gamma" %in% names(extraArgs),
                  yes = extraArgs$gamma,
                  no = 0.9)
  
  ## assign drag coefficient parameter
  s_c <- ifelse(test = "s_c" %in% names(extraArgs),
                yes = extraArgs$s_c,
                no = 0.8)
  
  ## assign drag coefficient parameter
  s_p <- ifelse(test = "s_p" %in% names(extraArgs),
                yes = extraArgs$s_p,
                no = 3.5)
  
  ## assign inter-impact time scaling, after Sklar & Dietrich (2004)
  c_1 <- ifelse(test = "c_1" %in% names(extraArgs),
                yes = extraArgs$c_1,
                no = 2 / 3)
  
  ## check/set d_s argument
  if(missing(d_s) == TRUE) {
    
    d_s <- NA
  }
  
  ## check/set s_s argument
  if(missing(s_s) == TRUE) {
    
    s_s <- NA
  }
  
  ## check/set n_c argument
  if(missing(n_c) == TRUE) {
    
    n_c <- NA
  }
  
  ## ORGANISE ESEIS DATA ------------------------------------------------------
  
  ## get start time
  eseis_t_0 <- Sys.time()
  
  
  ## collect function arguments
  eseis_arguments <- list(d_s,
                          s_s,
                          r_s,
                          q_s,
                          h_w,
                          w_w,
                          a_w,
                          f = c(1, 100),
                          r_0,
                          f_0,
                          q_0,
                          e_0,
                          v_0,
                          x_0,
                          n_0,
                          n_c,
                          g = g,
                          r_w = r_w,
                          k_s = k_s,
                          log_lim = log_lim,
                          log_length = log_length,
                          nu = nu,
                          power_d = power_d,
                          gamma = gamma,
                          s_c = s_c,
                          s_p = s_p,
                          c_1 = c_1,
                          n_c = n_c,
                          res = 100,
                          eseis = FALSE)
  
  ## calculate submersion-corrected specific sediment density ratio
  r_b <- (r_s - r_w) / r_w
  
  ## calculate bed shear velocity
  u_s <- sqrt(g * h_w * sin(a_w))
  
  ## calculate depth averaged flow velocity after Parker (1991)
  u_m <- 8.1 * u_s * (h_w / k_s)^(1 / 6)
  
  ## calculate chi as function of bed slope angle
  chi <- 0.407 * log(142 * tan(a_w))
  
  ## define tau star polynomial model
  t_s_c50 <- exp(2.59 * (10^(-2)) * (chi^4) + 
                   8.94 * 10^(-2) * (chi^3) + 
                   0.142 * (chi^2) + 0.41 * 
                   chi - 3.14)
  
  ## define output frequency vector
  f_i <- seq(from = f[1], 
             to = f[2], 
             length.out = res)
  
  ## calculate frequency specific quality factor
  q <- q_0 * (f_i / f_0)^e_0
  
  ## calculate frequency specific Rayleigh wave velocity
  v_c <- v_0 * (f_i / f_0)^(-x_0)
  
  ## calculate frequency specific group velovity
  v_u <- v_c / (1 + x_0)
  
  ## calculate beta term
  b <- (2 * pi * r_0 * (1 + x_0) * f_i^(1 + x_0 - e_0)) / 
    (v_0 * q_0 * f_0^(x_0 - e_0))
  
  ## calculate chi_beta term
  x_b <- 2 * log(1 + (1 / b)) * exp(-2 * b) + 
    (1 - exp(-b)) * exp(-b) * sqrt(2 * pi / b)
  
  ## 
  s_x <- log10((r_b * g * x_log^power_d) / nu^2)
  
  r_1 <- -3.76715 + 1.92944 * s_x - 
    0.09815 * (s_x^2) - 
    0.00575 * (s_x^3) + 
    0.00056 * (s_x^4)
  
  r_2 <- log10(1 - ((1 - s_c) / 0.85)) - 
    (1 - s_c)^2.3 * tanh(s_x - 4.6) +
    0.3 * (0.5 - s_c) * 
    (1 - s_c)^2 * 
    (s_x - 4.6)
  
  r_3 <- (0.65 - ((s_c / 2.83) * tanh(s_x - 4.6)))^(1 + ((3.5 - s_p) / 2.5))
  
  w_1 <- r_3 * 10^(r_2 + r_1)
  
  w_2 <- (r_b * g * nu * w_1)^(1/3)
  
  c_d <- (4 / 3) * (r_b * g * x_log) / (w_2^2)
  
  t_s <- (u_s^2) / (r_b * g * x_log)
  
  t_s_c <- t_s_c50 * ((x_log / d_s)^(-gamma))
  
  ## calculate depth-averaged mobile sediment layer 
  ## height, Sklar and Dietrich (2004)
  h_b <- 1.44 * x_log * (t_s / t_s_c)^0.5
  
  ## account for layer heights greater than fluid flow heights
  h_b[h_b > h_w] <- h_w
  
  ## calculate depth-averaged bed load velocity, Sklar and Dietrich (2004)
  u_b <- 1.56 * sqrt(r_b * g * x_log) * (t_s / t_s_c)^0.56
  
  ## account for bed load velocities higher than fluid velcoties
  u_b[u_b > u_m] <- u_m
  
  ## calculate sperical particle volume
  v_p <- (4 / 3) * pi * (x_log/2)^3
  
  ## calculate particle mass
  m <- r_s * v_p
  
  ## calculate terminal settling velocity
  w_st <- sqrt(4 * r_b * g * x_log / (3 * c_d))
  
  ## calculate bed load layer height
  h_b_2 <- 3 * c_d * r_w * h_b / (2 * r_s * x_log * cos(a_w))
  
  ## calculate particle impact velocity normal to the bed
  w_i <-  w_st * cos(a_w) * sqrt(1 - exp(-h_b_2))
  
  ## calculate depth averaged settling velocity through the bed load layer
  w_s <- (h_b_2 * w_st * cos(a_w)) / 
    (2 * log(exp(h_b_2 / 2) + sqrt(exp(h_b_2) - 1)))
  
  ## create variable parameter list
  p1 <- as.list(as.data.frame(rbind(f_i, x_b, v_c, v_u)))
  
  ## create constant parameter list
  p2 <- list(h_b = h_b,
             c_1 = c_1,
             w_s = w_s, 
             w_w = w_w, 
             q_s = q_s, 
             m = m, 
             v_p = v_p, 
             u_b = u_b, 
             r_s = r_s, 
             n_0 = n_0, 
             w_i = w_i,
             p_s = p_s, 
             n_c = n_c)
  
  ## calculate power spectra for each frequency interval
  z <- lapply(X = p1, FUN = function(p1, p2) {
    
    if(is.na(p2$n_c) == FALSE) {
      
      z <- complex(real = cos(-p2$n_c * pi * p1[1] * 
                                p2$h_b / (p2$c_1 * p2$w_s)), 
                   imaginary = sin(-p2$n_c * pi * p1[1] * h_b 
                                   / (p2$c_1 * p2$w_s)))
      
      f_t <- (Mod(1 + z))^2 / 2
      
      psd_raw <- (p2$c_1 * p2$w_w * p2$q_s * p2$w_s * pi^2 *
                    p1[1]^3 * p2$m^2 * p2$w_i^2 * p1[2]) * f_t /
        (p2$v_p * p2$u_b * p2$h_b * p2$r_s^2 * p1[3]^3 * p1[4]^2)
    } else {
      
      z <- complex(real = cos(-2 * pi * p1[1] * 
                                p2$h_b / (p2$c_1 * p2$w_s)), 
                   imaginary = sin(-2 * pi * p1[1] * h_b 
                                   / (p2$c_1 * p2$w_s)))
      
      f_t <- (Mod(1 + z))^2 / 2
      
      psd_raw <- (p2$c_1 * p2$w_w * p2$q_s * p2$w_s * pi^2 *
                    p1[1]^3 * p2$m^2 *p2$ w_i^2 * p1[2]) /
        (p2$v_p * p2$u_b * p2$h_b * p2$r_s^2 * p1[3]^3 * p1[4]^2)
    }
    
    psd_f <- sum(p_s * psd_raw * n_0^2)
    
    return(psd_f)
    
  }, p2)
  
  ## convert list to vector
  z <- do.call(base::c, z)
  
  ## assign data to output object
  P <- data.frame(frequency = f_i,
                  spectrum = z)
  
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
           call = "model_turbulence()",
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