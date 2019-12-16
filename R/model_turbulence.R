#' Model the seismic spectrum due to hydraulic turbulence
#' 
#' The function calculates the seismic spectrum as predicted by the model 
#' of Gimbert et al. (2014) for hydraulic turbulence. The code was written to 
#' R by Sophie Lagarde and integrated to the R package 'eseis' by Michael 
#' Dietze.
#' 
#' The model uses a set of predefined constants. These can also be changed
#' by the user, using the \code{...} argument:
#' \itemize{
#'   \item \code{c = 0.5}, instantaneous fluid-grain friction coefficient 
#'   (dimensionless)
#'   \item \code{g = 9.81}, gravitational acceleration (m/s^2)
#'   \item \code{k = 0.5}, Kolmogrov constant (dimensionless)
#'   \item \code{k_s = 3 * d_s}, roughness length (m)
#'   \item \code{h = k_s / 2}, reference height of the measurement (m)
#'   \item \code{e_0 = 0}, exponent of Q increase with frequency 
#'   (dimensionless)
#'   \item \code{r_w = 1000}, specific density of the fluid (kg/m^3)
#'   \item \code{c_w = 0.5}, instantaneous fluid-grain friction coefficient
#'   (dimensionless)
#' }
#' 
#' @param d_s \code{Numeric} value, mean sediment grain diameter (m)
#' 
#' @param s_s \code{Numeric} value, standard deviation of sediment grain 
#' diameter (m)
#' 
#' @param r_s \code{Numeric} value, specific sediment density (kg / m^3)
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
#' @param q_0 \code{Numeric} value, ground quality factor at \code{f_0}
#' 
#' @param v_0 \code{Numeric} value, phase speed of the Rayleigh wave at 
#' \code{f_0} (m/s)
#' 
#' @param p_0 \code{Numeric} value, variation exponent of Rayleigh wave 
#' velocities with frequency (dimensionless)
#' 
#' @param n_0 \code{Numeric} vector of length two, Greens function 
#' displacement amplitude coefficients. Cf. N_ij in eq. 36 in Gimbert et 
#' al. (2014) 
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
#' @examples
#' 
#' ## model the turbulence-related power spectrum
#' P <- model_turbulence(d_s = 0.03, # 3 cm mean grain-size
#'                       s_s = 1.35, # 1.35 log standard deviation
#'                       r_s = 2650, # 2.65 g/cm^3 sediment density
#'                       h_w = 0.8, # 80 cm water level
#'                       w_w = 40, # 40 m river width
#'                       a_w = 0.0075, # 0.0075 rad river inclination
#'                       f = c(1, 200), # 1-200 Hz frequency range
#'                       r_0 = 10, # 10 m distance to the river
#'                       f_0 = 1, # 1 Hz Null frequency 
#'                       q_0 = 10, # 10 quality factor at f = 1 Hz
#'                       v_0 = 2175, # 2175 m/s phase velocity
#'                       p_0 = 0.48, # 0.48 power law variation coefficient
#'                       n_0 = c(0.6, 0.8), # Greens function estimates
#'                       res = 1000) # 1000 values build the output resolution
#' 
#' ## plot the power spectrum
#' plot_spectrum(data = P)
#'               
#' @export model_turbulence
#' 
model_turbulence <- function(
  
  d_s,
  s_s,
  r_s = 2650,
  h_w,
  w_w,
  a_w,
  f = c(1, 100),
  r_0,
  f_0,
  q_0,
  v_0,
  p_0,
  n_0,
  res = 1000,
  eseis = FALSE,
  ...
) {
  
  ## CHECK AND SET DEFAULT ARGUMENTS ------------------------------------------
  
  ## extract additional arguments
  extraArgs <- list(...)
  
  ## assign gravitational acceleration
  g <- ifelse(test = "g" %in% names(extraArgs),
              yes = extraArgs$g,
              no = 9.81)

  ## assign Kolmogorov constant
  k <- ifelse(test = "k" %in% names(extraArgs),
              yes = extraArgs$k,
              no = 0.5)

  ## assign roughness length
  k_s <- ifelse(test = "k_s" %in% names(extraArgs),
              yes = extraArgs$k_s,
              no = 3 * d_s)

  ## assign measurement height above bed
  h <- ifelse(test = "h" %in% names(extraArgs),
              yes = extraArgs$h,
              no = k_s / 2)

  ## assign exponent of Q increase
  e_0 <- ifelse(test = "e_0" %in% names(extraArgs),
              yes = extraArgs$e_0,
              no = 0)

  ## assign flud density
  r_w <- ifelse(test = "r_w" %in% names(extraArgs),
              yes = extraArgs$r_w,
              no = 1000)
  
  ## assign friction coefficiant
  c_w <- ifelse(test = "c_w" %in% names(extraArgs),
              yes = extraArgs$c_w,
              no = 0.5)
  
  ## ORGANISE ESEIS DATA ------------------------------------------------------
  
  ## get start time
  eseis_t_0 <- Sys.time()
  
  ## collect function arguments
  eseis_arguments <- list(d_s = d_s,
                          s_s = s_s,
                          r_s = r_s,
                          h_w = h_w,
                          w_w = w_w,
                          a_w = a_w,
                          f = f,
                          r_0 = r_0,
                          f_0 = f_0,
                          q_0 = q_0,
                          v_0 = v_0,
                          p_0 = p_0,
                          n_0 = n_0,
                          res = res,
                          g = g,
                          k = k,
                          k_S = k_s,
                          h = h,
                          e_0 = e_0,
                          r_w = r_w,
                          c_w = c_w)
  
  ## CALCULATION PART ---------------------------------------------------------
  
  ## define frequency vector
  if(length(f) == 2) {
    
    f_seq <- seq(from = f[1], 
                 to = f[2], 
                 length.out = res)
  } else {
    
    f_seq <- f
  }

  ## calculate frequency dependent quality factor  
  q_seq <- q_0 * (f_seq / f_0)^e_0
  
  ## calculate frequency dependent wave phase velocity 
  v_seq <- v_0 * (f_seq / f_0)^-p_0
  
  ## calculate frequency dependent wave group velocity 
  v_u_seq <- v_seq / (1 + p_0)
  
  ## calculate beta
  beta <- (2 * pi * r_0 * (1 + p_0) * f_seq^(1 + p_0 - e_0)) / 
    (v_0 * q_0 * f_0^(p_0 - e_0))
  
  ## calculate psi
  psi <- 2 * log(1 + (1 / beta)) * exp(-2 * beta) + (1 - exp(-beta)) * 
    exp(-beta) * sqrt(2 * pi / beta)

  ## calcluate auxiliary variables
  c_p_0 <- 4 * (1 - (1 / 4 * k_s / h_w))
  
  c_k_s <- 8 * (1 - (k_s / (2 * h_w)))
  
  c_s <- 0.2 * (5.62 * log10(h_w / k_s) + 4)
  
  ## calculate zeta
  z <- c_k_s^(2/3) * c_p_0^(8/3) * c_s^(4/3)
  
  ## calculate auxiliary variables
  u_s <- sqrt(g * h_w * sin(a_w))
  
  u_p_0 <- c_p_0 * u_s
  
  s <- s_s / sqrt((1/3) - 2 / (pi^2))
  
  ## define integration limits
  l <- c(exp(-s) * d_s, exp(s) * d_s)

  ## integrate phi over grain-size distribition
  phi <- lapply(X = f_seq, FUN = function(f_seq) {
    
    f <- function(d) {
      a <- ((1 / (1 + (2 * f_seq * d / u_p_0)^(4/3)))^2)
      
      b <- ((1 / (2 * s * d) * (1 + cos(pi * (log(d) - log(d_s)) / s)))) * d^2
      
      return(a * b)
    }
    
    f_int <- stats::integrate(f = f, lower = l[1], upper = l[2])$value
    
    return(f_int)
  })
  
  ## convert list to vector
  phi <- do.call(c, phi)

  ## calculate spectral power
  p <- (n_0[1]^2 + n_0[2]^2) * 
    (k * w_w / (3 * (k_s^(2/3)))) * 
    ((r_w / r_s)^2) * 
    (((1 + p_0)^2) / ((f_0^(5 * p_0)) * v_0^5)) *
    z * psi * phi * 
    (f_seq^((4/3) + 5 * p_0)) * 
    (g^(7/3)) * 
    (sin(a_w)^(7/3)) * 
    (c_w^2)*(h_w^(7/3))
  
  ## create output data frame
  P <- data.frame(frequency = f_seq, 
                  spectrum = p)
  
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