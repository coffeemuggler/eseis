#' Create reference model reference parameter catalogue
#' 
#' In order to run the fluvial model inversion (FMI) routine, a set of 
#' randomised target parameter combinations needs to be created. This 
#' function does this job.
#' 
#' All parameters must be provided as single values, except for those 
#' parameters that shall be randomised, which must be provided as a vector
#' of length two. This vector defines the range within which uniformly 
#' distributed random values will be generated and assigned.
#' 
#' @param n \code{Numeric} value, number of output reference spectra.
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
#' @param f_min \code{Numeric} value, lower boundary of the frequency range 
#' to be modelled. 
#' 
#' @param f_max \code{Numeric} value, upper boundary of the frequency range 
#' to be modelled. 
#' 
#' @param r_0 \code{Numeric} value, distance of seismic station to source
#' 
#' @param f_0 \code{Numeric} value, reference frequency (Hz)
#' 
#' @param q_0 \code{Numeric} value, ground quality factor at \code{f_0}.
#' "Reasonable value may be \code{20}" (Tsai et al. 2012).
#' 
#' @param v_0 \code{Numeric} value, phase speed of the Rayleigh wave at 
#' \code{f_0} (m/s). Assuming a shear wave velocity of about 2200 m/s, 
#' Tsai et al. (2012) yield a value of 1295 m/s for this parameter.
#' 
#' @param p_0 \code{Numeric} value, variation exponent of Rayleigh wave 
#' velocities with frequency (dimensionless)
#' 
#' @param e_0 \code{Numeric} value, exponent characterizing quality factor 
#' increase with frequency (dimensionless). "Reasonable value may be 
#' \code{0}" (Tsai et al. 2012).
#' 
#' @param n_0_a \code{Numeric} value, lower Greens function 
#' displacement amplitude coefficients. Cf. N_ij in eq. 36 in Gimbert et 
#' al. (2014) 
#' 
#' @param n_0_b \code{Numeric} value, lower Greens function 
#' displacement amplitude coefficients. Cf. N_ij in eq. 36 in Gimbert et 
#' al. (2014) 
#' 
#' @param res \code{Numeric} value, output resolution, i.e. length of the 
#' spectrum vector.
#' 
#' @return \code{List} object with model reference parameters.
#' 
#' @author Michael Dietze
#' 
#' @keywords eseis
#' 
#' @examples
#' 
#' ## create two parameter sets where h_w (water level) and q_s (sediment
#' ## flux) are randomly varied.
#' 
#' ref_pars <- fmi_parameters(n = 2,
#'                            h_w = c(0.02, 2.00),
#'                            q_s = c(0.001, 50.000) / 2650,
#'                            d_s = 0.01,
#'                            s_s = 1.35,
#'                            r_s = 2650,
#'                            w_w = 6,
#'                            a_w = 0.0075,
#'                            f_min = 5,
#'                            f_max = 80,
#'                            r_0 = 6,
#'                            f_0 = 1,
#'                            q_0 = 10,
#'                            v_0 = 350,
#'                            p_0 = 0.55,
#'                            e_0 = 0.09,
#'                            n_0_a = 0.6,
#'                            n_0_b = 0.8,
#'                            res = 100)
#' 
#' @export fmi_parameters
#' 
fmi_parameters <- function(
  
  n,
  d_s,
  s_s,
  r_s,
  q_s,
  h_w,
  w_w,
  a_w,
  f_min,
  f_max,
  r_0,
  f_0,
  q_0,
  v_0,
  p_0,
  e_0,
  n_0_a,
  n_0_b,
  res
) {
  
  ## build preliminary output structure
  pars_template <- list(d_s = d_s,
                        s_s = s_s,
                        r_s = r_s,
                        q_s = q_s,
                        w_w = w_w,
                        a_w = a_w,
                        h_w = h_w,
                        f_min = f_min,
                        f_max = f_max,
                        r_0 = r_0,
                        f_0 = f_0,
                        q_0 = q_0,
                        v_0 = v_0,
                        p_0 = p_0,
                        e_0 = e_0,
                        n_0_a = n_0_a,
                        n_0_b = n_0_b,
                        res = res)
  
  ## build model parameter catalogue
  pars_reference <- lapply(X = 1:n, FUN = function(i, pars_template) {
    
    ## copy template
    pars_reference <- pars_template
    
    ## identify parameters to randomise
    i_touch <- lapply(X = pars_reference, FUN = function(pars_reference) {
      
      return(length(pars_reference) > 1)
    })
    i_touch <- seq(1, length(i_touch))[unlist(i_touch)]
    
    for(j in 1:length(i_touch)) {
      
      pars_reference[[i_touch[j]]] <- runif(n = 1, 
                                 min = pars_template[[i_touch[j]]][1], 
                                 max = pars_template[[i_touch[j]]][2])
    }
    
    return(pars_reference)
    
  }, pars_template)
  
  ## return output
  return(pars_reference)
}