#' Create reference model spectra catalogue
#' 
#' In order to run the fluvial model inversion (FMI) routine, a look-up 
#' table with reference spectra needs to be created (based on predefined 
#' model parameters). This function does this job.
#'
#' @param parameters \code{List} containing lists with model parameters 
#' for which the spectra shall be calculated.
#' 
#' @param n_cores \code{Numeric} value, number of CPU cores to use. Disabled 
#' by setting to 1. Default is 1.
#' 
#' @return \code{List} object containing the calculated reference spectra 
#' and the corresponding input parameters.
#' 
#' @author Michael Dietze
#' 
#' @keywords eseis
#' 
#' @examples
#' 
#' ## create 2 example reference parameter sets
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
#' ## create corresponding reference spectra
#' ref_spectra <- fmi_spectra(parameters = ref_pars)
#' 
#' @export fmi_spectra
#' 
fmi_spectra <- function (
  
  parameters,
  n_cores = 1
) {
  
  ## define helper function
  f <- function(parameters) {
    
    ## model spectrum due to water flow
    p_turbulence <- eseis::model_turbulence(d_s = parameters$d_s, 
                                            s_s = parameters$s_s, 
                                            r_s = parameters$r_s, 
                                            h_w = parameters$h_w, 
                                            w_w = parameters$w_w, 
                                            a_w = parameters$a_w, 
                                            f = c(parameters$f_min,
                                                  parameters$f_max), 
                                            r_0 = parameters$r_0, 
                                            f_0 = parameters$f_0, 
                                            q_0 = parameters$q_0, 
                                            v_0 = parameters$v_0, 
                                            p_0 = parameters$p_0, 
                                            n_0 = c(parameters$n_0_a,
                                                    parameters$n_0_b), 
                                            res = parameters$res,
                                            eseis = FALSE)
    
    ## model spectrum due to bedload impacts
    p_bedload <- eseis::model_bedload(d_s = parameters$d_s,
                                      s_s = parameters$s_s,
                                      r_s = parameters$r_s,
                                      q_s = parameters$q_s,
                                      h_w = parameters$h_w,
                                      w_w = parameters$w_w,
                                      a_w = parameters$a_w,
                                      f = c(parameters$f_min,
                                            parameters$f_max), 
                                      r_0 = parameters$r_0,
                                      f_0 = parameters$f_0,
                                      q_0 = parameters$q_0,
                                      e_0 = parameters$e_0,
                                      v_0 = parameters$v_0,
                                      x_0 = parameters$p_0,
                                      n_0 = parameters$n_0_a,
                                      res = parameters$res,
                                      eseis = FALSE)
    
    ## combine model outputs
    p_combined <- p_turbulence
    p_combined$spectrum <- p_turbulence$spectrum + p_bedload$spectrum
    
    ## convert linear to log scale
    p_turbulence_log <- p_turbulence
    p_bedload_log <-p_bedload
    p_combined_log <- p_combined
    
    p_turbulence_log$spectrum <- 10 * log10(p_turbulence$spectrum)
    p_bedload_log$spectrum <- 10 * log10(p_bedload$spectrum)
    p_combined_log$spectrum <- 10 * log10(p_combined$spectrum)
    
    ## return model outputs
    return(list(pars = parameters,
                frequency = p_combined_log$frequency,
                spectrum = p_combined_log$spectrum))
  }
  
  ## optinally initiate multicore environment
  if(n_cores > 1) {
    
    ## detect number of CPU cores
    n_cores_system <- parallel::detectCores()
    n_cores <- ifelse(test = n_cores > n_cores_system, 
                      yes = n_cores_system,
                      no = n_cores)
    
    ## initiate cluster
    cl <- parallel::makeCluster(n_cores)
    
    ## call helper function
    spectra <- parallel::parLapply(cl = cl,
                                   X = parameters, 
                                   fun = f)
    
    ## stop cluster
    parallel::stopCluster(cl = cl)
    
  } else {
    
    spectra <- lapply(X = parameters, FUN = f)
  }

  ## return output
  return(spectra)
}