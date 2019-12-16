#' Invert fluvial data set based on reference spectra catalogue
#' 
#' The fluvial model inversion (FMI) routine uses a predefined look-up table 
#' with reference spectra to identify those spectra that fit the empirical 
#' data set best, and returns the corresponding target variables and fit 
#' errors.
#' 
#' Note that the frequencies of the empiric and modelled data sets must 
#' match.
#'
#' @param reference \code{List} containing lists with precalculated model 
#' spectra.
#' 
#' @param data \code{eseis} object or \code{numeric} matrix (spectra organised
#' by columns), empiric spectra which are used to identify the best matching 
#' target parameters of the reference data set.
#' 
#' @param n_cores \code{Numeric} value, number of CPU cores to use. Disabled 
#' by setting to 1. Default is 1.
#'  
#' @return \code{List} object containing the inversion results.
#' 
#' @author Michael Dietze
#' 
#' @keywords eseis
#' 
#' @examples
#' 
#' ## NOTE THAT THE EXAMPLE IS OF BAD QUALITY BECAUSE ONLY 10 REFERENCE 
#' ## PARAMETER SETS AND SPECTRA ARE CALCULATED, DUE TO COMPUATATION TIME
#' ## CONSTRAINTS. SET THE VALUE TO 1000 FOR MORE MEANINGFUL RESULTS.
#' 
#' ## create 100 example reference parameter sets
#' ref_pars <- fmi_parameters(n = 10,
#'                            h_w = c(0.02, 1.20),
#'                            q_s = c(0.001, 8.000) / 2650,
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
#' ## define water level and bedload flux time series
#' h <- c(0.01, 1.00, 0.84, 0.60, 0.43, 0.32, 0.24, 0.18, 0.14, 0.11)
#' q <- c(0.05, 5.00, 4.18, 3.01, 2.16, 1.58, 1.18, 0.89, 0.69, 0.54) / 2650
#' hq <- as.list(as.data.frame(rbind(h, q)))
#' 
#' ## calculate synthetic spectrogram
#' psd <- do.call(cbind, lapply(hq, function(hq) {
#'
#'   psd_turbulence <- eseis::model_turbulence(h_w = hq[1],
#'                                             d_s = 0.01,
#'                                             s_s = 1.35,
#'                                             r_s = 2650,
#'                                             w_w = 6,
#'                                             a_w = 0.0075,
#'                                             f = c(10, 70),
#'                                             r_0 = 5.5,
#'                                             f_0 = 1,
#'                                             q_0 = 18,
#'                                             v_0 = 450,
#'                                             p_0 = 0.34,
#'                                             e_0 = 0.0,
#'                                             n_0 = c(0.5, 0.8),
#'                                             res = 100, 
#'                                             eseis = FALSE)$spectrum
#' 
#'   psd_bedload <- eseis::model_bedload(h_w = hq[1],
#'                                       q_s = hq[2],
#'                                       d_s = 0.01,
#'                                       s_s = 1.35,
#'                                       r_s = 2650,
#'                                       w_w = 6,
#'                                       a_w = 0.0075,
#'                                       f = c(10, 70),
#'                                       r_0 = 5.5,
#'                                       f_0 = 1,
#'                                       q_0 = 18,
#'                                       v_0 = 450,
#'                                       x_0 = 0.34,
#'                                       e_0 = 0.0,
#'                                       n_0 = 0.5,
#'                                       res = 100,
#'                                       eseis = FALSE)$spectrum
#'
#'   ## combine spectra
#'   psd_sum <- psd_turbulence + psd_bedload
#' 
#'   ## return output
#'   return(10 * log10(psd_sum))
#' }))
#' 
#' graphics::image(t(psd))
#' 
#' ## invert empiric data set
#' X <- fmi_inversion(reference = ref_spectra, 
#'                    data = psd)
#' 
#' ## plot model results
#' plot(X$parameters$q_s * 2650, 
#'      type = "l")
#' plot(X$parameters$h_w, 
#'      type = "l")
#' 
#' @export fmi_inversion
#' 
fmi_inversion <- function (
  
  reference,
  data,
  n_cores = 1
) {
  
  ## convert empiric data set to list
  if(class(data)[1] == "eseis") {
    if(data$meta$type == "spectrogram") {
      
      psd_list <- as.list(as.data.frame(data$PSD$S))
    } else {
      
      stop("This eseis object is not a spectrogram")
    }
  } else {
    
    psd_list <- as.list(as.data.frame(data))
  }
  
  ## convert list of spectra to matrix
  reference_spectra <- do.call(rbind, 
                               lapply(X = reference, FUN = function(x) {
                                 x$spectrum
                               }))
  
  ## convert list of parameters to matrix
  reference_parameters <- do.call(rbind, 
                                  lapply(X = reference, FUN = function(x) {
                                    unlist(x$pars)
                                  }))
  reference_parameters <- as.data.frame(reference_parameters)
  
  ## run the inversion process
  if(n_cores > 1) {
    
    ## detect number of CPU cores
    n_cores_system <- parallel::detectCores()
    n_cores <- ifelse(test = n_cores > n_cores_system, 
                      yes = n_cores_system,
                      no = n_cores)
    
    ## initiate cluster
    cl <- parallel::makeCluster(n_cores)
    
    ## invert the data set
    inversion <- 
      parallel::parLapply(cl = cl, X = psd_list, fun = function(psd, reference) {
      
      if(sum(is.na(psd)) == 0) {
        
        ## calculate differences to model spectra
        d <- t(t(reference_spectra) - psd)
        
        ## calculate RMSE    
        rmse <- sqrt(apply(X = d^2,
                           MARGIN = 1, 
                           FUN = mean))
        
        ## get minimum RMSE among reference spectra
        rmse_min <- min(rmse)
        
        ## find best model      
        mod_best <- which(rmse == rmse_min)[1]
        
        ## get frequency-resolved RMSE for best model
        rmse_f <- sqrt((reference_spectra[mod_best,] - psd)^2)
        
      } else {
        
        ## account for inevaluable data sets
        mod_best <- NA
        rmse_f <- rep(NA, length(psd))
        rmse <- NA
        rmse_min <- NA
      }
      
      ## return output
      return(list(rmse_f = rmse_f,
                  rmse = rmse_min,
                  mod_best = mod_best))
    }, reference)
    
    ## stop cluster
    parallel::stopCluster(cl = cl)
    
  } else {
    
    inversion <- lapply(X = psd_list, FUN = function(psd, reference) {
      
      if(sum(is.na(psd)) == 0) {
        
        ## calculate differences to model spectra
        d <- t(t(reference_spectra) - psd)
        
        ## calculate RMSE    
        rmse <- sqrt(apply(X = d^2,
                           MARGIN = 1, 
                           FUN = mean))
        
        ## get minimum RMSE among reference spectra
        rmse_min <- min(rmse)
        
        ## find best model      
        mod_best <- which(rmse == rmse_min)[1]
        
        ## get frequency-resolved RMSE for best model
        rmse_f <- sqrt((reference_spectra[mod_best,] - psd)^2)
        
      } else {
        
        ## account for inevaluable data sets
        mod_best <- NA
        rmse_f <- rep(NA, length(psd))
        rmse <- NA
        rmse_min <- NA
      }
      
      ## return output
      return(list(rmse_f = rmse_f,
                  rmse = rmse_min,
                  mod_best = mod_best))
    }, reference)
  }
  
  ## isolate index of best fit model
  model_best = as.numeric(unlist(lapply(inversion, FUN = function(x) {
    x$mod_best
  })))
  
  ## isolate best fit parameters and organise them as data frame
  parameters_out <- as.data.frame(matrix(nrow = length(model_best),
                                         ncol = ncol(reference_parameters)))
  names(parameters_out) <- names(reference_parameters)
  
  for(i in 1:ncol(parameters_out)) {
    
    parameters_out[,i] <- reference_parameters[model_best,i]
  }
  
  ## organise frequency-wise RMSE
  rmse = do.call(rbind, lapply(inversion, FUN = function(x) {
    x$rmse_f
  }))
  
  ## return output
  return(list(parameters = parameters_out,
              rmse = rmse))
}