#' List sensor library.
#' 
#' The function returns the list of supported sensors to extract signal 
#' deconvolution parameters.
#' 
#' Poles and zeros must be given in rad/s. Characteristics of further 
#' sensors can be added manually. See examples of \code{signal_deconvolve}
#' for further information.
#' 
#' @return \code{List} object, supported sensors with their parameters.
#' @author Michael Dietze
#' @keywords eseis
#' @examples
#' 
#' ## show sensors
#' list_sensor
#'                      
#' @export list_sensor
list_sensor <- function(
) {
  
  ## define TC120s parameters
  TC120s <- list(
    ID = "TC120s",
    name = "Trillium Compact 120s",
    manufacturer = "Nanometrics",
    type = "broadband seismometer",
    n_components = 3,
    comment = "Data taken from data base of Arnaud Burtin",
    poles = as.complex(c(0.03691+0.03702i,
                         0.03691-0.03702i,
                         -343.0+0.0i,
                         -370.0+467.0i,
                         -370.0-467.0i,
                         -836.0+1522.0i,
                         -836.0-1522.0i,
                         -4900.0+4700.0i,
                         -4900.0-4700.0i,
                         -6900.0+0.0i,
                         -15000+0.0i)),
    zeros = as.complex(c(0.0+0.0i,
                         0.0+0.0i,
                         -392.0+0.0i,
                         -1960.0+0.0i,
                         -1490.0+1740.0i,
                         -1490-1740.0i)),
    s = 749.1,
    k = 4.34493e+17)
  
  ## define LE3D5S parameters
  LE3D5S <- list(
    ID = "LE3D5S",
    name = "LE-3D/5s",
    manufacturer = "Lennartz",
    type = "seismometer",
    n_components = 3,
    comment = "Data taken from data base of Arnaud Burtin",
    poles = as.complex(c(-0.8886e+1+0.8886i,
                         -0.8886e+1-0.8886i,
                         -0.220e+1+0.0i)),
    zeros = as.complex(c(0.0e+1+0.0i, 
                         0.0e+1+0.0i, 
                         0.0e+1+0.0i)),
    s = 400,
    k = 1.9139)
  
  ## define L4C parameters
  L4C <- list(
    ID = "L4C",
    name = "L4C",
    manufacturer = "Mark",
    type = "seismometer",
    n_components = 1,
    comment = "Data taken from data base of Arnaud Burtin",
    poles = as.complex(c(-4.443e+1+4.443i, 
                         -4.443e+1-4.443i)),
    zeros = as.complex(c(0.0e+1+0.0i, 
                         0.0e+1+0.0i)),
    s = 166.54,
    k = 1.9139)
  
 
  ## build sensor library
  sensors <- list("TC120s" = TC120s)
  
  ## return output
  return(sensors)
}
