#' List sensor library.
#' 
#' The function returns the list of supported sensors to extract signal 
#' deconvolution parameters.
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
  TC120s_ABT <- list(
    ID = "TC120s_ABT",
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
    s = 754.3,
    k = 4.34493e+17)
  
  ## define TC120s parameters, old version
  TC120s <- list(
    ID = "TC120s",
    name = "Trillium Compact 120s",
    manufacturer = "Nanometrics",
    type = "broadband seismometer",
    n_components = 3,
    comment = "Data taken from R-package RSEIS",
    poles = as.complex(c(  -3.852e-02+3.658e-02i,
                           -3.852e-02-3.658e-02i,
                           -1.780e+02+0.000e+00i,
                           -1.350e+02+1.600e+02i,
                           -1.350e+02-1.600e+02i,
                           -6.710e+02+1.154e+03i,
                           -6.710e+02-1.154e+03i)),
    zeros = as.complex(c(0.0,
                         0.0,
                         -90.0,
                         -160.7,
                         -3108.0)),
    s = 1201,
    k = 308000)
  
  ## build sensor library
  sensors <- list("TC120s_ABT" = TC120s_ABT,
                  "TC120s" = TC120s)
  
  ## return output
  return(sensors)
}
