#' List sensor library.
#' 
#' The function returns the list of supported sensors to extract signal 
#' deconvolution parameters.
#' 
#' Poles and zeros must be given in rad/s. Characteristics of further 
#' sensors can be added manually. See examples of \code{signal_deconvolve}
#' for further information. The value s is the generator constant 
#' (sensitivity) given in Vs/m. The value k is the normalisation factor of 
#' the sensor.
#' 
#' @return \code{List} object, supported sensors with their parameters.
#' @author Michael Dietze
#' @keywords eseis
#' @examples
#' 
#' ## show sensors
#' list_sensor()
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
  
  TRI240 <- list(
    ID = "TRI240",
    name = "Trillium 240",
    manufacturer = "Nanometrics",
    type = "broadband seismometer",
    n_components = 3,
    comment = "Data taken from data base of Arnaud Burtin",
    poles = as.complex(c(-1.82e-2+1.80e-2i,
                         -1.82e-2-1.80e-2i,
                         -1.73e2+0.0i,
                         -1.96e2+2.31e2i,
                         -1.96e2-2.31e2i,
                         -7.32e2+1.41e3i,
                         -7.32e2-1.41e3i)),
    zeros = as.complex(c(0.0+0.0i,
                         0.0+0.0i,
                         -108.0+0.0i,
                         -161.0+0.0i)),
    s = 1200,
    k = 2.316e9)

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
    name = "Mark L4C",
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

  ## define CMG6TD parameters
  CMG6TD <- list(
    ID = "CMG6TD",
    name = "Guralp 6TD",
    manufacturer = "Guralp",
    type = "broadband seismometer",
    n_components = 3,
    comment = "Data taken from data base of Arnaud Burtin",
    poles = as.complex(c(-0.148597+0.148597i,
                         -0.148597-0.148597i,
                         -2.46936e3+0.0i,
                         -4.70635e1+0.0i,
                         -3.36765e2-1.36655e2i,
                         -3.36765e2+1.36655e2i)),
    zeros = as.complex(c(-3.16174e1+0.0i,
                         0.0+0.0i,
                         0.0+0.0i)),
    s = 2400,
    k = 4.8053e+08)
  
  ## define CMG6TD_b1is parameters
  CMG6TD_b1is <- list(
    ID = "CMG6TD_b1is",
    name = "Guralp 6TD_b1is",
    manufacturer = "Guralp",
    type = "seismometer",
    n_components = NA,
    comment = "Data taken from data base of Arnaud Burtin",
    poles = as.complex(c(-1.480e-1+-1.480e-1i,
                         -1.480e-1+1.480e-1i,
                         -3.91955e2+8.50693e2i,
                         -3.91955e2-8.50693e2i,
                         -2.199e3+0.0i,
                         -4.712e2+0.0i)),
    zeros = as.complex(c(0.0+0.0i,
                         0.0+0.0i)),
    s = 2400,
    k = 9.091e+11)
  
  ## define CMG40T parameters
  CMG40T <- list(
    ID = "CMG40T",
    name = "Guralp 40T",
    manufacturer = "Guralp",
    type = "seismometer",
    n_components = 3,
    comment = "Data taken from data base of Arnaud Burtin",
    poles = as.complex(c(-1.49e-01+1.49e-01i,
                         -1.49e-01-1.49e-01i,
                         -5.03e+02+0.0i,
                         -1.01e+03+0.0i,
                         -1.13e+03+0.0i)),
    zeros = as.complex(c(0.0+0.0i,
                         0.0+0.0i)),
    s = 800,
    k = 5.7414e+08)
  
  CMG3T <- list(
    ID = "CMG3T",
    name = "Guralp 3T",
    manufacturer = "Guralp",
    type = "broadband seismometer",
    n_components = 3,
    comment = "Data taken from data base of Arnaud Burtin",
    poles = as.complex(c(-0.0371+0.0371i,
                         -0.0371-0.0371i,
                         -459.9+236.2i,
                         -459.9-236.2i)),
    zeros = as.complex(c(0.0+0.0i,
                         0.0+0.0i,
                         920.5+0.0i)),
    s = 1500,
    k = -2.904e+02)
  
  CMG3ESP <- list(
    ID = "CMG3ESP",
    name = "Guralp 3ESP",
    manufacturer = "Guralp",
    type = "seismometer",
    n_components = 3,
    comment = "Data taken from data base of Arnaud Burtin",
    poles = as.complex(c(-0.1481+0.1481i,
                         -0.1481-0.1481i)),
    zeros = as.complex(c(0.0+0.0i,
                         0.0+0.0i)),
    s = 2000,
    k = 1)
  
  STS2 <- list(
    ID = "STS2",
    name = "Kinemetrics STS2",
    manufacturer = "Kinemetrics",
    type = "broadband seismometer",
    n_components = 3,
    comment = "Data taken from data base of Arnaud Burtin",
    poles = as.complex(c(-0.03701+0.03701i,
                         -0.03701-0.03701i,
                         -251.3+0.0i,
                         -131+467.3i,
                         -131-467.3i)),
    zeros = as.complex(c(0.0+0.0i,
                         0.0+0.0i)),
    s = 1500,
    k = 5.92e+07)
  
  L22 <- list(
    ID = "L22",
    name = "Sercel L22",
    manufacturer = "Sercel",
    type = "seismometer",
    n_components = 3,
    comment = "Data taken from data base of Arnaud Burtin",
    poles = as.complex(c(-8.886+8.886i,
                         -8.886-8.886i)),
    zeros = as.complex(c(0.0+0.0i,
                         0.0+0.0i)),
    s = 100,
    k = 1)
  
  NEOMAX <- list(
    ID = "NEOMAX",
    name = "NEOMAX",
    manufacturer = "NA",
    type = "seismometer",
    n_components = NA,
    comment = "Data taken from data base of Arnaud Burtin",
    poles = as.complex(c(-8.9692+8.9692i,
                         -8.886-8.4021i)),
    zeros = as.complex(c(0.0+0.0i,
                         0.0+0.0i)),
    s = 122.5,
    k = 1)
  
  IHR1C <- list(
    ID = "IHR1C",
    name = "Sismob IHR1C",
    manufacturer = "Sismob",
    type = "seismometer",
    n_components = 1,
    comment = "Data taken from data base of Arnaud Burtin",
    poles = as.complex(c(-8.7965+8.9742i,
                         -8.7965-8.9742i)),
    zeros = as.complex(c(0.0+0.0i,
                         0.0+0.0i)),
    s = 1920,
    k = 4.1037)
  
  KINKEI <- list(
    ID = "KINKEI",
    name = "KINKEI",
    manufacturer = "NA",
    type = "seismometer",
    n_components = NA,
    comment = "Data taken from data base of Arnaud Burtin",
    poles = as.complex(c(-8.7965+8.9742i,
                         -8.7965-8.9742i)),
    zeros = as.complex(c(0.0+0.0i,
                         0.0+0.0i)),
    s = 80,
    k = 6.4400e-07)
  
  PE6B <- list(
    ID = "PE6B",
    name = "PE-6/B",
    manufacturer = "unknown",
    type = "geophone",
    n_components = 3,
    comment = "Data taken from GIPP website",
    poles = as.complex(c(-15.88+23.43i,
                         -15.88-23.43i)),
    zeros = as.complex(c(0.0+0.0i,
                         0.0+0.0i)),
    s = 28.8,
    k = 1)
  
  
  ## build sensor library
  sensors <- list("TC120s" = TC120s,
                  "TRI240" = TRI240,
                  "LE3D5S" = LE3D5S,
                  "L4C" = L4C,
                  "CMG6TD" = CMG6TD,
                  "CMG6TD_b1is" = CMG6TD_b1is,
                  "CMG40T" = CMG40T,
                  "CMG3T" = CMG3T,
                  "CMG3ESP" = CMG3ESP,
                  "STS2" = STS2,
                  "L22" = L22,
                  "NEOMAX" = NEOMAX,
                  "IHR1C" = IHR1C,
                  "KINKEI" = KINKEI,
                  "PE6B" = PE6B)
  
  ## return output
  return(sensors)
}
