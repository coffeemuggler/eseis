#' List library with data logger information.
#' 
#' The function returns the list of supported data loggers to extract signal 
#' deconvolution parameters.
#' 
#' The value AD is the analogue-digital conversion factor.
#' 
#' @return \code{List} object, supported loggers with their parameters.
#' @author Michael Dietze
#' @keywords eseis
#' @examples
#' 
#' ## show documented loggers
#' list_logger()
#' 
#' ## show names of loggers in list
#' names(list_logger())
#'                      
#' @export list_logger
list_logger <- function(
) {
  
  ## define Cube3ext parameters
  Cube3ext <- list(
    ID = "Cube3ext",
    name = "Cube 3ext",
    manufacturer = "Omnirecs",
    type = "The greatest data logger in the world",
    n_components = 3,
    comment = "",
    AD = 2.4414e-07)
  
  ## define Cube3ext parameters
  Cube3extBOB <- list(
    ID = "Cube3extBOB",
    name = "Cube 3ext with breakout box",
    manufacturer = "Omnirecs",
    type = "The greatest data logger in the world",
    n_components = 3,
    comment = "",
    AD = 2.4414e-06)
  
  ## define Cube3ext parameters
  Centaur <- list(
    ID = "Centaur",
    name = "Centaur",
    manufacturer = "Nanometrics",
    type = "NA",
    n_components = 3,
    comment = "Important! SD is defined for 20 V. Change if needed.",
    AD = 20/(2^24))

  ## define Cube3ext parameters
  Taurus <- list(
    ID = "Taurus",
    name = "Taurus",
    manufacturer = "Nanometrics",
    type = "NA",
    n_components = 3,
    comment = "Not tested, values copied from A. Burtins data base.",
    AD = 2.500e-07)
  
  ## define Cube3ext parameters
  Osiris <- list(
    ID = "Osiris",
    name = "Osiris",
    manufacturer = "Nanometrics",
    type = "NA",
    n_components = 3,
    comment = "Not tested, values copied from A. Burtins data base.",
    AD = 1.4000e-06)

  ## define Cube3ext parameters
  RT72A08 <- list(
    ID = "RT72A08",
    name = "Reftek RT 72A08",
    manufacturer = "Reftek",
    type = "NA",
    n_components = 3,
    comment = "Not tested, values copied from A. Burtins data base.",
    AD = 1.907e-06)
  
  ## define Cube3ext parameters
  RT130 <- list(
    ID = "RT130",
    name = "Reftek RT 130",
    manufacturer = "Reftek",
    type = "NA",
    n_components = 3,
    comment = "Not tested, values copied from A. Burtins data base.",
    AD = 1.589e-06)
  
  ## define CMG6TD parameters
  RT130 <- list(
    ID = "CMG6TD",
    name = "Guralp 6TD",
    manufacturer = "Guralp",
    type = "Logger integrated in sensor",
    n_components = 3,
    comment = "Not tested, values copied from A. Burtins data base.",
    AD = 2.609E-07)
  
  ## build sensor library
  loggers <- list("Cube3ext" = Cube3ext,
                  "Cube3extBOB" = Cube3extBOB,
                  "Centaur" = Centaur,
                  "Taurus" = Taurus,
                  "Osiris" = Osiris,
                  "RT72A08" = RT72A08,
                  "RT130" = RT130)
  
  ## return output
  return(loggers)
}
