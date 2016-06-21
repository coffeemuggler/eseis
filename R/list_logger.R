#' List logger library.
#' 
#' The function returns the list of supported data loggers to extract signal 
#' deconvolution parameters.
#' 
#' @return \code{List} object, supported loggers with their parameters.
#' @author Michael Dietze
#' @keywords eseis
#' @examples
#' 
#' ## show sensors
#' list_logger
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
  
  ## build sensor library
  loggers <- list("Cube3ext" = Cube3ext)
  
  ## return output
  return(loggers)
}
