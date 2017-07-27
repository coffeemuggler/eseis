#' Initiate an eseis object
#' 
#' The function generates an empty eseis object, starting with processing 
#' step 0. The object contains no data and the history only contains the 
#' system information.
#' 
#' @return \code{S3} list object of class \code{eseis}.
#' 
#' @author Michael Dietze
#' @keywords eseis
#' @examples
#' 
#' ## initiate eseis object
#' aux_initiateeseis()
#'                      
#' @export aux_initiateeseis
aux_initiateeseis <- function(
  
) {
  
  ## build eseis object
  eseis <- list(signal = numeric(length = 0),
                meta = list(),
                header = list(),
                history = list("0" = list(system = sessionInfo())))
  
  ## provide eseis class name
  class(eseis) <- "eseis"
  
  ## return eseis object
  return(eseis)
}