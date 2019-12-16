#' Start GUI with seismic models
#' 
#' This function starts a browser-based graphic user interface to explore the 
#' parameter space of seismic models that predict the spectra of turbulent 
#' water flow and bedload flux.
#' 
#' @param ... further arguments to pass to \code{\link{runApp}}
#' 
#' @author Michael Dietze
#' @seealso \code{\link{runApp}}
#' @examples 
#' 
#' \dontrun{
#' # Start the GUI
#' gui_models()
#' }
#' 
#' @export gui_models
gui_models <- function(...) {
  app <- shiny::runApp(system.file("shiny/models", 
                                   package = "eseis"), 
                       launch.browser = TRUE, 
                       ...)
}