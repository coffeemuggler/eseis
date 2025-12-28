#' Start GUI with seismic models
#' 
#' This function starts a browser-based graphic user interface to explore the 
#' parameter space of seismic models that predict the spectra of turbulent 
#' water flow and bedload flux. Empirical spectra can be added if they are 
#' present in the current environment as eseis objects. Note that those 
#' spectra should be made from deconvolved input data in order to have the 
#' same units as the model spectra (dB). ATTENTION, due to an unresolved 
#' issue, there must be at least one eseis object present in the working 
#' environment.
#' 
#' @param ... further arguments to pass to \code{runApp}
#' 
#' @author Michael Dietze
#' @seealso \code{runApp}
#' @examples 
#' 
#' \dontrun{
#' # Start the GUI
#' gui_models()
#' }
#' 
#' @export gui_models

gui_models <- function(...) {
  
  example_spectrum_gui_models <- eseis::aux_initiateeseis()
  
  app <- shiny::runApp(system.file("shiny/models", 
                                   package = "eseis"), 
                       launch.browser = TRUE, 
                       ...)
  
  on.exit(rm(example_spectrum_gui_models))
}
