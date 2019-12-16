##/////////////////////////////////////////////////////////////////////////////
##//zzz.R
##/////////////////////////////////////////////////////////////////////////////
##
##=============================================================================
##author: Michael Dietze
##=============================================================================

##==============================================================================

.onAttach <- function(libname, pkgname){
  
  ## show startup message
  try(packageStartupMessage("Welcome to another bright 'eseis' session."), 
      silent = TRUE)
  
  ## try to load obspy library
  obspy <- try(reticulate::import("obspy"),
               silent = TRUE)
}
