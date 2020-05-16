#' Get cube file information 
#' 
#' This is a simple wrapper for the Gipptools program \code{cubeinfo}, 
#' providing a short summary of the cube file meta data, in a coherent 
#' data frame structure.
#' 
#' @param file \code{Characater} value, cube file to be processes
#' 
#' @param gipptools \code{Character} value, path to gipptools or 
#' Gipptools directory.
#' 
#' @return \code{data frame} with cube meta data
#' 
#' @author Michael Dietze
#' 
#' @keywords eseis
#' 
#' @examples
#' 
#' \dontrun{
#' 
#' ## get cube info
#' x = aux_cubeinfo(file = "data/cube/example.ATB", 
#'                  gipptools = "/software/gipptools-2019.332/")
#' 
#' }
#'
#' @export aux_cubeinfo
#' 
aux_cubeinfo <- function(
  
  file, 
  gipptools
) {

  ## get cube info output
  x <- system(command = paste0(gipptools, "bin/cubeinfo ", file), 
              intern = TRUE)
  
  ## remove comments and empty rows
  x <- x[grepl(x = x, 
               pattern = "# ---") == FALSE]
  x <- x[nchar(x) > 0]
  
  ## separate parameter names and values
  x_parameter <- trimws(substr(x = x, 
                               start = 3, 
                               stop = 26), 
                        which = "left")
  
  x_value <- trimws(substr(x = x, 
                           start = 30, 
                           stop = 100), 
                    which = "right")
  
  ## organise output as data frame
  x_tabular <- data.frame(rbind(x_value),
                          stringsAsFactors = FALSE)
  names(x_tabular) <- x_parameter
  rownames(x_tabular) <- ""
  
  ## return output
  return(x_tabular)
}

