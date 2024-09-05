#' Create multiple single-component files from multi-component files
#' 
#' The function changes the meta data and file names of seismic data sets
#' (organised in an eseis-compatible data structure). Data cubes in 
#' combination with splitter break-out boxes allow to record vertical 
#' component signals from three single-channel geophones, each connected by 
#' an individual cable. The logger interprets the data as E, N and Z 
#' components, regardless of the actual sensor and component connected to 
#' the respective cable. Hence, it is possible to record data from three 
#' independent (vertical) sensors with a single Data cube. However, the 
#' channels will be named according to the internal scheme (E, N, Z). To 
#' correct for that effect, the function will look up the right combination 
#' of station ID and component, and change meta data and name of each file
#' in the input directory accordingly, before saving the updated file in the 
#' output directory.
#'
#' @param input \code{Character} value, path to directory where the Cube 
#' files to be processed are stored.
#' 
#' @param output \code{Character} value, path to directory where output data is 
#' written. It is not recommended to use the same directory for output and 
#' input, as files may be overwritten unintentionally.
#' 
#' @param ID_in \code{Character} vector with the IDs of the stations that 
#' will be changed. Note that for every channel of the input data, the 
#' corresponding ID must be repeated. See details for further information.
#' 
#' @param component_in \code{Character} vector with the components of the 
#' input stations that will be changed. The length must match the length of 
#' \code{ID_in} as well as \code{ID_out} and \code{component_out}.
#' 
#' @param ID_out \code{Character} vector with the IDs of the output files
#' corresponding to the input stations and components.
#' 
#' @param component_out \code{Character} vector with the components of the 
#' output stations.
#' 
#' @param gipptools \code{Character} value, path to gipptools or 
#' cubetools directory. Only needed when mseed files are processed
#' 
#' @return A set of converted and organised seismic files written to disk.
#' 
#' @author Michael Dietze
#' 
#' @keywords eseis
#' 
#' @examples
#'
#' \dontrun{
#' 
#' ## the example will convert the E, N and Z components of station RUEG1
#' ## to the station IDs RUEGA, RUEGB, RUEGC, all with BHZ as component
#' 
#' aux_splitcubechannels(input =  paste0(system.file("extdata", 
#'                                       package="eseis"), "/"), 
#'                       output = tempdir(), 
#'                       ID_in = c("RUEG1", "RUEG1", "RUEG1"), 
#'                       component_in = c("BHE", "BHN", "BHZ"),
#'                       ID_out = c("RUEGA", "RUEGB", "RUEGC"),
#'                       component_out = c("BHZ", "BHZ", "BHZ"))
#' 
#' }
#'                         
#' @export aux_splitcubechannels

aux_splitcubechannels <- function(

  input,
  output,
  ID_in,
  component_in,
  ID_out,
  component_out,
  gipptools
  
) {
  
  ## check/set input directory
  if(missing(input) == TRUE) {
    
    stop("No input directory provided!")
    
  } else if(dir.exists(input) == FALSE) {
    
    stop("Input directory does not exist!")
  }
  
  ## check/set output directory
  if(missing(output) == TRUE) {
    
    output <- getwd()
    warning("No output directory provided! Writing to working directory.")
    
  } else if(dir.exists(output) == FALSE) {
    
    stop("Output directory does not exist!")
  }
  
  ## set/correct path definitions
  input <- paste0(dirname(input), "/", basename(input), "/")
  output <- paste0(dirname(output), "/", basename(output), "/")
  
  ## check consistent lengths of information vectors
  l_id_in <- length(ID_in)
  l_id_out <- length(ID_out)
  l_cmp_in <- length(component_in)
  l_cmp_out <- length(component_out)
  
  if(l_id_out != l_id_in) {
    stop("Output IDs must be as many as input IDs!")
  }
  
  if(l_cmp_in != l_id_in) {
    stop("Input components must be as many as input IDs!")
  }
  
  if(l_cmp_out != l_id_in) {
    stop("Output components must be as many as input IDs!")
  }
  
  ## convert vectors to data frame
  info <- data.frame(ID_in = ID_in, 
                     ID_out = ID_out,
                     cmp_in = component_in,
                     cmp_out = component_out)

  ## make list of input files
  files_in <- list.files(path = input, 
                         recursive = TRUE,
                         full.names = FALSE)
  
  ## create output sub directories
  dirs_in <- list.dirs(input)[-1]
  for(i in 1:length(dirs_in)) {
    
    ## change input for output directory 
    dir_out <- gsub(x = dirs_in[i],
                    pattern = input,
                    replacement = output)
    
    ## optionally create output directory
    if(dir.exists(dir_out) == FALSE) {
      
      dir.create(dir_out)
    }
  }
  
  ## process each file
  for(i in 1:length(files_in)) {
  
    ## isolate file name
    file_i <- tail(strsplit(x = files_in[i], 
                            split = "/", 
                            fixed = TRUE)[[1]],1)
    file_i <- strsplit(x = file_i, split = ".", fixed = TRUE)[[1]]
    
    ## extract station ID
    ID_i <- file_i[file_i %in% info$ID_in]
    
    ## extract component information
    cmp_i <- file_i[file_i %in% info$cmp_in]
    
    ## isolate matching new file information
    info_i <- info[info$ID_in == ID_i & info$cmp_in == cmp_i,]

    if(nrow(info_i) == 1) {
      
      ## define new file name
      file_out_i <- files_in[i]
      file_out_i <- gsub(x = file_out_i, 
                         pattern = info_i$ID_in, 
                         replacement = info_i$ID_out)
      file_out_i <- gsub(x = file_out_i, 
                         pattern = info_i$cmp_in, 
                         replacement = info_i$cmp_out)
      
      ## try to read file as sac
      s <- try(suppressWarnings(eseis::read_sac(
        file = paste0(input, files_in[i]))), silent = TRUE)
      
      ## find out file type
      if(class(s)[1] == "eseis") {
        
        if(s$meta$n >= 1) {
          
          format <- "sac"
        } else {
          
          format <- "mseed"
        }
      } else {
        
        format <- "mseed"
      }
      
      ## handle file based on file format
      if(format == "sac") {
        
        ## CASE A - sac format
        
        ## read sac file
        s <- try(read_sac(file = paste0(input, files_in[i])), silent = TRUE)
        
        ## check successful import
        if(class(s)[1] == "eseis") {
          
          ## update meta info
          s$meta$station <- info_i$ID_out
          s$meta$component <- info_i$cmp_out
          
          ## save file under new name
          try(write_sac(data = s, 
                        unit = "unknown", 
                        file = paste0(output, file_out_i)), 
              silent = TRUE)
        }
        
      } else {
        
        ## CASE B - mseed format
        
        ## check/set gipptools directory
        if(missing(gipptools) == TRUE) {
          
          stop("Path to gipptools missing!")
          
        } else if(dir.exists(gipptools) == FALSE) {
          
          stop("Path to gipptools is wrong!")
        }
        
        ## check if gipptools are available
        if(file.exists(paste0(gipptools, "/bin/cube2mseed")) | 
           file.exists(paste0(gipptools, "/bin/cube2mseed.cmd")) == TRUE) {
          
          test_gipptools <- NULL
        } else {
          
          stop("No gipptools function(s) available under that path!")
        }
        
        ## correct path definition to gipptools
        gipptools <- paste0(dirname(gipptools), "/", basename(gipptools), "/")
        
        ## edit and re-save mseed file
        try(system(command = paste0(gipptools, "/bin/mseed2mseed", 
                                " --set-station=", info_i$ID_out,
                                " --set-channel=", info_i$cmp_out,
                                " ", paste0(input, files_in[i]),
                                " > ", paste0(output, file_out_i))))
      }
    }
  }
}