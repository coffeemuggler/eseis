#' Convert Datacube files and organise them in directory structure
#' 
#' The function converts Omnirecs/Digos Datacube files to mseed or sac files 
#' and organises these in a coherent directory structure (see details) for 
#' available structures. The conversion depends on the gipptools software
#' package (see details) provided externally.
#' 
#' @details
#' 
#' The function converts seismic data from the binary cube file format to  
#' mseed (cf. \code{read_mseed}) or sac (cf. \code{read_sac}) and organises 
#' the resulting files into a consistent structure, expected by 'eseis' for 
#' convenient data handling (cf. \code{read_data}). 
#' 
#' Currently, there are two data structure schemes supported, \code{"eseis"}  
#' and \code{"seiscomp"}. In the \code{"eseis"} case, the daily cube files are 
#' cut to hourly files and organised in directories structured by four digit 
#' year and three digit Julian day numbers. In each Julian day directory, the 
#' hourly files are placed and named after the following scheme: 
#' STATION.YEAR.JULIANDAY.HOUR.MINUTE.SECOND.COMPONENT.
#' 
#' The \code{"seiscomp"} case will yield daily files, which are organised by
#' four digit year, seismic network, seismic station, and seismic component, 
#' each building a separate directory. In the deepest subdirectory, files are 
#' named by: NETWORK.STATION.LOCATION.COMPONENT.TYPE.YEAR.JULIANDAY.
#' 
#' The component naming scheme defines the codes for the sensor's band 
#' code (first letter) and instrument code (second letter). The third letter, 
#' defining the spatial component, will be added automatically. For definitions 
#' of channel codes see \code{https://migg-ntu.github.io/SeisTomo_Tutorials/seismology/seismic-data/seismic-time-series-data.html}.
#' 
#' The function requires that the software gipptools 
#' (\code{http://www.gfz-potsdam.de/en/section/geophysical-deep-sounding/infrastructure/geophysical-instrument-pool-potsdam-gipp/software/gipptools/}) 
#' is installed. Note that the gipptools are provided at regular update 
#' intervals, including an up to date GPS leap second table, essential to 
#' convert recently recorded files.
#' 
#' The Cube files will be imported in place but a series of temporary files 
#' will be created in a temporary directory in the specified output directory.  
#' Hence, if the routine stops due to a processing issue, one needs to delete  
#' the temporary data manually. The path to the temporary directory will be 
#' provided as screen output when the argument \code{verbose = TRUE}.
#' 
#' The Cube files can be converted in two modes: \code{"file-wise"} and 
#' \code{"dir-wise"}. In \code{"file-wise"} mode, each Cube file will be 
#' converted individually. This option has the advantage that if one file in 
#' a month-long sequence of records is corrupt, the conversion will not stop,
#' but only discard the part from the corrupted section until the file end. 
#' The disadvantage is however, that the data before the first and after 
#' the last GPS tags will not be converted unless the option 
#' \code{fringe = "constant"} (by default this is the case) is used.
#' 
#' In \code{"dir-wise"} mode, the fringe sample issue reduces to the margins of  
#' the total sequence of daily files but the corrupt file issue will become a 
#' more severe danger to the success when converting a large number of files.
#' 
#' Specifying an input directory (\code{input}) is mandatory. That 
#' directory should only contain the directories with the cube files to 
#' process. Files downloaded from a Cube are usually contained in one or more 
#' further directories, which should be moved into a single one before 
#' running this function.
#' 
#' Each set of cube files from a given logger should be located in a separate 
#' directory per logger and these directories should have the same name as 
#' the logger IDs (\code{logger_ID}). An appropriate structure for files from
#' two loggers, A1A and A1B, would be something like: \cr 
#' \enumerate{
#'   \item input
#'   \enumerate{
#'     \item A1A
#'       \enumerate{
#'         \item file1.A1A
#'         \item file2.A1A
#'       }
#'     \item A1B
#'       \enumerate{
#'         \item file1.A1B
#'         \item file2.A1B
#'       }
#'    }
#' }
#' 
#' The component definition can follow the typical keywords and key letters 
#' defined in seismology: \code{https://migg-ntu.github.io/SeisTomo_Tutorials/seismology/seismic-data/seismic-time-series-data.html},
#' hence the first letter indicating the instrument's band type and the second 
#' letter indicating the instrument code or instrument type.
#' 
#' | Band code | Explanation band type  |
#' | --------- | ---------------------- |
#' | E         | Extremely short period |
#' | S         | Short period           |
#' | H         | High broad band        |
#' | B         | Broad band             |
#' | M         | Mid band               |
#' | L         | Long band              |
#' | V         | Very long band         |
#' 
#' | Instrument code | Explanation               |
#' | --------------- | ------------------------- |
#' | H               | High gain seismometer     |
#' | L               | Low gain seismometer      |
#' | G               | Gravimeter                |
#' | M               | Mass position seismometer |
#' | N               | Accelerometer             |
#' | P               | Geophone                  |
#' @md
#' 
#' @param station \code{data frame} with seismic station information
#' See \code{aux_stationinfofile}. This \code{data frame} can also be provided 
#' manually, in which case it must contain two elements: a first vector that 
#' contains the Cube IDs, and a second that contains the corresponding 
#' station IDs that will be used in the meta info and file names. If the 
#' argument is omitted, the Cube IDs will be used as station IDs. 
#' 
#' @param input \code{Character} value, path to directory where the Cube 
#' files to be processed are stored. 
#' 
#' @param output \code{Character} value, path to directory where output 
#' data is written to.
#' 
#' @param gipptools \code{Character} value, path to gipptools or cubetools 
#' directory. 
#' 
#' @param format \code{Character} value, output file format. One out of 
#' \code{"mseed"} and \code{"sac"}. Default is \code{"sac"}.
#' 
#' @param pattern \code{Character} value, file organisation scheme keyword. 
#' One out of \code{"eseis"} and \code{"seiscomp"}. Default is \code{"eseis"}. 
#' See details and \code{read_data} for further information.
#' 
#' @param component \code{Character} vector, component code and output file 
#' extension prefix. It is assumed that this prefix comprises two characters, 
#' the first describing the band code, the second the instrument code. See 
#' details for further information. Default is \code{"BH"}, hence broadband 
#' and high gain sensor. The spatial component ("E", "N", "Z") will be added
#' automatically. See details for a tabular overview of common band codes and 
#' instrument codes.
#' 
#' @param mode \code{Character} value, mode of file conversion. One out of 
#' \code{"file-wise"} and \code{"dir-wise"}. Default is \code{"file-wise"}. 
#' See details for further important information.
#' 
#' @param fringe \code{Character} value, option to handle data outside the 
#' GPS-tagged time span. One out of \code{"skip"}, \code{"nominal"} or 
#' \code{"constant"}. Default is \code{"constant"}.
#' 
#' @param cpu \code{Numeric} value, fraction of CPUs to use for parallel 
#' processing. If omitted, one CPU is used.
#' 
#' @param verbose \code{Logical} value, option to enable extended screen 
#' output of cubetools operations. Default is \code{FALSE}. This option 
#' might not work with Windows operating systems.
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
#' ## basic example with minimum effort
#' aux_organisecubefiles(stationfile = data.frame(logger = c("A1A", "A1B"),
#'                                                station = c("ST1", "ST2")), 
#'                       input = "input", 
#'                       gipptools = "software/gipptools-2023.352")
#' 
#' }
#'                         
#' @export aux_organisecubefiles

aux_organisecubefiles <- function(
    
  station,
  input,
  output,
  gipptools,
  format = "sac",
  pattern = "eseis",
  component = "BH",
  mode = "dir-wise",
  fringe = "constant",
  cpu,
  verbose = TRUE
) {
  
  ## Part 1 - checks, tests, adjustments --------------------------------------
  
  ## check station info file correctness
  if(inherits(x = station, what = "data.frame") == FALSE) {
    
    stop("Station info table is not a data frame!")
  } else {
    
    if(("ID" %in% names(station) | "logger_ID" %in% names(station)) == FALSE) {
      
      if(ncol(station) != 2) {
        
        stop("station info table does not contain elements ID and logger_ID")
      } else {
        
        ## update names for consistency
        names(station) <- c("logger_ID", "ID")
      }
    }
  }
  
  ## check/set validity of software directory
  if(missing(gipptools) == TRUE) {
    
    stop("Path to gipptools missing!")
    
  } else if(dir.exists(gipptools) == FALSE) {
    
    stop("Path to gipptools is wrong!")
  }
  
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
  
  ## check if gipptools are available
  if(file.exists(paste0(gipptools, "/bin/cube2mseed")) | 
     file.exists(paste0(gipptools, "/bin/cube2mseed.cmd")) == TRUE) {
  
    test_gipptools <- NULL
  } else {
    
    stop("No gipptools function(s) available under that path!")
  }
  
  ## set/correct path definitions
  gipptools <- paste0(dirname(gipptools), "/", basename(gipptools), "/")
  input <- paste0(dirname(input), "/", basename(input), "/")
  output <- paste0(dirname(output), "/", basename(output), "/")
  
  ## remove potential double slashes
  gipptools <- gsub(x = gipptools, pattern = "//", 
                    replacement = "/", fixed = TRUE)
  input <- gsub(x = input, pattern = "//", 
                replacement = "/", fixed = TRUE)
  output <- gsub(x = output, pattern = "//", 
                 replacement = "/", fixed = TRUE)
  
  ## check if gipptools actually work
  gt_hi <- try(system(paste0(gipptools, "/bin/cubeinfo --version"), 
                      intern = TRUE, ignore.stderr = TRUE), silent = TRUE)
  gt_hi <- sum(grepl(x = gt_hi, pattern = "Release: GIPPtools")) > 0
  
  if(gt_hi == FALSE) {
    
    stop("Gipptools function(s) not working! Java installed?")
  }
  
  ## check file format keywords
  if(format %in% c("sac", "mseed") == FALSE) {
    
    stop("File format keyword not supported!")
  }
  
  ## check pattern keywords
  if(pattern %in% c("eseis", "seiscomp") == FALSE) {
    
    stop("File organisation pattern keyword not supported!")
  }
  
  ## check mode keywords
  if(mode %in% c("dir-wise", "file-wise") == FALSE) {
    
    stop("File conversion mode keyword not supported!")
  }
  
  ## check fringe sample keywords
  if(fringe %in% c("constant", "skip", "nominal") == FALSE) {
    
    stop("Cube fringe sample keyword not supported!")
  }
  
  ## check/set fraction of CPUs to use
  if(missing(cpu) == TRUE) {
    
    cpu <- 0
  }
  
  ## PREPARATION STEPS --------------------------------------------------------
  
  ## detect and adjust number of cores to use
  cores <- parallel::detectCores()
  n_cpu <- floor(cores * cpu)
  n_cpu <- ifelse(n_cpu < 1, 1, n_cpu)
  cores <- ifelse(cores < n_cpu, cores, n_cpu)
  
  ## initiate cluster
  cl <- parallel::makeCluster(getOption("mc.cores", cores))
  
  ## create temporary directories
  temp <- paste0(output, "/temp")
  temp_raw <- paste0(temp, "/raw")
  temp_org <- paste0(temp, "/org")
  
  ## create temporary output directories
  if(dir.exists(paths = temp) == FALSE) {
    dir.create(path = temp, showWarnings = FALSE)
  }
  
  if(dir.exists(paths = temp_raw) == FALSE) {
    dir.create(path = temp_raw, showWarnings = FALSE)
  }
  
  if(dir.exists(paths = temp_org) == FALSE) {
    dir.create(path = temp_org, showWarnings = FALSE)
  }
  
  ## make list of Cube file extensions
  id_cube_input <- list.files(path = input, recursive = TRUE)
  id_cube_input <- do.call(rbind, strsplit(id_cube_input,
                                           split = ".", 
                                           fixed = TRUE))[,2]
  id_cube_input_unique <- unique(x = id_cube_input)
  
  ## remove extensions that are not in station info table
  id_ok <- apply(X = as.matrix(id_cube_input_unique), 
                 MARGIN = 1, 
                 FUN = function(x, station) {
                   
                   x %in% station$logger_ID
                 }, station)
  id_cube_input_unique <- id_cube_input_unique[id_ok]
  
  ## check if files to convert remain
  if(length(id_cube_input_unique) < 1) {
    
    stop("No match of files in input dir and station info table IDs!")
  }
  
  ## convert Cube files in dir-wise mode
  if(mode == "dir-wise") {
    
    ## process all Cube files by ID
    z <- parallel::parLapply(
      cl = cl, X = id_cube_input_unique, fun = function(x, pars) {
        
        ## call gipptools::cube2mseed 
        system(command = paste0(pars$gipptools, 
                                '/bin/cube2mseed', 
                                ifelse(test = pars$verbose == TRUE, 
                                       yes = ' --verbose', 
                                       no = ''),
                                ' --fringe-samples=', toupper(x = pars$fringe),
                                ' --include-pattern="*.', x, '"',
                                ' --output-dir=', pars$temp_raw, '/ ',
                                pars$input, '/'), 
               show.output.on.console = pars$verbose)
        
      }, pars = list(input = input, temp_raw = temp_raw, fringe = fringe, 
                     verbose = verbose, gipptools = gipptools))
  }
  
  ## convert Cube files in file-wise mode
  if(mode == "file-wise") {
    
    ## make list of Cube files
    files_cube <- data.frame(path = list.files(path = input, 
                                               recursive = TRUE,
                                               full.names = TRUE),
                             file = list.files(path = input, 
                                               recursive = TRUE,
                                               full.names = FALSE),
                             extn = id_cube_input)
    
    ## keep only files mentioned in station info table
    id_ok <- apply(X = as.matrix(files_cube$extn), 
                   MARGIN = 1, 
                   FUN = function(x, station) {
                     
                     x %in% station$logger_ID
                   }, station)
    files_cube <- files_cube[id_ok,]
    
    z <- parallel::parLapply(
      cl = cl, X = files_cube$path, fun = function(x, pars) {
        
        system(command = paste0(pars$gipptools, '/bin/cube2mseed', 
                                ifelse(test = pars$verbose == TRUE, 
                                       yes = ' --verbose', 
                                       no = ''),
                                ' --fringe-samples=', toupper(x = pars$fringe),
                                ' --output-dir=', pars$temp_raw, '/ ',
                                x), 
               show.output.on.console = verbose)
        
      }, pars = list(verbose = verbose, 
                     fringe = fringe,
                     temp_raw = temp_raw,
                     gipptools = gipptools))
  }
  
  ## make list of converted files
  files_raw <- list.files(path = temp_raw)
  
  ## organise files in eseis-pattern
  if(pattern == "eseis") {
    
    ## cut daily to hourly files
    z <- parallel::parLapply(cl = cl, X = files_raw, fun = function(x, pars) {
      
      ## call gipptools::cube2mseed 
      system(command = paste0(pars$gipptools, "/bin/mseedcut", 
                              " --file-length=HOUR --force-overwrite",
                              " --output-dir=", pars$temp_org, " ",
                              pars$temp_raw, "/", x), 
             show.output.on.console = pars$verbose)
      
    }, pars = list(gipptools = gipptools, temp_raw = temp_raw, 
                   temp_org = temp_org, verbose = verbose))
    
    ## remove daily files and directory
    invisible(unlink(temp_raw, recursive = TRUE))
    
    ## make list of hourly files
    files_org <- list.files(path = temp_org)
    
    ## adjust meta data and save files in correct structure
    z <- invisible(parallel::parLapply(
      cl = cl, X = files_org, fun = function(x, pars) {
        
        ## read mseed file
        s <- try(eseis::read_mseed(file = paste0(pars$temp_org, "/", x)))
        
        ## check if import was successful
        if(inherits(s, "eseis")) {
          
          ## match station ID
          id_sub <- substr(x = s$meta$station, start = 3, stop = 5)
          s$meta$station <- 
            as.character(pars$station$ID[pars$station$logger_ID == id_sub])
          
          ## optionally change component names
          s$meta$component[s$meta$component == "p0"] <- 
            paste0(pars$component, "Z")
          s$meta$component[s$meta$component == "p1"] <- 
            paste0(pars$component, "N")
          s$meta$component[s$meta$component == "p2"] <-
            paste0(pars$component, "E")
          
          ## build file name
          name_output <- paste(s$meta$station, 
                               format(s$meta$starttime, "%y.%j.%H.%M.%S"),
                               s$meta$component,
                               sep = ".")
          
          ## extract output directory time info
          dir_y <- format(s$meta$starttime, "%Y")
          dir_j <- format(s$meta$starttime, "%Y/%j")
          
          ## create output directories
          if(dir.exists(paste0(pars$output, "/", dir_y)) == FALSE) {
            
            dir.create(paste0(pars$output, "/", dir_y))
          }
          
          if(dir.exists(paste0(pars$output, "/", dir_j)) == FALSE) {
            
            dir.create(paste0(pars$output, "/", dir_j))
          }
          
          ## save file
          if(pars$format == "sac") {
            
            ## write sac file
            eseis::write_sac(data = s, unit = 1,
                             file = paste0(pars$output, "/", dir_j, "/",
                                           name_output, ".SAC"))
          } else if(pars$format == "mseed") {
            
            ## edit and re-save mseed file
            system(command = paste0(pars$gipptools, "/bin/mseed2mseed", 
                                    " --set-station=", s$meta$station,
                                    " --set-channel=", s$meta$component,
                                    " ", paste0(pars$temp_org, "/", x),
                                    " > ", paste0(pars$output, "/", dir_j, 
                                                  "/", name_output)))
          }
        }
      }, pars = list(temp_org = temp_org,
                     output = output,
                     station = station,
                     component = component,
                     format = format,
                     gipptools = gipptools)))
  }
  
  ## remove daily files and directory
  invisible(unlink(temp, recursive = TRUE))
  
  ## stop cluster
  parallel::stopCluster(cl = cl)
}
