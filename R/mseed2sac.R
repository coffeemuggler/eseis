mseed2sac <-
structure(function(# Function to convert from miniseed to sac.
  ### This function converts files in miniseed format to the sac format. 
  ### It uses the Obspy library (see details.)
  file.in,
  ### Character \code{scalar} or \code{vector}, name of the files to be read,
  ### with extension. 
  path.out,
  ### Character \code{scalar} or \code{vector}, optional path to where the
  ### output files are written to.
  file.out
  ### Character \code{scalar} or \code{vector}, optional names of the output 
  ### files, with extension. If omitted, the input file names are used and 
  ### extensions \code{BHE}, \code{BHN}, \code{BHZ} will be added.
){
  ## check/set path and file names
  if(missing(path.out) == TRUE) {
    path.out <- ""
  }
  
  if(missing(file.out) == TRUE) {
    ## modify output filename extension
    ext.out <- character(length(file.in))
    ext.out[grepl(x = file.in, pattern = "pri0") == TRUE] <- "BHE"
    ext.out[grepl(x = file.in, pattern = "pri1") == TRUE] <- "BHN"
    ext.out[grepl(x = file.in, pattern = "pri2") == TRUE] <- "BHZ"
    
    ## merge output file names
    file.out <- paste(path.out, file.in, ".", ext.out, sep = "")
  } else {
    file.out <- paste(path.out, file.out, sep = "")
  }
  
  ## strip output file names
  file.out.full <- strsplit(x = file.out, split = "/")
  file.out <- character(length(file.out.full))
  for(i in 1:length(file.out.full)) {
    file.out[i] <- file.out.full[[i]][length(file.out.full[[i]])]
  }
  
  
  ## load python function
  python.exec("from obspy.core import read")

  ## loop through all input data sets
  for(i in 1:length(file.in)) {
    ## read mseed file
    python.exec(paste("st = read('",
                      file.in[i],
                      "')",
                     sep = ""))
        
    ## save file in sac formatH
    python.exec(paste("st.write('",
                      path.out,
                      file.out[i],
                       "', format='SAC')", 
                      sep = ""))
  }
  
  ### Files in sac format are created.
  
  ##details<<
  ## The conversion uses the Obspy library. See 
  ## https://github.com/obspy/obspy/wiki for how to install it
  ## on diverse platforms. 
  
  ##seealso<<
}, ex = function(){
  ## uncomment to run
  # mseed2sac(file.in = "cube1.A7P.pri0")
  
  # mseed2sac(file.in = list.files(path = "../input"))
  
  # mseed2sac(file.in = list.files(pattern = "pri0"))
  
  # mseed2sac(file.in = "cube1.A7P.pri0", 
  #           path.out = "../output/",
  #           file.out = "cube1.BHE")
})
