cube2mseed <-
structure(function(# Function to convert cube-format to mseed-format.
  ### This function converts files in the Omnirecs cube format to the miniseed
  ### format. It uses code provided by Omnirecs (see details).

  file.in,
  ### Character \code{scalar} or \code{vector}, name of the files to be read,
  ### with extension. 
  
  path.out,
  ### Character \code{scalar} or \code{vector}, optional path to where the
  ### output files are written to. If omitted, a new directory called 
  ### \code{"output"} will be created since this is necessary for the
  ### Omnirecs code to work.
  
  file.out,
  ### Character \code{scalar} or \code{vector}, optional names of the output 
  ### files, without extension. If omitted, the input file names are used.
  
  path
  ### Character \code{scalar}, absoulte location of the cube2mseed file 
  ### code, e.g. \code{~/Downloads/CUBEtools/bin/}.

  ){
  
  ## check/set path and file names
  if(missing(path.out) == TRUE) {
    path.out <- "output"
    dir.create(path = path.out, showWarnings = FALSE)
  }
  
  if(missing(file.out) == TRUE) {
    file.out.full <- strsplit(x = file.in, split = "/")
    file.out <- character(length(file.out.full))
    for(i in 1:length(file.out.full)) {
      file.out[i] <- file.out.full[[i]][length(file.out.full[[i]])]
    }
  }
  if(missing(path) == TRUE) {
    path <- ""
  }
  
  ## loop through all input data sets
  for(i in 1:length(file.in)) {
    ## convert files
    system(paste(path,
                 "cube2mseed --time-correction=STARTTIME ", 
                 "--force-conversion --output-dir=", 
                 path.out, 
                 " ", 
                 file.in[i],
                 sep = ""))
    
    ## rename files, 0-component
    if(sum(grepl(pattern = "pri0", x = dir(path = path.out))) > 0) {
      file.rename(from = paste(path.out, 
                               "/",
                               dir(path = path.out)[
                                 grepl(pattern = "pri0", 
                                       x = dir(path = path.out))],
                               sep = ""),
                  to = paste(path.out, 
                             "/",
                             file.out[i], 
                             ".finished0", 
                             sep = ""))
    }
    
    ## rename files, 1-component
    if(sum(grepl(pattern = "pri1", x = dir(path = path.out))) > 0) {
      file.rename(from = paste(path.out, 
                               "/",
                               dir(path = path.out)[
                                 grepl(pattern = "pri1", 
                                       x = dir(path = path.out))],
                               sep = ""),
                  to = paste(path.out, 
                             "/",
                             file.out[i], 
                             ".finished1", 
                             sep = ""))
    }
    
    ## rename files, 2-component
    if(sum(grepl(pattern = "pri2", x = dir(path = path.out))) > 0) {
      file.rename(from = paste(path.out, 
                               "/",
                               dir(path = path.out)[
                                 grepl(pattern = "pri2", 
                                       x = dir(path = path.out))],
                               sep = ""),
                  to = paste(path.out, 
                             "/",
                             file.out[i], 
                             ".finished2", 
                             sep = ""))
    }  
  }
  
  
  files.old <- list.files(path = paste(path.out, "/", sep = ""), 
                          pattern = "finished")
  dot.pos <- c(nchar(files.old[1]) - 9, 
               nchar(files.old[1]),
               nchar(files.old[1]))
  
  files.new <- paste(substr(x = files.old, 
                            start = 1, 
                            stop = dot.pos[1]),
                     "pri",
                     substr(x = files.old, 
                            start = dot.pos[2], 
                            stop = dot.pos[3]),
                     sep = "")
  
  file.rename(from = paste(path.out, 
                           "/",
                           files.old,
                           sep = ""),
              to = paste(path.out, 
                         "/",
                         files.new, 
                         sep = ""))
  
  ### Files in miniseed format are created.
  
  ##details<<
  ## Omnirecs provides Java-based code to convert cube files to the miniseed
  ## format. The software can be downloaded from 
  ## http://www.omnirecs.de/documents.html. For use in this function
  ## Java must be installed and at least the folders \code{bin} and \code{java}
  ## from the Cube2miniSEED (C2M) directory must be present. Alternatively, 
  ## make the cube tools globally available by adding the path to the 
  ## bin-directory. In Ubuntu this can be done by opening the file .bashrq 
  ## in the home directory and adding the line 
  ## \code{export $PATH:/home/CUBEtools/bin}. In Windows XP this can be done
  ## by adding the path to CUBEtools/bin to environmental variables list.
  
  ##seealso<<
}, ex = function(){
  ## uncomment to run
  # cube2mseed(file.in = "cubefile.A1A", 
  #            path.out = "output", 
  #            file.out = "miniseed",
  #            path = "../code/C2M")
})
