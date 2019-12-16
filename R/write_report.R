#' Create a HTML report for (RLum) objects
#'
#' This function creates a HTML report for a given eseis object, listing its 
#' complete processing history. The report serves both as a convenient way of 
#' browsing through objects and as a proper approach to documenting and saving
#' scientific data and workflows.
#' 
#' The function heavily lends ideas from the function \code{report_RLum()} 
#' written by Christoph Burow, which is contained in the package 
#' \code{Luminescence}. This function here is a truncated, tailored version 
#' with minimised availabilities.
#'
#' @param object, \code{eseis} object to be reported on
#' 
#' @param file \code{Character} value, name of the output file (without 
#' extension)
#' 
#' @param title \code{Character} value, title of the report
#' 
#' @param browser \code{Logical} value, optionally open the HTML file in the
#' default web browser after it has been rendered.
#' 
#' @param css \code{Character} value, path to a CSS file to change the 
#' default styling of the HTML document.
#' 
#' @author 
#' Michael Dietze
#' 
#' @return
#' HTML and .Rds file.
#' 
#' @examples
#' 
#' \dontrun{
#' ## load example data set
#' data(rockfall)
#' 
#' ## make report for rockfall object
#' write_report(object = rockfall_eseis, 
#'              browser = TRUE)
#' }
#'
#' @md
#' 
#' @export write_report
write_report <- function(
  object, 
  file,
  title = "eseis report",
  browser = FALSE,
  css
) {
  
  ## INITIAL TESTS AND CHECKS -------------------------------------------------
  
  ## check if submitted object is of class eseis
  if(class(object)[1] != "eseis") {
    
    stop("Submitted object is no eseis object!")
  }
  
  
  ## check/set output file name
  if(missing(file) == TRUE) {
    
    output_dir <- file.path(tempdir(), "output")
    print(paste("Output will be written to", output_dir))
    
    file <- paste(output_dir, "report", sep = "/")
  }
  
  ## check if css file exists
  if(missing(css) == FALSE) {
    
    if(file.exists(css) == FALSE) {
      
      stop(paste("No CSS file present at", css, "!"))
    }
  }
  
  ## set default css options
  css_content <- list(font_family = "arial",
                      headings_size = "166%",
                      content_color = "#a72925")
  
  ## define rmd and html file names
  file.rmd <- paste(file, 
                    ".Rmd", 
                    sep = "")
  
  file.html <- paste(file, 
                     ".html", 
                     sep = "")
  
  ## Create and open the file
  file.create(file.rmd)
  
  tmp <- file(file.rmd, 
              open = "w")
  
  ## HEADER PART --------------------------------------------------------------
  
  ## write Rmd basic header information
  writeLines("---", con = tmp)
  writeLines("output:", con = tmp)
  writeLines("  html_document:", con = tmp)
  writeLines("    mathjax: null", con = tmp)
  writeLines("    title: RLum.Report", con = tmp)
  writeLines(paste("    theme:", "cerulean"), con = tmp)
  writeLines(paste("    highlight:", "haddock"), con = tmp)
  writeLines("    toc: true", con = tmp)
  writeLines("    toc_float: true", con = tmp)
  writeLines("    toc_depth: 6", con = tmp)
  if (missing(css) == FALSE) {
    
    writeLines(paste("    css:", css), con = tmp)
  }
  writeLines("    md_extensions: -autolink_bare_uris", con = tmp)
  writeLines("---", con = tmp)
  writeLines(paste0(
    "<style>",
    paste0("h1, h2, h3, h4, h5, h6 { font-size:", 
           css_content$headings_size," } \n"),
    paste0("#root { color: ", css_content$content_color," } \n"),
    paste0("BODY { font-family:", css_content$font_family, " } \n"),
    "</style>"
  ),
  con = tmp)
  
  
  ## Write report title
  writeLines(paste("<div align='center'><h1>", 
                   title, 
                   "</h1></div>\n\n<hr>"), 
             con = tmp) 
  
  ## write summary information
  writeLines(paste("**Date:**", 
                   Sys.time(),
                   "\n\n",
                   "**Platform:**", 
                   object$history[[1]]$system$platform, 
                   "\n\n",
                   "**OS:**", 
                   object$history[[1]]$system$running, 
                   "\n\n",
                   "**R version:**", 
                   object$history[[1]]$system$R.version$version.string, 
                   "\n\n",
                   "**Locale settings:**\n\n", 
                   paste(strsplit(x = object$history[[1]]$system$locale, 
                                  split = ";", 
                                  fixed = TRUE)[[1]], 
                         collapse = ", "), 
                   "\n\n",
                   "**Packages** \n\n",
                   "**&nbsp;&nbsp;&raquo; Base:** ",
                   paste(object$history[[1]]$system$basePkgs, 
                         collapse = ", "),
                   "\n\n**&nbsp;&nbsp;&raquo; Other:** ",
                   paste(unlist(lapply(object$history[[1]]$system$otherPkgs, 
                                       FUN = function(x) {
                                         paste(x$Package, 
                                               x$Version, 
                                               collapse = "_")
                                       })), 
                         collapse = ", "),
                   "\n\n**&nbsp;&nbsp;&raquo; Namespace:** ",
                   paste(unlist(lapply(object$history[[1]]$system$loadedOnly, 
                                       FUN = function(x) {
                                         paste(x$Package, 
                                               x$Version, 
                                               collapse = "_")
                                       })), 
                         collapse = ", "),
                   "<hr>"),
             con = tmp)
  
  writeLines(paste0("# ",
                    "<span style='color:#74a9d8'>",
                    "Meta data",
                    "</span>",
                    "\n\n"),
             con = tmp)
  
  ## convert POSIXct to character string
  object$meta$starttime <- format(x = object$meta$starttime, 
                                  format = "%Y-%m-%d %H:%M:%S")
  
  ## write object meta information
writeLines("<pre style='padding:0px;border:0px'>",
             con = tmp)
  
  for(i in 1:length(object$meta)) {
    
    writeLines(paste0("<span style='color:#428bca'>",
                      names(object$meta)[i],
                      ": </span>", 
                      object$meta[i]),
               con = tmp)
  }
  
  writeLines(paste0("</pre>",
                    "\n\n"),
             con = tmp) 
  
  ## append stepwise history content
  for(i in 2:length(object$history)) {
    
    ## write history title, i.e., function call
    writeLines(paste0("# ",
                      "<span style='color:#74a9d8'>",
                      i - 1,
                      ": ",
                      object$history[[i]]$call,
                      "</span>",
                      "\n\n"),
               tmp)
    
    ## write function call start time and duration
    writeLines(paste0("<pre style='padding:0px;border:0px'>",
                      "<span style='color:#428bca'>",
                      " Time | duration: </span>", 
                      object$history[[i]]$time,
                      " | ",
                      round(x = object$history[[i]]$duration, 
                            digits = 3),
                      " s",
                      "</pre>",
                      "\n\n"),
               tmp)
    
    
    writeLines("<pre style='padding:0px;border:0px'>", 
               con = tmp)
    
    ## add function arguments
    for(j in 1:length(object$history[[i]]$arguments)) {
      
      writeLines(paste0("<span style='color:#428bca'>",
                        names(object$history[[i]]$arguments)[j],
                        " = </span>", 
                        ifelse(test = j == 1,
                               yes = "data", 
                               no = object$history[[i]]$arguments[j])),
                 con = tmp)
    }
    
    writeLines(paste0("</pre>",
                      "\n\n"),
               con = tmp) 
    
  }
  
  ## RENDER HTML FILE ---------------------------------------------------------
  
  ## close rmd file
  close(tmp)
  
  ## render html file
  rmarkdown::render(file.rmd, 
                    clean = TRUE,
                    quiet = TRUE)
  
  ## remove rmd file
  try(invisible(unlink(file.rmd, 
                       recursive = TRUE)))
  
  ## optionally open file in browser
  if (browser == TRUE) {
      try(utils::browseURL(file.html))
  }
}

