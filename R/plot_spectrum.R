#' Plot a spectrum of a seismic signal
#' 
#' This function plots a line graph of the spectrum of a seismic signal.
#' 
#' @param data \code{eseis} object or \code{data frame} with two elements,
#' \code{frequency} vector and \code{spectrum} vector.
#' 
#' @param unit \code{Character} value. One out of \code{"linear"}, 
#' \code{"log"}, \code{"dB"}. Default is \code{"dB"}.
#' 
#' @param n \code{Numeric} value, number of values to which the dataset 
#' is reduced. Default is \code{10000}.
#' 
#' @param \dots Further arguments passed to the plot function.
#' 
#' @return A line plot.
#' 
#' @author Michael Dietze
#' 
#' @keywords eseis
#' 
#' @seealso \code{\link{signal_spectrum}}
#' 
#' @examples
#' 
#' ## load example data set
#' data(rockfall)
#' 
#' ## calculate spectrum
#' spectrum_rockfall <- signal_spectrum(data = rockfall_eseis)
#' 
#' ## plot data set with lower resolution
#' plot_spectrum(data = spectrum_rockfall)
#' 
#' @export plot_spectrum
#' 
plot_spectrum <- function(
  
  data,
  unit = "dB", 
  n = 10000,
  ...
) {
  
  ## check data structure
  if(class(data)[1] == "list") {
    
    ## apply function to list
    data_out <- lapply(X = data, 
                       FUN = eseis::plot_spectrum,
                       unit = unit,
                       n = n)
    
  } else {
    
    ## get start time
    eseis_t_0 <- Sys.time()
    
    ## collect function arguments
    eseis_arguments <- list(data = "")
    
    ## check if input object is of class eseis
    if(class(data)[1] == "eseis") {
      
      ## set eseis flag
      eseis_class <- TRUE
      
      ## store initial object
      eseis_data <- data
      
      ## extract signal vector
      data <- eseis_data$spectrum
      
    } else {
      
      ## set eseis flag
      eseis_class <- FALSE
      
      ## check if appropriate data is present
      if(class(data)[1] != "data.frame") {
        
        stop("Spectrum data is no data frame!")
      }
    }
    
    ## extract additional plot arguments
    args <- list(...)
    
    ## check/set plot arguments
    if ("type" %in% names(args)) {
      type <- args$type
    }
    else {
      
      type = "l"
    }
    
    if ("xlab" %in% names(args)) {
      xlab <- args$xlab
    }
    else {
      
      xlab = "Frequency (Hz)"
    }
    
    if ("ylab" %in% names(args)) {
      ylab <- args$ylab
    }
    else {
      
      if(unit == "dB") {
        
        ylab <- expression(paste("Power (10 ", log[10], " (", m^2, "/", s^2, 
                                 ") /", " Hz)", sep = ""))
        
        
      } else if(unit == "linear") {
        
        ylab <- expression(paste("Power (", m^2, "/", s^2, 
                                 ") /", " Hz)", sep = ""))
      } else if(unit == "log") {
        
        ylab <- expression(paste("Power (log(", m^2, "/", s^2, 
                                 ") /", " Hz)", sep = ""))
      } else {
        
        ylab <- ""
      }
    }
    
    ## remove keywords
    keywords <- c("type", "xlab", "ylab")
    
    args <- args[!names(args)%in%keywords]
    
    ## convert data to specified units
    if(unit == "dB") {
      
      data$spectrum <- 10 * log10(data$spectrum)
    } else if(unit == "log") {
      
      data$spectrum <- log(data$spectrum)
    }
    
    ## account for data sets smaller than n
    if(length(data$spectrum) < n) {
      
      n <- length(data$spectrum)
    }
    
    ## get NA values to pad data set
    n_pad <- ceiling(x = length(data$spectrum) / n) * n - length(data$spectrum)
    
    ## pad data set
    s_pad <- c(data$spectrum, rep(NA, 
                                   times = n_pad))
    
    f_pad <- c(data$frequency, rep(NA, 
                                    times = n_pad))
    
    ## convert signal vector to matrix
    S <- matrix(data = s_pad, 
                ncol = n)
    
    ## calculate columnwise min and max
    S_min <- matrixStats::colMins(x = S, 
                                  na.rm = TRUE)
    
    S_max <- matrixStats::colMaxs(x = S, 
                                  na.rm = TRUE)
    
    ## convert min and max alternatingly to vector
    s_plot <- as.numeric(rbind(S_min, S_max))
    
    ## get time vector subset
    i <- seq(from = 1, 
             to = length(f_pad), 
             by = nrow(S))
    
    ## make pairs of time vector subsets
    f_plot <- rep(f_pad[i], each = 2)
    
    ## account for log scale x axis
    if ("log" %in% names(args)) {
      
      ## check/correct that no y-axis log is used
      if(args$log == "xy") {
        
        warning("Y axis log scale not supported, use other unit instead.")
        
        args$log <- "x"
      }
      
      ## remove zero values
      if(args$log == "x") {
        
        f_plot <- f_plot[-(1:2)]
        
        s_plot <- s_plot[-(1:2)]
      }
    }
    
    
    ## generate plot
    do.call(what = graphics::plot, 
            args = c(list(f_plot, 
                          s_plot, 
                          type = type, 
                          xlab = xlab,
                          ylab = ""), 
                     args))
    
    ## add y axis label
    graphics::mtext(text = ylab, side = 2, line = 2.75)
  }
}