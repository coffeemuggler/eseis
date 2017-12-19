#' Plot a probabilistic power spectral density estimate (PPSD)
#' 
#' The function uses the output of \code{signal_spectrogram()} to plot a 
#' probabilistic power spectral density estimate. 
#' 
#' @param data \code{List} object, spectrogram to be plotted. Must be output
#' of \code{signal_spectrogram()} or of equivalent structure.
#' 
#' @param res \code{Integer} vector of length two, factors of image 
#' resolution in pixels, i.e. in time and frequency dimension.  
#' Default is \code{c(100, 100)}.
#' 
#' @param n \code{Integer} vector of length two, factors by which the image 
#' will be smoothend by a running average. \code{n} sets the filter window 
#' size, in x and y direction, respectively. By default, the window sizes 
#' are set to one percent of the input data set dimension.
#' 
#' @param \dots Additional arguments passed to the plot function.
#' 
#' @return Graphic output of a spectrogram.
#' @author Michael Dietze
#' @seealso \code{\link{signal_spectrogram}}
#' @keywords eseis
#' @examples
#' 
#' ## load example data set
#' data(rockfall)
#' 
#' ## deconvolve data set
#' r <- signal_deconvolve(data = rockfall_eseis)
#' 
#' ## calculate PSD
#' p <- signal_spectrogram(data = r)
#' 
#' ## plot PPSD
#' plot_ppsd(data = p$PSD)
#' 
#' ## plot PPSD with lower resolution, more smoothing and other colour
#' ppsd_color <- colorRampPalette(c("white", "black", "red"))
#' 
#' plot_ppsd(data = p$PSD, 
#'           res = c(200, 200), 
#'           n = c(15, 20), 
#'           col = ppsd_color(200))
#'
#' @export plot_ppsd
plot_ppsd <- function(
  data,
  res = c(500, 500),
  n,
  ...
) {
  
  ## extract data elements
  S <- t(data$S)
  f <- seq(from = data$f[1], 
           to = data$f[length(data$f)], 
           length.out = res[1])
  
  ## check/set averaging window size
  if(missing(n) == TRUE) {
    
    n <- round(x = res / 100, 
               digits = 0)
    
    n[n < 1] <- 1
  }
  
  if(length(n) == 1) {
    
    n <- c(n, n)
  }
  
  ## evaluate data set range
  data_range <- range(S,
                      na.rm = TRUE)
  
  ## generate y-vector
  y <- seq(from = data_range[1],
           to = data_range[2],
           length.out = res[2])
  
  ## evaluate number of values below each y threshold
  z <- lapply(X = y, FUN = function(y, S, res) {
    
    z <- colSums(x = S < y)
    
    stats::approx(x = 1:length(z),
                  y = z,
                  xout = seq(from = 1,
                             to = length(z),
                             length.out = res[1]))$y
  }, S, res)
  
  ## convert list to matrix
  z <- do.call(what = rbind,
               args = z)
  
  ## evaluate differences between summed values and transpose matrix
  z <- t(apply(X = z,
               MARGIN = 2,
               FUN = diff))
  
  ## average PPSD
  z <- (apply(X = t(z), 
             MARGIN = 1, 
             FUN = caTools::runmean, 
             k = n[2], 
             alg = "fast", 
             endrule = "mean"))
  z <- t(apply(X = t(z),
             MARGIN = 2,
             FUN = caTools::runmean,
             k = n[1],
             alg = "fast",
             endrule = "mean"))
  
  ## check/set default arguments
  args <- list(...)
  
  if("main" %in% names(args)) {
    
    main <- args$"main"
  } else {
    
    main <- "PPSD"
  }
  
  if("xlab" %in% names(args)) {
    
    xlab <- args$"xlab"
  } else {
    
    xlab <- "f (Hz)"
  }
  
  if("ylab" %in% names(args)) {
    
    ylab <- args$"ylab"
  } else {
    
    ylab <- expression(paste("10 ", log[10], " (", m^2, "/", s^2, 
                             ") /", " Hz", sep = ""))
  }
  
  if("col" %in% names(args)) {
    
    col <- args$"col"
  } else {
    
    col_pal <- colorRampPalette(colors = c("white",
                                           "black"))
    
    col <- col_pal(200)
  }

  ## remove predefined plot arguments
  keywords <- c("x", "y", "z", "ylab", "xlab", "col")
  args <- args[!names(args)%in%keywords]
  
  ## plot data set
  do.call(what = image,
          args = c(list(x = f,
                        y = y,
                        z = z,
                        main = main, 
                        xlab = xlab,
                        ylab = "",
                        col = col)))
  
  ## add y axis label
  mtext(side = 2, 
        line = 2.5, 
        text = ylab)
  
}


