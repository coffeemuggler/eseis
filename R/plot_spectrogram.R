#' Plot spectrograms (power spectral density estimates)
#' 
#' This function plots spectrograms of seismic signals. It uses the output 
#' of \code{signal_spectrogram}.
#' 
#' As of version 0.7.2, the value range (\code{zlim}) is no longer set to the 
#' full data range but to the range between quantiles 0.01 and 0.99. For the 
#' full value range to be plotted, use \code{zlim = range(data$PSD$S)}.
#' 
#' As of version 0.7.2, the default plot colour has changed from the "jet" 
#' colour palette to the "Inferno" palette. This due to perception issues with 
#' the "jet" palette. If one wants to decisively use the "jet" colours, this 
#' can be done by adding the keyword \code{col = "jet"}. To use other 
#' colour schemes, such as sequential HCL schemes from the 
#' colorspace package, specify them as additional argument, e.g. 
#' \code{col = colorspace::sequential_hcl(200, palette = "Plasma")},
#' \code{col = colorspace::sequential_hcl(200, palette = "Inferno")},
#' \code{col = colorspace::sequential_hcl(200, palette = "Viridis")}. 
#' 
#' @param data \code{List} object, spectrogram to be plotted. Must be output
#' of \code{signal_spectrogram} or of equivalent structure.
#' 
#' @param legend \code{Logical} value, option to add colour bar legend. Legend
#' label can be changed by \code{zlab}.
#' 
#' @param keep_par \code{Logical} value, option to omit resetting plot 
#' parameters after function execution. Useful for adding further data to the 
#' PSD plot. Default is \code{FALSE} (parameters are reset to original values).
#' 
#' @param agg \code{Integer} vector of length two, factors of image 
#' aggregation, i.e. in time and frequency dimension. Useful to decrease 
#' image size. Default is \code{c(1, 1)} (no aggregation).
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
#' ## deconvolve signal
#' rockfall <- signal_deconvolve(data = rockfall_eseis)
#' 
#' ## calculate spectrogram
#' PSD <- signal_spectrogram(data = rockfall)
#' 
#' ## plot spectrogram
#' plot_spectrogram(data = PSD)
#' 
#' ## plot spectrogram with legend and labels in rainbow colours
#' plot_spectrogram(data = PSD, 
#'                  xlab = "Time (min)", 
#'                  ylab = "f (Hz)", 
#'                  main = "Power spectral density estimate", 
#'                  legend = TRUE, 
#'                  zlim = c(-220, -70),
#'                  col = rainbow(100)) 
#'                  
#' ## plot spectrogram with frequencies in log scale
#' plot_spectrogram(data = PSD, log = "y")
#' 
#' ## plot spectrogram with formatted time axis (minutes and seconds)
#' plot_spectrogram(data = PSD, format = "%M:%S")
#'                      
#' @export plot_spectrogram
plot_spectrogram <- function(
  data,
  legend = TRUE,
  keep_par = FALSE,
  agg = c(1, 1),
  ...
) {
  
  ## check for eseis object
  if(class(data)[1] == "eseis") {
    
    data <- data$PSD
  }
  
  ## read additional plot arguments
  extraArgs <- list(...)
  
  ## check/set plot title
  if ("main" %in% names(extraArgs)) {
    
    main <- extraArgs$main
  }
  else {
    
    main <- paste("PSD")
  }
  
  ## check/set plot x-axis label
  if ("xlab" %in% names(extraArgs)) {
    
    xlab <- extraArgs$xlab
  }
  else {
    xlab <- "Time"
  }
  
  ## check/set plot y-axis label
  if ("ylab" %in% names(extraArgs)) {
    
    ylab <- extraArgs$ylab
  }
  else {
    ylab <- "Frequency (Hz)"
  }
  
  ## check/set plot z-axis label
  if ("zlab" %in% names(extraArgs)) {
    
    zlab <- extraArgs$zlab
  }
  else {
    
    zlab <- expression(paste("10 ", log[10], " (", m^2, "/", s^2, 
                             ") /", " Hz", sep = ""))
  }
  
  ## set z-limits
  if("zlim" %in% names(extraArgs)) {
    
    zlim_psd <- extraArgs$zlim
    
    legend_values <- pretty(range(extraArgs$zlim, na.rm = TRUE))
    
  } else {
    
    zlim_psd <- quantile(x = data$S, probs = c(0.01, 0.99), na.rm = TRUE)
    
    legend_values <- pretty(zlim_psd, na.rm = TRUE)
    
  }
  
  if ("axes" %in% names(extraArgs)) {
    axes <- extraArgs$axes
  }
  else {
    
    axes <- TRUE
  }
  
  ## handle date formats
  if ("format" %in% names(extraArgs)) {
    format <- extraArgs$format
  }
  else {
    
    format <- ""
  }
  
  ## handle colour scheme
  if("col" %in% names(extraArgs)) {
    
    col <- extraArgs$col

    ## handle keyword case "jet"
    if(col[1] == "jet") {
      
      col <- colorRampPalette(colors = c("darkblue",
                                         "blue",
                                         "cyan",
                                         "lightgreen",
                                         "yellow",
                                         "red",
                                         "brown",
                                         "grey30"))
                                                   
      col <- col(200)
    }
    
  }
  else {
  
    ## assign default colour palette inferno  
    col <- colorspace::sequential_hcl(200, palette = "Inferno")
    
  }
  
  ## remove keywords from plot arguments
  keywords <- c("main", 
                "xlab", 
                "ylab", 
                "zlab",
                "zlim", 
                "format", 
                "axes",
                "col")
  
  extraArgs <- extraArgs[!names(extraArgs)%in%keywords]
  
  ## check input data
  if(class(data)[1] != "spectrogram") {
    
    if(class(data)[1] != "list" | 
       length(data) != 3 | 
       class(data[[1]])[1] != "matrix" |
       class(data[[2]])[1] != "POSIXct" | 
       class(data[[3]])[1] != "numeric") {
      
      stop("Input data is not appropriate!")
    }
  }
  
  data$S[data$S < zlim_psd[1]] <- zlim_psd[1]
  data$S[data$S > zlim_psd[2]] <- zlim_psd[2]
  
  ## optionally decrease image quality
  t_out <- seq(from = 1, 
               to = length(data$t),
               by = agg[1])
  
  t_plot <- as.POSIXct(x = data$t[t_out], 
                       tz = format(data$t[1], 
                                   format="%Z"))
  
  f_out <- seq(from = 1, 
               to = length(data$f),
               by = agg[2])
  
  ## optionally remove zero value for log y-axis
  if("log" %in% names(extraArgs)) {
    
    if(extraArgs$log == "y") {
      
      f_out <- f_out[-1]
    }
  }
  
  if(legend == FALSE) {
    
    ## plot image map of PSD
    do.call(what = graphics::image, 
            args = c(list(x = data$t[t_out], 
                          y = data$f[f_out], 
                          z = t(data$S[f_out, t_out]), 
                          col = col,
                          axes = FALSE,
                          main = main,
                          xlab = xlab,
                          ylab = ylab,
                          zlim = zlim_psd), 
                     extraArgs))
    
    ## optionally add axes
    if(axes == TRUE) {
      
      graphics::axis.POSIXct(side = 1, 
                             x = data$t[t_out], 
                             format = format)
      
      graphics::axis(side = 2)
    }
    
    ## add box
    box(which = "plot")
    
  } else {
    
    ## get maximum number of characters
    legend_nchar <- max(nchar(legend_values))
    
    ## estimate maximum tick value width
    legend_width <- graphics::par()$cin[1] * legend_nchar
    
    ## get line height
    line_height <- graphics::par()$lheight * graphics::par()$cin[2]
    
    ## calculate space needed for legend
    legend_space <- 4 * line_height + legend_width
    
    ## get old plot margins
    mai_in <- graphics::par()$mai
    mai_new <- mai_in
    
    ## adjust plot margins
    mai_new[4] <- legend_space
    graphics::par(mai = mai_new)

    ## plot image map of PSD
    do.call(what = graphics::image, 
            args = c(list(x = data$t[t_out], 
                          y = data$f[f_out], 
                          z = t(data$S[f_out, t_out]), 
                          axes = FALSE,
                          col = col,
                          main = main,
                          xlab = xlab,
                          ylab = ylab,
                          zlim = zlim_psd), 
                     extraArgs))
    
    ## add box
    box(which = "plot")
    
    ## optionally add axes
    if(axes == TRUE) {
      
      graphics::axis.POSIXct(side = 1, 
                             x = data$t[t_out], 
                             format = format)
      
      graphics::axis(side = 2)
    }
    
    ## allow overplotting
    xpd_in <- graphics::par()$xpd
    graphics::par(xpd = TRUE)
    
    ## add empty dummy plot for legend placement
    par(new = TRUE)
    image(x = data$t[t_out], 
          y = data$f[f_out], 
          z = t(data$S[f_out, t_out]), 
          axes = FALSE, ann = FALSE, col = NA)
    
    ## define coordinates for colour scale bar
    x_0 <- graphics::par()$usr[2] + 0.5 * graphics::par()$cxy[1]
    x_1 <- graphics::par()$usr[2] + 1.5 * graphics::par()$cxy[1]
    y_0 <- graphics::par()$usr[3]
    y_1 <- graphics::par()$usr[4]
    
    ## define colour scale bar increment
    d_y <- (y_1 - y_0) / 200
    
    ## define colour scale bar polygons
    polygons <- matrix(nrow = 200, ncol = 8)
    polygons <- cbind(rep(x = x_0, times = 200),
                      rep(x = x_0, times = 200),
                      rep(x = x_1, times = 200),
                      rep(x = x_1, times = 200),
                      seq(from = y_0, to = y_1 - d_y, by = d_y),
                      seq(from = y_0 + d_y, to = y_1, by = d_y),
                      seq(from = y_0 + d_y, to = y_1, by = d_y),
                      seq(from = y_0, to = y_1 - d_y, by = d_y))
    
    ## convert to y-scale
    y_ticks <- approx(x = range(zlim_psd), 
                      y = c(y_0, y_1), 
                      xout = legend_values)$y
    
    ## draw legend bar
    for(i in 1:nrow(polygons)) {
      graphics::polygon(x = polygons[i,1:4], 
                        y = polygons[i,5:8], 
                        border = NA, 
                        col = col[i])
    }
    
    ## draw polygon around colour scale bar
    graphics::polygon(x = c(x_0, x_0, x_1, x_1),
                      y = c(y_0, y_1, y_1, y_0))
    
    ## draw z-axis
    graphics::lines(x = c(x_1, x_1), 
                    y = c(y_0, y_1))
    
    ## draw z-axis ticks and labels
    for(i in 1:length(legend_values)) {
      
      graphics::lines(x = c(x_1,
                            x_1 + 0.7 * graphics::par()$cxy[1]), 
                      y = rep(y_ticks[i], 2))
      
      graphics::text(x = x_1 + 1.5 * graphics::par()$cxy[1], 
                     y = y_ticks[i], 
                     adj = c(0, 0.5), 
                     labels = legend_values[i])
    }
    
    ## add z-axis label
    graphics::mtext(side = 4, line = 5, text = zlab)

    ## restore overplotting option
    graphics::par(xpd = xpd_in)
    
    ## optionally restore initial plot parameters
    if(keep_par == FALSE) {
      
      graphics::par(mai = mai_in)
    }
  }
  
}