#' Calculate spectrograms (power spectral density estimates) from time series.
#' 
#' This function creates spectrograms from seismic signals. It supports the
#' standard spectrogram apporach, multitaper, and the Welch method.
#' 
#' @param data \code{List} object, spectrogram to be plotted. Must be output
#' of \code{signal_spectrogram()} or of equivalent structure.
#' 
#' @param legend \code{Character} scalar, label of the legend colour scale.
#' When omitted, no legend is plotted.
#' 
#' @param col \code{Character} scalar, colour palette to use. Default is 
#' \code{"gradient_1"}.
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
#' rockfall <- signal_deconvolve(data = rockfall, dt = 1/200)
#' 
#' ## calculate spectrogram
#' PSD <- signal_spectrogram(data = rockfall, time = t, dt = 1/200)
#' 
#' ## plot spectrogram
#' plot_spectrogram(data = PSD)
#' 
#' ## plot spectrogram with legend and labels in rainbow colours
#' plot_spectrogram(data = PSD, 
#'                  xlab = "Time (min)", 
#'                  ylab = "f (Hz)", 
#'                  main = "Power spectral density estimate", 
#'                  legend = "dB", 
#'                  zlim = c(-220, -70),
#'                  col = "rainbow")
#' 
#'                      
#' @export plot_spectrogram
plot_spectrogram <- function(
  data,
  legend,
  col = "gradient_1",
  agg = c(1, 1),
  ...
) {
  
  ## define colours
  if(col == "gradient_1") {
    
    col <- colorRampPalette(colors = c("darkblue", 
                                       "blue", 
                                       "cyan", 
                                       "lightgreen", 
                                       "yellow", 
                                       "red", 
                                       "brown", 
                                       "grey30"))
  }
  
  ## check input data
  if(class(data) != "spectrogram") {
    
    if(class(data) != "list" | 
       length(data) != 3 | 
       class(data[[1]]) != "matrix" |
       class(data[[2]])[1] != "POSIXct" | 
       class(data[[3]]) != "numeric") {
      
      stop("Input data is not appropriate!")
    }
  }
  
  ## set z-limits
  if("zlim" %in% names(list(...))) {
    
    zlim_psd <- list(...)$zlim
  } else {
    
    zlim_psd <- range(data$S, na.rm = TRUE)
  }
  
  data$S[data$S < zlim_psd[1]] <- zlim_psd[1]
  data$S[data$S > zlim_psd[2]] <- zlim_psd[2]
  
  ## optionally decrease image quality
  t_out <- seq(from = 1, 
               to = length(data$t),
               by = agg[1])
  
  f_out <- seq(from = 1, 
               to = length(data$f),
               by = agg[2])
  
  if(missing(legend) == TRUE) {
    
    ## plot image map of PSD
    image(x = data$t[t_out], 
          y = data$f[f_out], 
          z = t(data$S[f_out, t_out]), 
          col = do.call(what = col, 
                        args = list(200)),
          ...)
  } else {
    
    ## get range of values
    if("zlim" %in% names(list(...))) {
      
      legend_values <- pretty(range(list(...)$zlim, na.rm = TRUE))
    } else {
      
      legend_values <- pretty(data$S, na.rm = TRUE)
    }
    
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
    image(x = data$t[t_out], 
          y = data$f[f_out], 
          z = t(data$S[f_out, t_out]), 
          col = do.call(what = col, 
                        args = list(200)),
          ...)
    
    ## allow overplotting
    xpd_in <- graphics::par()$xpd
    graphics::par(xpd = TRUE)
    
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
    y_ticks <- seq(from = graphics::par()$usr[3], 
                   to = graphics::par()$usr[4], 
                   length.out = length(legend_values))
    
    ## draw legend bar
    for(i in 1:nrow(polygons)) {
      graphics::polygon(x = polygons[i,1:4], 
                        y = polygons[i,5:8], 
                        border = NA, 
                        col = do.call(what = col, 
                                      args = list(200))[i])
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
    graphics::mtext(side = 4, line = 5, text = legend)
    
    ## restore initial plot parameters
    graphics::par(xpd = xpd_in,
                  mai = mai_in)
  }
  
  
}