#' Plot a correlogram from noise cross correlation analysis
#' 
#' The function uses the output of \code{ncc_correlate()} to show an  
#' image plot of a noise cross correlation analysis.
#' 
#' @param data \code{List} object, spectrogram to be plotted. Must be output
#' of \code{ncc_correlate()} or of equivalent structure.
#' 
#' @param agg \code{Integer} vector of length two, factors of image 
#' aggregation, i.e. in time and lag dimension. Useful to decrease 
#' image size. Default is \code{c(1, 1)} (no aggregation).
#' 
#' @param legend \code{Logical} value, option to add colour bar legend. Legend
#' label can be changed by \code{zlab}.
#' 
#' @param keep_par \code{Logical} value, option to omit resetting plot 
#' parameters after function execution. Useful for adding further data to the 
#' plot. Default is \code{FALSE} (parameters are reset to original values).
#' 
#' @param \dots Additional arguments passed to the plot function.
#' 
#' @return Graphic output of a correlogram.
#' 
#' @author Michael Dietze
#' 
#' @seealso \code{\link{ncc_correlate}}
#' 
#' @keywords eseis
#' 
#' @examples
#' 
#' \dontrun{
#'   
#'   ## calculate correlogram
#'   cc <- ncc_correlate(start = "2017-04-09 00:30:00", 
#'                        stop = "2017-04-09 01:30:00", 
#'                        ID = c("RUEG1", "RUEG2"), 
#'                        component = c("Z", "Z"), 
#'                        dir = paste0(system.file("extdata", 
#'                                     package = "eseis"), "/"), 
#'                        window = 600, 
#'                        overlap = 0, 
#'                        lag = 20, 
#'                        f = c(0.05, 0.1), 
#'                        sd = 1)
#'                        
#'    ## explicit plot function call with adjusted resolution
#'    plot_correlogram(data = cc, agg = c(2, 5))
#'    
#'    ## define plot colour scale
#'    cls <- colorRampPalette(colors = c("brown", "white", "green"))
#'    
#'    ## simple function call with user-defined colour scale
#'    plot(cc, col = cls(100))
#' }
#'
#' @export plot_correlogram

plot_correlogram <- function(
    data,
    agg = c(1, 1),
    legend = TRUE,
    keep_par = FALSE,
    ...
) {
  
  ## check/set default arguments
  args <- list(...)
  
  if("main" %in% names(args)) {
    
    main <- args$"main"
  } else {
    
    main <- "Correlogram"
  }
  
  if("xlab" %in% names(args)) {
    
    xlab <- args$"xlab"
  } else {
    
    xlab <- "Time"
  }
  
  if("ylab" %in% names(args)) {
    
    ylab <- args$"ylab"
  } else {
    
    ylab <- "Time lag (s)"
  }
  
  ## check/set plot z-axis label
  if ("zlab" %in% names(args)) {
    
    zlab <- args$zlab
  } else {
    
    zlab <- "Correlation coefficient"
  }
  
  if("col" %in% names(args)) {
    
    col <- args$"col"
  } else {
    
    col_pal <- colorRampPalette(colors = c("blue", "grey", "orange"))
    
    col <- col_pal(200)
  }
  
  if("xlim" %in% names(args)) {
    
    xlim <- args$"xlim"
  } else {
    
    xlim <- range(data$CC$t, na.rm = TRUE)
  }
  
  if("ylim" %in% names(args)) {
    
    ylim <- args$"ylim"
  } else {
    
    ylim <- range(data$CC$lag)
  }
  
  ## set z-limits
  if("zlim" %in% names(args)) {
    
    zlim_cc <- args$zlim
    
    legend_values <- pretty(range(args$zlim, na.rm = TRUE))
    
  } else {
    
    zlim_cc <- c(-1, 1)
    
    legend_values <- pretty(zlim_cc, na.rm = TRUE)
    
  }
  
  if ("axes" %in% names(args)) {
    axes <- args$axes
  } else {
    
    axes <- TRUE
  }
  
  ## handle date formats
  if ("format" %in% names(args)) {
    format <- args$format
  } else {
    
    format <- ""
  }
  
  ## remove predefined plot arguments
  keywords <- c("main", "xlab", "ylab", "zlab", 
                "col", "zlim", "format", "axes")
  args <- args[!names(args)%in%keywords]
  
  ## check input data
  if(class(data)[1] != "eseis") {
    
    if(class(data)[1] != "list" | 
       length(data) != 3 | 
       class(data[[1]])[1] != "matrix" |
       class(data[[2]])[1] != "POSIXct" | 
       class(data[[3]])[1] != "numeric") {
      
      stop("Input data is not appropriate!")
    }
  }
  
  data$CC$CC[data$CC$CC < zlim_cc[1]] <- zlim_cc[1]
  data$CC$CC[data$CC$CC > zlim_cc[2]] <- zlim_cc[2]
  
  ## optionally decrease image quality
  t_out <- seq(from = 1, 
               to = length(data$CC$t),
               by = agg[1])
  
  t_plot <- as.POSIXct(x = data$CC$t[t_out], 
                       tz = format(data$CC$t[1], 
                                   format="%Z"))
  
  lag_out <- seq(from = 1, 
                 to = length(data$CC$lag),
                 by = agg[2])
  
  if(legend == FALSE) {
    
    ## plot image map of PSD
    do.call(what = graphics::image, 
            args = c(list(x = data$CC$t[t_out], 
                          y = data$CC$lag[lag_out], 
                          z = t(data$CC$CC[lag_out, t_out]), 
                          col = col,
                          axes = FALSE,
                          main = main,
                          xlab = xlab,
                          ylab = ylab,
                          zlim = zlim_cc), 
                     args))
    
    ## optionally add axes
    if(axes == TRUE) {
      
      graphics::axis.POSIXct(side = 1, 
                             x = data$CC$t[t_out], 
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
            args = c(list(x = data$CC$t[t_out], 
                          y = data$CC$lag[lag_out], 
                          z = t(data$CC$CC[lag_out, t_out]), 
                          axes = FALSE,
                          col = col,
                          main = main,
                          xlab = xlab,
                          ylab = ylab,
                          zlim = zlim_cc), 
                     args))
    
    ## add box
    box(which = "plot")
    
    ## optionally add axes
    if(axes == TRUE) {
      
      graphics::axis.POSIXct(side = 1, 
                             x = data$CC$t[t_out], 
                             format = format)
      
      graphics::axis(side = 2)
    }
    
    ## allow overplotting
    xpd_in <- graphics::par()$xpd
    graphics::par(xpd = TRUE)
    
    ## add empty dummy plot for legend placement
    par(new = TRUE)
    image(x = data$CC$t[t_out], 
          y = data$CC$lag[lag_out], 
          z = t(data$CC$CC[lag_out, t_out]), 
          axes = FALSE, ann = FALSE, col = NA)
    
    ## define coordinates for colour scale bar
    x_0 <- graphics::par()$usr[2] + 0.5 * graphics::par()$cxy[1]
    x_1 <- graphics::par()$usr[2] + 1.5 * graphics::par()$cxy[1]
    y_0 <- graphics::par()$usr[3]
    y_1 <- graphics::par()$usr[4]
    
    ## define colour scale bar increment
    d_y <- (y_1 - y_0) / length(col)
    
    ## define colour scale bar polygons
    polygons <- matrix(nrow = length(col), ncol = 8)
    polygons <- cbind(rep(x = x_0, times = length(col)),
                      rep(x = x_0, times = length(col)),
                      rep(x = x_1, times = length(col)),
                      rep(x = x_1, times = length(col)),
                      seq(from = y_0, to = y_1 - d_y, by = d_y),
                      seq(from = y_0 + d_y, to = y_1, by = d_y),
                      seq(from = y_0 + d_y, to = y_1, by = d_y),
                      seq(from = y_0, to = y_1 - d_y, by = d_y))
    
    ## convert to y-scale
    y_ticks <- approx(x = range(zlim_cc), 
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