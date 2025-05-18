#' Plot dv/v results from noise cross correlation analysis
#'
#' The function uses the output of \code{ncc_stretch()} to show a 
#' compound plot with a relative seismic wave velocity change (dv/v) time 
#' series together with the goodness-of-fit matrix for all tested dvv values.
#' 
#' By default the goodness-of-fit matrix will be scaled to the full range of 
#' tested dv/v values, whereas the dv/v time series will be scaled to the 
#' actual range of dv/v values. By setting \code{ylim}, both plots will be 
#' scaled to that range. 
#' 
#' @param data \code{List} object, spectrogram to be plotted. Must be output
#' of \code{ncc_stretch()} or of equivalent structure.
#' 
#' @param ratio \code{Numeric} value, ratio of the space that is taken by 
#' the time series in favour of the goodness-of-fit matrix plot. Default is 
#' \code{0.5} (50 percent width allocated to dv/v time series and 50 percent
#' allocated to the goodness-of-fit matrix). If set to \code{0}, only the 
#' goodness-of-fit matrix is plotted. If set to \code{1}, only the dv/v time 
#' series is plotted
#' 
#' @param n \code{Numeric} value, optional number of samples in 
#' running window used for smoothing the spectrogram. Only applied if a 
#' number is provided. Smoothing is performed as running mean.
#' 
#' @param points \code{Logical} value, option to add white points to 
#' goodness-of-fit matrix, where maximum fit quality is located. The 
#' transparency of the points expresses the strength of the goodness-of-fit. 
#' Default is \code{TRUE}.
#' 
#' @param keep_par \code{Logical} value, option to omit resetting plot 
#' parameters after function execution. Useful for adding further data to the 
#' plot. Default is \code{FALSE} (parameters are reset to original values).
#' 
#' @param \dots Additional arguments passed to the plot function.
#' 
#' @return Graphic output of a relative seismic wave velocity change estimate
#' 
#' @author Michael Dietze
#' 
#' @seealso \code{\link{ncc_stretch}}, \code{\link{ncc_correlate}}
#' 
#' @keywords eseis
#' 
#' @examples
#' 
#' \dontrun{
#'   
#'   ## calculate correlogram
#'   cc <- ncc_correlate(start = "2017-04-09 00:30:00", 
#'                       stop = "2017-04-09 01:30:00", 
#'                       ID = c("RUEG1", "RUEG2"), 
#'                       dt = 1/10,
#'                       component = c("Z", "Z"), 
#'                       dir = paste0(system.file("extdata", 
#'                                    package = "eseis"), "/"), 
#'                       window = 600, 
#'                       overlap = 0, 
#'                       lag = 20, 
#'                       f = c(0.05, 0.1), 
#'                       sd = 1)
#'    
#'    ## estimate dv/v
#'    dv <- ncc_stretch(data = cc, 
#'                      lag = c(1, 10), 
#'                      range = 0.4)
#'    
#'    ## plot result
#'    plot_dvv(data = dv)
#'    
#'    ## plot only goodness-of-fit matrix
#'    plot_dvv(data = dv, ratio = 0)
#'    
#'    ## plot only dv/v time series
#'    plot_dvv(data = dv, ratio = 1)
#'    
#'    ## plot both again but with different plot ratio
#'    plot_dvv(data = dv, ratio = 0.6)
#'    
#'    ## plot matrix with different colour scale
#'    plot_dvv(data = dv, col = heat.colors(200))
#'    
#'    ## plot with smoothened dv/v curve
#'    plot_dvv(data = dv, n = 3)
#'    
#' }
#'
#' @export plot_dvv

plot_dvv <- function(

  data,
  ratio = 0.5,
  n,
  points = TRUE,
  keep_par = FALSE,
  ...
      
) {
  
  ## check/set dt
  if(missing(n) == TRUE) {
    
    n <- NULL
  }
  
  ## check/set default arguments
  args <- list(...)
  
  if("main" %in% names(args)) {
    
    main <- args$"main"
  } else {
    
    main <- "Relative velocity evolution (dv/v)"
  }
  
  if("xlab" %in% names(args)) {
    
    xlab <- args$"xlab"
  } else {
    
    xlab <- "Time"
  }
  
  if("ylab" %in% names(args)) {
    
    ylab <- args$"ylab"
  } else {
    
    ylab <- "dv/v"
  }
  
  ## check/set plot z-axis label
  if ("zlab" %in% names(args)) {
    
    zlab <- args$zlab
  } else {
    
    zlab <- "Goodness of fit"
  }
  
  if("col" %in% names(args)) {
    
    col <- args$"col"
  } else {
    
    ## assign default colour palette inferno  
    col <- colorspace::sequential_hcl(200, palette = "Inferno")
  }
  
  if("xlim" %in% names(args)) {
    
    xlim <- args$"xlim"
  } else {
    
    xlim <- range(data$DVV$t, na.rm = TRUE)
  }
  
  if("ylim" %in% names(args)) {
    
    ylim <- args$"ylim"
  } else {
    
    ylim <- range(data$DVV$range)
  }
  
  ## set z-limits
  if("zlim" %in% names(args)) {
    
    zlim_gof <- args$zlim
    
    legend_values <- pretty(range(args$zlim, na.rm = TRUE))
    
  } else {
    
    zlim_gof <- range(data$DVV$gof, na.rm = TRUE)
    
    legend_values <- pretty(zlim_gof, na.rm = TRUE)
    
  }
  
  if ("axes" %in% names(args)) {
    axes <- args$axes
  } else {
    
    axes <- TRUE
  }
  
  if ("type" %in% names(args)) {
    type <- args$type
  } else {
    
    type <- "l"
  }
  
  ## handle date formats
  if ("format" %in% names(args)) {
    format <- args$format
  } else {
    
    format <- ""
  }
  
  ## remove predefined plot arguments
  keywords <- c("main", "xlab", "ylab", "zlab", 
                "col", "zlim", "format", "axes", "type")
  args <- args[!names(args)%in%keywords]
  
  ## check input data
  if(class(data)[1] != "eseis") {
    
    if(class(data)[1] != "list" | 
       length(data) != 3 | 
       class(data[[1]])[1] != "POSIXct" |
       class(data[[2]])[1] != "numeric" | 
       class(data[[3]])[1] != "numeric" |
       class(data[[4]])[1] != "matrix") {
      
      stop("Input data is not appropriate!")
    }
  }
  
  ## replace values beyond limits by limit values
  data$DVV$gof[data$DVV$gof < zlim_gof[1]] <- zlim_gof[1]
  data$DVV$gof[data$DVV$gof > zlim_gof[2]] <- zlim_gof[2]
  
  ## optionally perform smoothing
  if(is.null(n) == FALSE) {
    
    data$DVV$dvv <- caTools::runmean(x = data$DVV$dvv, k = n)
  }
  
  ## get old plot margins
  mai_in <- graphics::par()$mai
  
  ## get maximum number of characters
  legend_nchar <- max(nchar(legend_values))
  
  ## estimate maximum tick value width
  legend_width <- graphics::par()$cin[1] * legend_nchar
  
  ## get line height
  line_height <- graphics::par()$lheight * graphics::par()$cin[2]
  
  ## calculate space needed for legend
  legend_space <- 4 * line_height + legend_width
  
  ## assign new plot margins
  mai_new <- mai_in
  
  ## adjust plot margins
  mai_new[4] <- legend_space
  graphics::par(mai = mai_new)
  
  ## create plot
  if(ratio == 0) {
    
    ## plot image map of PSD
    do.call(what = graphics::image, 
            args = c(list(x = data$DVV$t, 
                          y = data$DVV$range, 
                          z = t(data$DVV$gof), 
                          col = col,
                          axes = FALSE,
                          main = main,
                          xlab = xlab,
                          ylab = ylab,
                          zlim = zlim_gof), 
                     args))
    
    ## optionally add points 
    if(points == TRUE) {
      
      ## extract maximum goodness-of-fit value
      cls_pts1 <- apply(X = data$DVV$gof, MARGIN = 2, FUN = function(x) {
        
        grDevices::adjustcolor(col = "white", alpha.f = max(x, na.rm = TRUE))
      })
      
      cls_pts2 <- apply(X = data$DVV$gof, MARGIN = 2, FUN = function(x) {
        
        grDevices::adjustcolor(col = "black", alpha.f = max(x, na.rm = TRUE))
      })
      
      ## add points to image plot
      points(x = data$DVV$t, y = data$DVV$dvv, col = cls_pts1, pch = 20)
      points(x = data$DVV$t, y = data$DVV$dvv, col = cls_pts2, pch = 1)
      
    }
    
    ## add box
    box(which = "plot")
    
    ## optionally add axes
    if(axes == TRUE) {
      
      graphics::axis.POSIXct(side = 1, 
                             x = data$DVV$t, 
                             format = format)
      
      axis(side = 2)
    }
    
    ## allow overplotting
    xpd_in <- graphics::par()$xpd
    graphics::par(xpd = TRUE)
    
    ## add empty dummy plot for legend placement
    par(new = TRUE)
    image(x = data$DVV$t, 
          y = data$DVV$range, 
          z = t(data$DVV$gof), 
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
    y_ticks <- approx(x = range(zlim_gof), 
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
    graphics::mtext(side = 4, line = 5, text = zlab, cex = par()$cex * 1)
    
    ## restore overplotting option
    graphics::par(xpd = xpd_in)
    
  } else if(ratio == 1) {
    
    ## plot time series
    do.call(what = graphics::plot, 
            args = c(list(x = data$DVV$t, 
                          y = data$DVV$dvv, 
                          main = main,
                          xlab = xlab,
                          ylab = ylab,
                          type = type), 
                     args))
    
  } else {
    
    ## save and set plot margins
    mar_old <- par()$mar
    mar_a <- mar_old
    mar_a[1] <- 0.5
    mar_b <- mar_old
    mar_b[3] <- 0.5
    
    layout(mat = cbind(c(rep(1, round(ratio * 100)),
                         rep(2, round(100 - ratio * 100)))))
    par(mar = mar_a, xaxs = "i")

    ## plot image map of PSD
    do.call(what = graphics::image, 
            args = c(list(x = data$DVV$t, 
                          y = data$DVV$range, 
                          z = t(data$DVV$gof), 
                          col = col,
                          axes = FALSE,
                          main = main,
                          xlab = xlab,
                          ylab = ylab,
                          zlim = zlim_gof), 
                     args))
    
    ## optionally add points 
    if(points == TRUE) {
      
      ## extract maximum goodness-of-fit value
      cls_pts1 <- apply(X = data$DVV$gof, MARGIN = 2, FUN = function(x) {
        
        adjustcolor(col = "white", alpha.f = max(x, na.rm = TRUE))
      })
      cls_pts2 <- apply(X = data$DVV$gof, MARGIN = 2, FUN = function(x) {
        
        adjustcolor(col = "black", alpha.f = max(x, na.rm = TRUE))
      })
      
      ## add points to image plot
      points(x = data$DVV$t, y = data$DVV$dvv, col = cls_pts1, pch = 20)
      points(x = data$DVV$t, y = data$DVV$dvv, col = cls_pts2, pch = 1)
    }
    
    ## add box
    box(which = "plot")
    
    ## optionally add axes
    if(axes == TRUE) {axis(side = 2)}
    
    ## allow overplotting
    xpd_in <- graphics::par()$xpd
    graphics::par(xpd = TRUE)
    
    ## add empty dummy plot for legend placement
    par(new = TRUE)
    image(x = data$DVV$t, 
          y = data$DVV$range, 
          z = t(data$DVV$gof), 
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
    y_ticks <- approx(x = range(zlim_gof), 
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
    graphics::mtext(side = 4, line = 5, text = zlab, cex = par()$cex * 1)
    
    ## restore overplotting option
    graphics::par(xpd = xpd_in)
    
    par(mar = mar_b)
    
    ## plot time series
    do.call(what = graphics::plot, 
            args = c(list(x = data$DVV$t, 
                          y = data$DVV$dvv, 
                          axes = FALSE, 
                          ann = FALSE,
                          type = type), 
                     args))
    
    ## optionally add axes
    if(axes == TRUE) {
      
      graphics::axis.POSIXct(side = 1, 
                             x = data$DVV$t, 
                             format = format)
      mtext(text = xlab, side = 1, line = 3, cex = par()$cex * 1)
      graphics::axis(side = 2)
      mtext(text = ylab, side = 2, line = 3, cex = par()$cex * 1)
    }
    
    ## add box
    box(which = "plot")
  }
  
  ## restore plot layout
  layout(mat = cbind(1))
  
  ## optionally restore initial plot parameters
  if(keep_par == FALSE) {
    
    graphics::par(mai = mai_in)
  }
}