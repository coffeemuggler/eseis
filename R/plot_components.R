#' Plot three seismic components against each other
#' 
#' The function visualises the time evolution of three seismic components 
#' of the same signal against each other as line graphs. There are three 
#' different visualisation types available: \code{2D} (a panel of three 
#' 2D plots), \code{3D} (a perspective threedimensional plot) and 
#' \code{scene} (an interactive threedimensional plot, mainly for 
#' exploratory purpose).
#' 
#' The plot type \code{type = "3D"} requires the package \code{plot3D} 
#' being installed. The plot type \code{type = "scene"} requires the  
#' package \code{rgl} being installed. 
#' 
#' @param data \code{List}, \code{data frame} or \code{matrix}, seismic
#' componenents to be processed. If \code{data} is a matrix, the components 
#' must be organised as columns. Also, \code{data} can be a list of 
#' \code{eseis} objects.
#' 
#' @param type \code{Character} value, plot type. One out of \code{"2D"} 
#' (panel of three 2-dimensional plots), \code{"3D"} (perspective 3D plot)
#' and \code{"scene"} (interactive 3D plot). Default is \code{"2D"}.
#' 
#' @param order \code{Caracter} value, order of the seismic components. 
#' Describtion must contain the letters \code{"x"},\code{"y"} and
#' \code{"z"} in the order according to the input data set. Default is 
#' \code{"xyz"} (NW-SE-vertical).
#' 
#' @param \dots Further arguments passed to the plot function.
#' 
#' @return A plot
#' 
#' @author Michael Dietze
#' 
#' @keywords eseis
#' 
#' @examples 
#' 
#' ## load example data set
#' data(earthquake)
#' 
#' ## filter seismic signals
#' s <- eseis::signal_filter(data = s, 
#'                           dt = 1/200, 
#'                           f = c(0.05, 0.1))
#' 
#' ## integrate signals to get displacement
#' s_d <- eseis::signal_integrate(data = s, dt = 1/200)
#' 
#' ## plot components in 2D
#' plot_components(data = s_d, 
#'                 type = "2D")
#' 
#' ## plot components with time colour-coded
#' plot_components(data = s_d, 
#'                 type = "2D",
#'                 col = rainbow(n = length(s$BHE)))
#' 
#' ## plot components with used defined coulour ramp
#' col_user <- colorRampPalette(colors = c("grey20", "darkblue", "blue", 
#'                                         "green", "red", "orange"))
#' 
#' plot_components(data = s_d, 
#'                 type = "2D",
#'                 col = col_user(n = length(s$BHE)))
#' 
#' ## plot components as 3D plot, uncomment to use
#' #plot_components(data = s_d, 
#' #                 type = "3D",
#' #                 col = rainbow(n = length(s$BHE)))
#'                 
#' @export plot_components
#' 
plot_components <- function(
  
  data,
  type = "2D",
  order = "xyz",
  ...
) {
  
  ## save initial plot parameters
  par_old <- graphics::par(no.readonly = TRUE)
  
  ## set restoration of initial parameters
  on.exit(graphics::par(par_old))
  
  ## extract additional plot arguments
  args <- list(...)
  
  ## check/set plot arguments
  if ("main" %in% names(args)) {
    main <- args$main
  } else {
    
    if(type == "2D") {
      
      main <- c("EW-Vertical", 
                "NS-Vertical", 
                "EW-NS")
    } else if(type == "3D" | type == "scene") {
      
      main <- "Component plot"
    }
  }
  
  if ("xlab" %in% names(args)) {
    
    xlab <- args$xlab
  } else {
    
    if(type == "2D") {
      
      xlab <- c("East-West", 
                "North-South", 
                "East-West")
    } else if(type == "3D" | type == "scene") {
      
      xlab <- "East-West"
    }
  }
  
  if ("ylab" %in% names(args)) {
    
    ylab <- args$ylab
  } else {
    
    if(type == "2D") {
      
      ylab <- c("Vertical", 
                "Vertical", 
                "North-South")
    } else if(type == "3D" | type == "scene") {
      
      ylab <- "North-South"
    }
  }
  
  if ("zlab" %in% names(args)) {
    zlab <- args$zlab
  } else {
    
    zlab <- "Vertical"
  }
  
  if ("lwd" %in% names(args)) {
    
    lwd <- args$lwd
  } else {
    
    lwd <- 1
  }
  
  if ("col" %in% names(args)) {
    
    col <- args$col
  } else {
    
    col <- colorRampPalette(colors = 1)
    col <- col(n = ncol(data))
  }
  
  if ("phi" %in% names(args)) {
    
    phi <- args$phi
  } else {
    
    phi <- 20
  }
  
  if ("theta" %in% names(args)) {
    
    theta <- args$theta
  } else {
    
    theta <- 30
  }
  
  ## homogenise data structure
  if(class(data[[1]])[1] == "eseis") {
    
    ## store initial object
    eseis_data <- data
    
    ## extract signal vector
    data <- lapply(X = data, FUN = function(X) {
      
      X$signal
    })
    
    ## convert signal vector list to matrix
    data <- do.call(cbind, data)
    
  }
  
  ## homogenise data structure
  if(class(data)[1] == "list") {
    
    data <- do.call(cbind, data)
  }
  
  data <- as.data.frame(x = data)
  
  ## optionally update component order
  component_ID <- strsplit(x = order, split = "")[[1]]
  
  data <- data[,order(component_ID)]
  
  ## option 1 - 2D plot panel
  if(type == "2D") {
    
    ## change plot options
    graphics::par(mfcol = c(1, 3), 
                  pty = "s")
    
    ## prepare data structure for plotting
    data_segments_1 <- data.frame(x0 = data[,1][-nrow(data)],
                                  y0 = data[,3][-nrow(data)],
                                  x1 = data[,1][-1],
                                  y1 = data[,3][-1])
    
    data_segments_2 <- data.frame(x0 = data[,2][-nrow(data)],
                                  y0 = data[,3][-nrow(data)],
                                  x1 = data[,2][-1],
                                  y1 = data[,3][-1])
    
    data_segments_3 <- data.frame(x0 = data[,1][-nrow(data)],
                                  y0 = data[,2][-nrow(data)],
                                  x1 = data[,1][-1],
                                  y1 = data[,2][-1])
    
    ## cretae first plot
    graphics::plot(NA, 
                   xlim = range(data[,1]), 
                   ylim = range(data[,3]),
                   main = main[1],
                   xlab = xlab[1],
                   ylab = ylab[1])
    
    graphics::segments(x0 = data_segments_1$x0, 
                       y0 = data_segments_1$y0, 
                       x1 = data_segments_1$x1, 
                       y1 = data_segments_1$y1,
                       col = col,
                       lwd = lwd)
    
    ## create second plot
    graphics::plot(NA, 
                   xlim = range(data[,2]), 
                   ylim = range(data[,3]),
                   main = main[2],
                   xlab = xlab[2],
                   ylab = ylab[2])
    
    graphics::segments(x0 = data_segments_2$x0, 
                       y0 = data_segments_2$y0, 
                       x1 = data_segments_2$x1, 
                       y1 = data_segments_2$y1,
                       col = col,
                       lwd = lwd)
    
    ## create thrid plot
    graphics::plot(NA,
                   xlim = range(data[,1]), 
                   ylim = range(data[,2]),
                   main = main[3],
                   xlab = xlab[3],
                   ylab = ylab[3])
    
    graphics::segments(x0 = data_segments_3$x0, 
                       y0 = data_segments_3$y0, 
                       x1 = data_segments_3$x1, 
                       y1 = data_segments_3$y1,
                       col = col,
                       lwd = lwd)
    
  } else if(type == "3D") {
    
    ## check if package plot3D is installed
    if (requireNamespace("plot3D", quietly = TRUE) == FALSE) {
      
      stop("Package plot3D is not installed, 3D is not possible!")
    }
    
    ## create 3D plot
    plot3D::segments3D(x0 = data[,1][-length(data[,1])], 
                       y0 = data[,2][-length(data[,2])], 
                       z0 = data[,3][-length(data[,3])],
                       x1 = data[,1][-1], 
                       y1 = data[,2][-1], 
                       z1 = data[,3][-1],
                       xlim = range(data),
                       ylim = range(data),
                       zlim = range(data),
                       main = main,
                       xlab = xlab,
                       ylab = ylab,
                       zlab = zlab,
                       lwd = lwd,
                       col = col,
                       phi = phi,
                       theta = theta, 
                       ticktype = "detailed")
    
  } else if(type == "scene") {
    
    ## check if package rgl is installed
    if (requireNamespace("rgl", quietly = TRUE) == FALSE) {
      
      stop("Package rgl is not installed, scene is not possible!")
    }
    
    ## prepare data structure
    data_plot <- data.frame(x0 = data[,1][-length(data[,1])], 
                            y0 = data[,2][-length(data[,2])], 
                            z0 = data[,3][-length(data[,3])],
                            x1 = data[,1][-1], 
                            y1 = data[,2][-1], 
                            z1 = data[,3][-1])
    
    ## open rgl device
    rgl::open3d(scale=c(1, 1, 1))
    
    ## draw line segments
    rgl::segments3d(x=as.vector(t(data_plot[,c(1, 4)])),
                    y=as.vector(t(data_plot[,c(2, 5)])),
                    z=as.vector(t(data_plot[,c(3, 6)])), 
                    col = col,
                    lwd = lwd)
    
    ## add axes
    rgl::axes3d()
    
    ## annotate axes
    rgl::title3d(xlab = xlab,
                 ylab = ylab,
                 zlab = zlab)
  }
  
  ## restore old plot parameters
  graphics::par(par_old)
}