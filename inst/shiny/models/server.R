#' Function to generate GUI server for eseis
#' 
#' A graphical user interface (GUI) server backend is started.
#' 
#' @param input A collection of input objects.
#' @param output A collection of input objects.
#' @return A GUI server.
#' @author Michael Dietze
#' @keywords eseis
#' @examples
#' 
#' ## Not run
#' gui_models()
#' 
#' @export shinyServer

# Define server logic required to draw a histogram
shinyServer <- function(input, output, session) {
  
  observe({
    updateSliderInput(session, "d_s",
                      min = input$d_s_min,
                      max = input$d_s_max)
    
    updateSliderInput(session, "s_s",
                      min = input$s_s_min,
                      max = input$s_s_max)
    
    updateSliderInput(session, "r_s",
                      min = input$r_s_min,
                      max = input$r_s_max)
    
    updateSliderInput(session, "q_s",
                      min = input$q_s_min,
                      max = input$q_s_max)
    
    updateSliderInput(session, "h_w",
                      min = input$h_w_min,
                      max = input$h_w_max)
    
    updateSliderInput(session, "w_w",
                      min = input$w_w_min,
                      max = input$w_w_max)
    
    updateSliderInput(session, "a_w",
                      min = input$a_w_min,
                      max = input$a_w_max)
    
    updateSliderInput(session, "r_0",
                      min = input$r_0_min,
                      max = input$r_0_max)
    
    updateSliderInput(session, "q_0",
                      min = input$q_0_min,
                      max = input$q_0_max)
    
    updateSliderInput(session, "v_0",
                      min = input$v_0_min,
                      max = input$v_0_max)
    
    updateSliderInput(session, "p_0",
                      min = input$p_0_min,
                      max = input$p_0_max)
    
    updateSliderInput(session, "e_0",
                      min = input$e_0_min,
                      max = input$e_0_max)
    
    updateSliderInput(session, "n_0",
                      min = input$n_0_min,
                      max = input$n_0_max)
  })
  
  output$main_plot <- renderPlot({
    
    if(nchar(input$data) > 0) {
      
      p_empiric <- get(input$data)
    } else {
      
      p_empiric <- list(
        spectrum = data.frame(
          frequency = seq(from = input$f[1],
                          to = input$f[2], 
                          length.out = input$res),
          spectrum = rep(1e-200, input$res)),
        meta = list(type = "spectrum"))
      class(p_empiric) = "eseis"
    }
    
    if(input$plot_river) {
      
      p_turbulence <- eseis::model_turbulence(d_s = input$d_s,
                                              s_s = input$s_s,
                                              r_s = input$r_s,
                                              h_w = input$h_w,
                                              w_w = input$w_w,
                                              a_w = input$a_w,
                                              f = input$f,
                                              r_0 = input$r_0,
                                              f_0 = input$f_0,
                                              q_0 = input$q_0,
                                              v_0 = input$v_0,
                                              p_0 = input$p_0,
                                              n_0 = input$n_0,
                                              res = input$res, 
                                              eseis = TRUE)      
    } else {
      
      p_turbulence <- list(
        spectrum = data.frame(
          frequency = seq(from = input$f[1],
                          to = input$f[2], 
                          length.out = input$res),
          spectrum = rep(1e-200, input$res)),
        meta = list(type = "spectrum"))
      class(p_turbulence) = "eseis"
    }
    
    if(input$plot_bedload) {
      
      p_bedload <- eseis::model_bedload(d_s = input$d_s,
                                        s_s = input$s_s,
                                        r_s = input$r_s,
                                        q_s = input$q_s / input$r_s,
                                        h_w = input$h_w,
                                        w_w = input$w_w,
                                        a_w = input$a_w,
                                        f = input$f,
                                        r_0 = input$r_0,
                                        f_0 = input$f_0,
                                        q_0 = input$q_0,
                                        e_0 = input$e_0,
                                        v_0 = input$v_0,
                                        x_0 = input$p_0,
                                        n_0 = input$n_0[1],
                                        res = input$res,
                                        eseis = TRUE)
      
    } else {
      
      p_bedload <- list(
        spectrum = data.frame(
          frequency = seq(from = input$f[1],
                          to = input$f[2], 
                          length.out = input$res),
          spectrum = rep(1e-200, input$res)),
        meta = list(type = "spectrum"))
      class(p_bedload) = "eseis"
    }
    
    if(input$plot_river_bedload) {
      
      p_combined <- p_bedload
      p_combined$spectrum$spectrum <- p_combined$spectrum$spectrum + 
        p_turbulence$spectrum$spectrum
      
    } else {
      
      p_combined <- list(
        spectrum = data.frame(
          frequency = seq(from = input$f[1],
                          to = input$f[2], 
                          length.out = input$res),
          spectrum = rep(1e-200, input$res)),
        meta = list(type = "spectrum"))
      class(p_combined) = "eseis"
    }
    
    if(input$plot_empiric == TRUE) {
      
      eseis::plot_spectrum(data = p_empiric,
                           xlim = input$xlim,
                           ylim = input$ylim,
                           col = adjustcolor("green", 0.5),
                           lwd = 1.5,
                           ann = FALSE,
                           axes = FALSE)
      par(new = TRUE)
      
    }
    
    eseis::plot_spectrum(data = p_turbulence,
                         xlim = input$xlim,
                         ylim = input$ylim,
                         col = "blue")    
    par(new = TRUE)
    
    eseis::plot_spectrum(data = p_bedload,
                         xlim = input$xlim,
                         ylim = input$ylim,
                         col = "brown",
                         ann = FALSE,
                         axes = FALSE)
    
    par(new = TRUE)
    eseis::plot_spectrum(data = p_combined,
                         xlim = input$xlim,
                         ylim = input$ylim,
                         col = "grey20",
                         lwd = 1.5,
                         ann = FALSE,
                         axes = FALSE)
    
  })
}