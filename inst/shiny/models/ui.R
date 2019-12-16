#' Function to generate GUI for eseis
#' 
#' A graphical user interface (GUI) is started.
#' 
#' @return A GUI.
#' @author Michael Dietze
#' @keywords eseis
#' @examples
#' 
#' ## Not run
#' gui_models()
#' 
#' @export shinyUI
#' 
shinyUI(fluidPage(
  
  # Application title
  titlePanel("Seismic spectra model visualisation"),
  
  sidebarLayout(
    tabsetPanel(
      tabPanel("Sediment", 
               sidebarPanel(
                 sliderInput("d_s", "Grain-size D_50 (m):",
                             min = 0.01, max = 0.10, value = 0.05),
                 sliderInput("s_s", "Grain-size stdv. (log10):",
                             min = 0.1, max = 1, value = 0.2),
                 sliderInput("r_s", "Sediment density (kg/m³):",
                             min = 2000, max = 3000, value = 2650),
                 sliderInput("q_s", "Sediment flux (kg/ms):",
                             min = 0, max = 1, value = 0.5)
               )
      ),
      tabPanel("River", sidebarPanel(
        sliderInput("h_w", "River depth (m):",
                    min = 0.1, max = 2, value = 1),
        sliderInput("w_w", "River width (m):",
                    min = 1, max = 20, value = 10),
        sliderInput("a_w", "River gradient (radians):",
                    min = 0.001, max = 0.01, value = 0.005),
        sliderInput("r_0", "Distance to river (m):",
                    min = 1, max = 100, value = 20)
      )),
      tabPanel("Seismic", sidebarPanel(
        sliderInput("q_0", "Quality factor at f_0 (n.u.):",
                    min = 1, max = 100, value = 15),
        sliderInput("v_0", "Phase velocity (m/s):",
                    min = 100, max = 5000, value = 1000),
        sliderInput("p_0", "Power law variation exponent (n.u.):",
                    min = 0.1, max = 2, value = 0.45),
        sliderInput("e_0", "Quality factor increase w. f (n.u.):",
                    min = 0.0, max = 1, value = 0.0),
        sliderInput("n_0", "Greens function exponents (n.u.):",
                    min = 0.1, max = 2, value = c(0.5, 0.8)),
        sliderInput("f", "Frequency range to model (Hz):",
                    min = 1, max = 500, value = c(1, 100)),
        sliderInput("f_0", "Reference frequency (Hz):",
                    min = 1, max = 500, value = 1)
      )),
      tabPanel("Data", sidebarPanel(
        textInput("data", "Empiric spectrum (R object name):", value = "")
      )),
      tabPanel("Plot", sidebarPanel(
        checkboxInput("plot_river", "River spectrum", TRUE),
        checkboxInput("plot_bedload", "Bedload spectrum", TRUE),
        checkboxInput("plot_river_bedload", "Combined specturm", TRUE),
        checkboxInput("plot_empiric", "Empirical spectrum", TRUE),
        sliderInput("xlim", "x-axis limits",
                    min = 0, max = 1000, value = c(1, 100)),
        sliderInput("ylim", "y-axis limits:",
                    min = -250, max = 100, value = c(-180, -100)),
        sliderInput("res", "Spectrum resolution:",
                    min = 10, max = 10000, value = 1000)
      )),
      tabPanel("Setting", sidebarPanel(
        div(style="display: inline-block;vertical-align:top; width: 150px;",
            textInput("d_s_min", "Grain-size D_50 min (m)", 0.01)),
        div(style="display: inline-block;vertical-align:top; width: 150px;",
            textInput("d_s_max", "Grain-size D_50 max (m)", 0.10)),
        
        div(style="display: inline-block;vertical-align:top; width: 150px;",
            textInput("s_s_min", "Grain-size stdv min (log10)", 0.1)),
        div(style="display: inline-block;vertical-align:top; width: 150px;",
            textInput("s_s_max", "Grain-size stdv min (log10)", 1.0)),
        
        div(style="display: inline-block;vertical-align:top; width: 150px;",
            textInput("r_s_min", "Grain-size density (kg/m³)", 2000)),
        div(style="display: inline-block;vertical-align:top; width: 150px;",
            textInput("r_s_max", "Grain-size density (kg/m³)", 3000)),
        
        div(style="display: inline-block;vertical-align:top; width: 150px;",
            textInput("q_s_min", "Bedload flux (kg/sm)", 0)),
        div(style="display: inline-block;vertical-align:top; width: 150px;",
            textInput("q_s_max", "Bedload flux (kg/sm)", 1)),
        
        div(style="display: inline-block;vertical-align:top; width: 150px;",
            textInput("h_w_min", "River depth (m)", 0.1)),
        div(style="display: inline-block;vertical-align:top; width: 150px;",
            textInput("h_w_max", "River depth (m)", 2)),
        
        div(style="display: inline-block;vertical-align:top; width: 150px;",
            textInput("w_w_min", "River width (m)", 1)),
        div(style="display: inline-block;vertical-align:top; width: 150px;",
            textInput("w_w_max", "River width (m)", 20)),
        
        div(style="display: inline-block;vertical-align:top; width: 150px;",
            textInput("a_w_min", "River gradient (radians)", 0.001)),
        div(style="display: inline-block;vertical-align:top; width: 150px;",
            textInput("a_w_max", "River gradient (radians)", 0.01)),

        div(style="display: inline-block;vertical-align:top; width: 150px;",
            textInput("r_0_min", "River distance (m)", 1)),
        div(style="display: inline-block;vertical-align:top; width: 150px;",
            textInput("r_0_max", "River distance (m)", 100)),
        
        div(style="display: inline-block;vertical-align:top; width: 150px;",
            textInput("q_0_min", "Quality factor at f_0 (n.u.)", 1)),
        div(style="display: inline-block;vertical-align:top; width: 150px;",
            textInput("q_0_max", "Quality factor at f_0 (n.u.)", 100)),
        
        div(style="display: inline-block;vertical-align:top; width: 150px;",
            textInput("v_0_min", "Phase velocity (m/s)", 100)),
        div(style="display: inline-block;vertical-align:top; width: 150px;",
            textInput("v_0_max", "Phase velocity (m/s)", 5000)),
        
        div(style="display: inline-block;vertical-align:top; width: 150px;",
            textInput("p_0_min", "Power law variation exponent (n.u.)", 0.1)),
        div(style="display: inline-block;vertical-align:top; width: 150px;",
            textInput("p_0_max", "Power law variation exponent (n.u.)", 2)),
        
        div(style="display: inline-block;vertical-align:top; width: 150px;",
            textInput("e_0_min", "Quality factor increase w. f (n.u.)", 0)),
        div(style="display: inline-block;vertical-align:top; width: 150px;",
            textInput("e_0_max", "Quality factor increase w. f (n.u.)", 1)),
        
        div(style="display: inline-block;vertical-align:top; width: 150px;",
            textInput("n_0_min", "Greens function exponents (n.u.)", 0.1)),
        div(style="display: inline-block;vertical-align:top; width: 150px;",
            textInput("n_0_max", "Greens function exponents (n.u.)", 2))
      ))
    ),
    mainPanel(
      plotOutput("main_plot", height = 600)
    )
  )
))