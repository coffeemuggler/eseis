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
                 sliderInput("d_s", "Grain-size D_50 (log10 m):",
                             min = -5, max = 2, value = 0, step = 0.03),
                 sliderInput("s_s", "Grain-size stdv. (log10):",
                             min = 0.1, max = 4, value = 0.9),
                 sliderInput("r_s", "Sediment density (k/m³):",
                             min = 500, max = 5000, value = 2650),
                 sliderInput("q_s", "Sediment flux (kg/ms):",
                             min = 0, max = 100, value = 2, step = 0.01)
               )
      ),
      tabPanel("River", sidebarPanel(
        sliderInput("h_w", "River depth (m):",
                    min = 0.01, max = 40, value = 10),
        sliderInput("w_w", "River width (m):",
                    min = 1, max = 500, value = 5, step = 1),
        sliderInput("a_w", "River gradient (radians):",
                    min = 0.0001, max = 0.1, value = 0.0075),
        sliderInput("r_0", "Distance to river (m):",
                    min = 1, max = 100000, value = 20)
      )),
      tabPanel("Seismic", sidebarPanel(
        sliderInput("q_0", "Quality factor at f_0 (n.u.):",
                    min = 1, max = 100, value = 15),
        sliderInput("v_0", "Phase velocity (m/s):",
                    min = 10, max = 5000, value = 500, step = 1),
        sliderInput("p_0", "Power law variation exponent (n.u.):",
                    min = 0.1, max = 2, value = 0.30, step = 0.01),
        sliderInput("e_0", "Quality factor increase with frequency (n.u.):",
                    min = 0.0, max = 2, value = 0.0, step = 0.01),
        sliderInput("n_0", "Greens function exponents (n.u.):",
                    min = 0.1, max = 2, value = c(0.5, 0.8)),
        sliderInput("f", "Frequency range to model (Hz):",
                    min = 1, max = 500, value = c(1, 100), step = 1),
        sliderInput("f_0", "Reference frequency (Hz):",
                    min = 1, max = 500, value = 1, step = 1)
      )),
      tabPanel("Data", sidebarPanel(
        textInput("data", "Empiric spectrum (R object name):", value = "")
      )),
      tabPanel("Plot", sidebarPanel(
        checkboxInput("plot_river", "River spectrum", TRUE),
        checkboxInput("plot_bedload", "Bedload spectrum", TRUE),
        checkboxInput("plot_river_bedload", "Combined specturm", TRUE),
        checkboxInput("plot_empiric", "Empiric spectrum", TRUE),
        sliderInput("xlim", "x-axis limits",
                    min = 0, max = 1000, value = c(1, 100)),
        sliderInput("ylim", "y-axis limits:",
                    min = -250, max = 100, value = c(-180, -100), step = 1),
        sliderInput("res", "Spectrum resolution:",
                    min = 10, max = 10000, value = 1000)
      ))
      
    ),
    mainPanel(
      plotOutput("main_plot", height = 600)
    )
  )
))