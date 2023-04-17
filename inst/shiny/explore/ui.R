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
#' gui_explore()
#' 
#' @export shinyUI
#' 
shinyUI(fluidPage(
  
  sidebarLayout(
    tabsetPanel(
      tabPanel(
        "Load event", 
        sidebarPanel(
          h3("Event data"),
          hr(),
          div(style="display: inline-block;vertical-align:top; width: 200px;",
              textInput("start", 
                        "Starttime (Y-m-d H:M:S UTC)", 
                        "2017-03-21 04:00:00")),
          div(style="display: inline-block;vertical-align:top; width: 100px;",
              textInput("dur", "Duration (s)", "10")),
          div(style="display: inline-block;vertical-align:top; width: 150px;",
              textInput("sta", "Station ID", "RUEG1")),
          div(style="display: inline-block;vertical-align:top; width: 150px;",
              textInput("cmp", "Component", "BHZ")),
          hr(),
          div(style="display: inline-block;vertical-align:top; width: 150px;",
              shinyFiles::shinyDirButton("dir_sac", "Data directory", "sac")),
          div(style="display: inline-block;vertical-align:top; width: 150px;",
              actionButton("load", "Load event")),
          verbatimTextOutput("dir_sac")
        )),
      
      tabPanel(
        "Preprocessing", 
        sidebarPanel(
          checkboxInput("deconvolve", "Deconvolve", FALSE),
          selectInput("sensor", "Sensor", 
                      choices = c("PE6B", "TC120s")),
          selectInput("logger", "Logger", 
                      choices = c("Cube3ext", "Cube3extBOB", "Centaur")),
          textInput("gain", "Gain", "1"),
          checkboxInput("demean", "Demean", FALSE),
          checkboxInput("detrend", "Detrend", FALSE),
          checkboxInput("filter", "Filter", FALSE),
          sliderInput("f_filter", "Filter frequencies", value = c(0, 10), 
                      min = 0, max = 200, step = 0.1),
          checkboxInput("taper", "Taper", FALSE),
          sliderInput("p_taper", "Taper size", min = 0, max = 0.5, 
                      value = 0),
          checkboxInput("envelope", "Envelope", FALSE)
        )),
      
      tabPanel(
        "Picking", 
        sidebarPanel(
          h4("STA/LTA picker"),
          checkboxInput("pick_stalta", "Enable STA/LTA", FALSE),
          sliderInput("t_sta", "STA time", min = 0, 
                      max = 10, value = 0.5, step = 0.05),
          sliderInput("t_lta", "STA time", min = 0, 
                      max = 180, value = 20, step = 0.05),
          sliderInput("stalta_on", "On-ratio", min = 0,
                      max = 10, value = 2, step = 0.05),
          sliderInput("stalta_off", "Off-ratio", min = 0,
                      max = 10, value = 1, step = 0.05),
          checkboxInput("stalta_freeze", "Freeze LTA", FALSE),
          checkboxInput("stalta_show", "Show STA-LTA ratio", FALSE),
          hr(),
          h4("Kurtosis picker"),
          checkboxInput("pick_kurtosis", "Enable kurtosis", FALSE),
          sliderInput("kurtosis_on", "On-level", min = 0, 
                      max = 50, value = 2, step = 0.05),
          sliderInput("kurtosis_off", "Off-level", min = 0, 
                      max = 20, value = 1, step = 0.05),
          sliderInput("kurtosis_dur_min", "Minimum duration", min = 0, 
                      max = 60, value = 5, step = 0.05),
          sliderInput("kurtosis_dur_max", "Maximum duration", min = 0, 
                      max = 180, value = 20, step = 0.05),
          sliderInput("kurtosis_kurt_size", 
                      "Kurtosis window size (s)", min = 0,
                      max = 60, value = 2, step = 0.05),
          sliderInput("kurtosis_amp_size",
                      "Amplitude window size (s)", min = 0,
                      max = 60, value = 10, step = 0.05)
        )),
      
      tabPanel(
        "Locating", 
        sidebarPanel(
          
        )),
      
      tabPanel(
        "Visualisation",
        sidebarPanel(
          selectInput("n_plots", "Number of plot panels",
                      choices = c("1", "2", "3")),
          selectInput("plot_1", "Plot type",
                      choices = c("waveform", "spectrum", 
                                  "PSD", "PPSD", "disabled"), 
                      selected = "waveform"),
          selectInput("plot_2", "Plot type",
                      choices = c("waveform", "spectrum", 
                                  "PSD", "PPSD", "disabled"), 
                      selected = "disabled"),
          selectInput("plot_3", "Plot type",
                      choices = c("waveform", "spectrum", 
                                  "PSD", "PPSD", "disabled"), 
                      selected = "disabled"),
          sliderInput("plot_height", "Plot height", 
                      min = 0, max = 2000, value = 600)
        ))
    ),
    
    mainPanel(
      plotOutput("plot", height = 800)
    )
  )
  
))