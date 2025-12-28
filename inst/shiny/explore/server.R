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
  
  shinyDirChoose(
    input,
    'dir_sac',
    roots = c(home = '~'),
    filetypes = c('', 'txt', 'bigWig', "tsv", "csv", "bw")
  )
  
  global <- reactiveValues(datapath = getwd())
  
  dir <- reactive(input$dir_sac)
  
  output$dir_sac <- renderText({
    global$datapath
  })
  
  observeEvent(ignoreNULL = TRUE,
               eventExpr = {
                 input$dir_sac
               },
               handlerExpr = {
                 if (!"path" %in% names(dir())) return()
                 home <- normalizePath("~")
                 global$datapath <-
                   file.path(home, paste(unlist(dir()$path[-1]), 
                                         collapse = .Platform$file.sep))
               })
  
  
  s_raw <- reactive({ 
    
    if(input$load == 0) return()
    
    isolate({
      
      s_raw <- eseis::aux_getevent(start = as.POSIXct(isolate(input$start)),
                          duration = as.numeric(isolate(input$dur)),
                          station = as.character(isolate(input$sta)),
                          component = as.character(isolate(input$cmp)),
                          dir = paste0(isolate(input$dir_sac), "/"))
      
      output$plot <- renderPlot({
        
        eseis::plot_signal(s_raw)})
    })
  })
  
  
  output$plot <- renderPlot({
    
    eseis::plot_signal(s_raw)
    
    if(input$deconvolve == TRUE) {
      
      s <- eseis::signal_deconvolve(
        data = s_raw, 
        sensor = input$sensor, 
        logger = input$logger, 
        gain = as.numeric(input$gain))
    } else {
      
      s <- s_raw
    }
    
    if(input$demean == TRUE) {
      
      s <- eseis::signal_demean(data = s)
    }
    
    if(input$detrend == TRUE) {
      
      s <- eseis::signal_detrend(data = s)
    }
    
    if(input$filter == TRUE) {
      
      s <- eseis::signal_filter(data = s, f = input$f_filter)
    }
    
    if(input$taper == TRUE) {
      
      s <- eseis::signal_taper(data = s, p = input$p_taper)
    }
    
    if(input$envelope == TRUE) {
      
      s <- eseis::signal_envelope(data = s)
    }
    
    ## STA LTA picking
    if(input$pick_stalta == TRUE) {
      
      events_stalta <- eseis::pick_stalta(
        data = s, 
        sta = round(input$t_sta * 1/s$meta$dt), 
        lta = round(input$t_lta * 1/s$meta$dt), 
        freeze = input$stalta_freeze,
        on = input$stalta_on,
        off = input$stalta_off)$picks
      
      stalta_start <- events_stalta$start
      stalta_stop <- events_stalta$start + events_stalta$duration
      
      if(length(stalta_start) < 1) {
        
        stalta_start <- NA
        stalta_stop <- NA
      }
    } else {
      stalta_start <- NA
      stalta_stop <- NA
    }
    
    ## kurtosis picking
    if(input$pick_kurtosis == TRUE) {
      
      events_kurtosis <- eseis::pick_kurtosis(
        data = s, 
        on = input$kurtosis_on, 
        off = input$kurtosis_off, 
        dur_min = input$kurtosis_dur_min,
        dur_max = input$kurtosis_dur_max,
        window_kurt = round(input$kurtosis_kurt_size * 1/s$meta$dt),
        window_amp = round(input$kurtosis_amp_size * 1/s$meta$dt))$picks
      
      events_kurtosis$duration[is.na(events_kurtosis$duration)] <- 
        input$kurtosis_dur_max * 1/s$meta$dt
      kurtosis_start <- events_kurtosis$start
      kurtosis_stop <- events_kurtosis$start + events_kurtosis$duration
      
      if(length(kurtosis_start) < 1) {
        
        kurtosis_start <- NA
        kurtosis_stop <- NA
      }
    } else {
      kurtosis_start <- NA
      kurtosis_stop <- NA
    }            
    
    ## plotting
    par(mfcol = c(as.numeric(input$n_plots), 1))
    
    if(input$plot_1 != "disabled") {
      
      if(input$plot_1 == "waveform") {
        
        eseis::plot_signal(data = s)
        
        for(i in 1:length(stalta_start)) {
          
          polygon(x = c(stalta_start[i], stalta_start[i], 
                        stalta_stop[i], stalta_stop[i]),
                  y = c(min(s$signal), max(s$signal), 
                        max(s$signal), min(s$signal)),
                  border = NULL, 
                  col = adjustcolor(col = "orange", alpha.f = 0.3))
        }
        
        for(i in 1:length(kurtosis_start)) {
          
          polygon(x = c(kurtosis_start[i], kurtosis_start[i], 
                        kurtosis_stop[i], kurtosis_stop[i]),
                  y = c(min(s$signal), max(s$signal), 
                        max(s$signal), min(s$signal)),
                  border = NULL,
                  col = adjustcolor(col = "darkblue", alpha.f = 0.3))
        }
        
        if(input$stalta_show == TRUE) {
          
          ## calculate sta vector
          data_sta <- caTools::runmean(x = s$signal, 
                                       k = round(input$t_sta * 1/s$meta$dt), 
                                       alg = "fast", 
                                       endrule = "NA",
                                       align = "right")
          
          ## calculate lta vector
          data_lta <- caTools::runmean(x = s$signal, 
                                       k = round(input$t_lta * 1/s$meta$dt), 
                                       alg = "fast", 
                                       endrule = "NA",
                                       align = "right")
          
          ## calculate stalta vector
          data_stalta <- data_sta / data_lta
          
          par(new = TRUE)
          plot(data_stalta, type = "l", ann = FALSE, 
               axes = FALSE, col = "red")
          axis(side = 4)
        }
        
      } else if(input$plot_1 == "spectrum") {
        
        eseis::plot_spectrum(data = eseis::signal_spectrum(data = s))
      } else if(input$plot_1 == "PSD") {
        
        eseis::plot_spectrogram(data = eseis::signal_spectrogram(data = s))
      } else if(input$plot_1 == "PPSD") {
        
        eseis::plot_ppsd(data = eseis::signal_spectrogram(data = s))
      }
    }
    
    if(input$plot_2 != "disabled") {
      
      if(input$plot_2 == "waveform") {
        
        eseis::plot_signal(data = s)
      } else if(input$plot_2 == "spectrum") {
        
        eseis::plot_spectrum(data = eseis::signal_spectrum(data = s))
      } else if(input$plot_2 == "PSD") {
        
        eseis::plot_spectrogram(data = eseis::signal_spectrogram(data = s))
      } else if(input$plot_2 == "PPSD") {
        
        eseis::plot_ppsd(data = eseis::signal_spectrogram(data = s))
      }
    }
    
    if(input$plot_3 != "disabled") {
      
      if(input$plot_3 == "waveform") {
        
        eseis::plot_signal(data = s)
      } else if(input$plot_3 == "spectrum") {
        
        eseis::plot_spectrum(data = eseis::signal_spectrum(data = s))
      } else if(input$plot_3 == "PSD") {
        
        eseis::plot_spectrogram(data = eseis::signal_spectrogram(data = s))
      } else if(input$plot_3 == "PPSD") {
        
        eseis::plot_ppsd(data = eseis::signal_spectrogram(data = s))
      }
    }
  })
}
