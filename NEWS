Changes for R-package eseis

Version 0.7.2
  - feature added, signal_filter, zero phase shift option added
  - bug fixed, spatial_distance, SpatRaster converted to list to maintain rda-file compatibility
  - bugs fixed, fmi_spectra and fmi_inversion, power units corrected
  - function added, read_fdsn, to be replacing aux_getFDSNdata in the future
  - function added, read_data, to be replacing aux_getevent in the future
  - renaming of parameters for homogenisation in several functions
  - transition from package raster (and sp) to terra
  - function revised, aux_psdsummary, fully overhauled to create long PSDs
  - function removed, aux_psdpanel removed as it was obsolete
  - bug fixed, plot_spectrogram() legend glitch for log = "y" fixed
  - feature added, signal_spectrum() argument n added for smoothing support
  - code changed, signal_spectrum() output name spectrum changed to power
  - code changed, plot_spectrogram() colour scale set to Inferno
  - code changed, plot_spectrogram() colour range set to quantile(0.01, 0.99)
  - feature added, aux_getevent() automatic file type recognition
  - bug fixed, signal_filter() p = 0.05 now added
  - bug fixed, signal_motion() windowing fixed, speed increased, output added
  - feature added, plot_spectrogram documentation extended for colour scales

Version 0.6.0 (2021-11-26)
  - function updated, signal_filter() conversion of signal vector from ts to numeric added
  - bug fixed, aux_stationinfofile() GPS extraction routine updated
  - bug fixed, aux_stationinfofile() Windows support added
  - bug fixed, aux_organisecubefiles() Windows support added
  - bug fixed, list_sensor() L4C sensor poles definition corrected
  - bug fixed, spatial_amplitude() error catch implemented
  - bug fixed, aux_eseisobspy() file name update propagated
  - feature added, signal_hvratio() log-scale option added
  - feature added, plot_ppsd() xlim and ylim options added
  - feature added, aux_getevent() more precise error messages added
  - feature added, aux_getevent() screen output suppression
  - function renamed, signal_stalta() to pick_stalta()
  - function added, spatial_track()
  - function added, model_amplitude()
  - function added, gui_explore()
  - function added, signal_stats()
  - function added, signal_fill()
  - function added, pick_kurtosis()
  - function added, pick_correlation()
  - function added, aux_sonifysignal()
  - function added, aux_cubeinfo()
  - function model_bedload() corrected for wide GSD cases
  - function spatial_amplitude() bugfix, coupling factor implementation
  - function signal_filter() feature added, lazy option added

Version 0.5.0 (2019-12-16)
  - function signal_filter(): frequency domain filtering implemented
  - signal type meta element added
  - function write_mseed() added
  - function aux_obspyeseis() added
  - functionaux_eseisobspy() added
  - function signal_clip() added
  - function signal_cut() added
  - function aux_commondt() added
  - function model_bedload() added
  - function spatial_amplitude() added
  - function spatial_crop() added
  - function spatial_pmax() added
  - function fmi_parameters() added
  - function fmi_spectra() added
  - function fmi_inversion() added

Version 0.4.0 (2018-05-25)
  - eseis object implemented and all functions modified to support this object
  - function aux_initiateeseis() added
  - function aux_psdsummary() added
  - function plot_spectrum() added
  - function aux_getFDSNstation() added
  - function aux_getFDSNdata() added
  - function plot_ppsd() added
  - function aux_psdpanels() added
  - function write_report() added
  - function model_turbulence() added
  - functions aux_getIRISstation() and aux_getIRISdata() renamed
  - plot_signal(), speed increased by factor ten
  - plot_spectrogram(), time shift bug fixed, log-y option added, default labels added, option to keep plot arguments added 
  - plot functions, time axis format option added
  - aux_organisecubefiles(), manual mseed conversion option added
  - aux_organisecubefiles(), JRE heap space modification added
  - aux_organisecubefiles(), temp directory removal changed to unlink()
  - aux_stationinfofile(), removal of outliers implemented
  - aux_psdsummary(), bug removed
  - aux_organisecubefiles(), feature added (mseed_keep)
  - aux_getfdsnstation(), option added to only return ftl links
  - aux_getevent(), time zone conflicts issue solved

Version 0.3.2 (2017-07-26)
  - inherited internal issues without relevance for functionality solved
  - function aux_loadevent() added
  - function plot_components() added
  - function signal_motion() added
  - function signal_hvratio() added
  - function aux_hvanalysis() added
  - function aux_organisecntaurfiles() added
  - function signal_deconvolve(): s-parameter for PE6B corrected, argument gain added
  - function plot_signal() added
  - function signal_spectrogram(): na-handling implemented
  - function aux_fixmseed() added
  - function spatial_distance(): aoi option implemented
  - function aux_gettemperature() added
  - function aux_getevent() added
  - functions with FFT usage optimised by fftw approach
  - function aux_getirisstations() added
  - function aux_getirisdata() added

Version 0.3.1 (2017-01-21)
  - function aux_stationinfofile() added
  - function aux_organisecubefiles() added
  - function signal_clip() added
  - function time_clip() added
  - function plot_spectrogram() revised
  - function list_logger() and list_sensor() extended
  - function read_mseed() added
  - function signal_rotate added
  - function signal_integrate() added
  - function signal_spectrum() added

Version 0.3.0 (2016-06-22)
  - All functions reworked for Roxygen2
  - Syntax changed
  - write_sac() new defined to be without depenencies, improved handling and data input
  - read_sac() new defined to be without depenencies, more consistent data handling
  - new functions for signal processing added: signal_detrend, signal_demean, signal_taper, signal_padd, signal_sum
  - function spatial_distance() now merges distance_map() and distance_stations()
  - functions to store sensor and logger data added
  - function signal_deconvolve rewritten to take manually supplied instrument data
  - function time_convert modified to allow three time formats

Version 0.2.3 (2015-02-11)
  - Added function: hilbert()
  - Added function: distance.map()
  - Added function: distance.stations()
  - Added function: snr()
  - Function envelope() modified, now using hilbert() and spec.taper()

Version 0.2.1 (2014-10-17)
  - Complete renaming of the package
  - extensive reworking of documentation
  - Added function: write.sac()
  - Added function: scan.files()
  - Added function: cube.GPS()

Version 0.1.3 (2014-06-20)
  - Added function: cube2mseed()
  - Added function: mseed2sac()
  - Modified functions: many, mainly to now only support vectors/matrices
	
Version 0.1.2 (2014-01-14)
  - Added function: clip.array()
  - Added function: spectrogram()
  - Added function: filter.signal()
  - Removed function: create.spectrogram()
  - Removed function: plot.sac.old()
  - Removed function: filter.sac.old()
  - Modified function: build.array() - summary content extended

Version 0.1.1 (2014-01-06)
  - Initial version

