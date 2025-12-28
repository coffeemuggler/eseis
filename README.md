# eseis

The R package 'eseis' v. 0.9.0 provides a collection of R functions for environmental seismology data processing. While there are numerous other packages and software collections for basic, state-of-the-art seismic data processing, the functions of 'eseis' are tailored to environmental-seismological problems.

For further details, visit the  [official website](http://playgroundearth.de/pages/eseis.html).

## Updates and info

As of November 2024, the new version 0.8 is available through CRAN. This version brings along quite a few major changes, part of which were overdue from old legacies, part were imposed by CRAN and progress/changes with other packages. The NEWS file contains a detailed list of all relevant changes throughout the last 1.5 years. Most important, especially when running old R scripts with the new package are the following changes:

- New functions have been added to interpolate signals, differentiate signals, fill signals, and check temporal data coverage
- It is now possible to pick events using data sets from a seismic network (functions aux_picknetwork and aux_picknetworkparallel)
- The location family has been expanded by the parabola technique using the function spatial_parabola(), and all spatial analysis functions had been updated to accommodate the change from the 'raster' package to the 'terra' package
- The signal picker functions family has been expanded and picker functions now have their own prefix: pick_stalta(), pick_correlation(), pick_kurtosis(). Hence, the old function signal_stalta() does no longer exist
- Seismic noise interferometry (noise cross correlation, ncc) support has been added, through the functions ncc_correlate(), ncc_stretch(), plot_correlation()
- There is now a convenience function to plot a seismic event as a comprehensive figure (waveform, spectrum, spectrogram, statistics): plot_event()
- Station XML is now supported for signal deconvolution, and further instruments (sensors and loggers) have been added to the keyword list for zero-pole-based deconvolution, as well as some minor bugs have been corrected
- Station info files are now required as data frames in R, instead of as path/file names to those files, when converting Digos DataCube files to mseed or SAC files in the default eseis file structure, and the entire aux_organiscubefiles() function has been updated for more expressive error catching and fast and robust conversion
- The GUI of fluvial and bedload models has been refurbished to now also support empirical spectra in the plot
- Seismic data import via the function read_data() has been improved to handle data gaps and wildcards for components

Numerous orphaned or abandoned packages needed to be accounted for, including: multitaper, raster, IRISSeismic. This means that several functions may now work different or that their flexibility and number of available features had to be reduced. This is a pity in those cases but I found no way to work around those issues. 

## Installation

#### i. Requirements

With release of version 0.7.0 the GNU Compiler Collection (*gcc*) 
becomes essential to install 'eseis' from source. Depending on your OS please download and install one of the following:

**Windows (32/64bit)** - 'Rtools' (provided by CRAN)

   http://cran.r-project.org/bin/windows/Rtools/

**Mac OS X** - 'Xcode' (provided by Apple)

   https://developer.apple.com/xcode/downloads/

**Linux** systems usually have *gcc* pre-installed. No further action is needed.

Furthermore, in order to convert seismic data recorded by a Digos Cube data logger, one needs external software. The gipptools (https://www.gfz-potsdam.de/en/section/geophysical-deep-sounding/infrastructure/geophysical-instrument-pool-potsdam-gipp/software/gipptools/) are a collection of functions that are designed to convert the proprietary file format of Cube loggers to mseed files, supported by many seismic data handlign software. The gipptools are used by the function aux_stationinfofile() and aux_organisecubefiles().

If the function aux_fixmseed() shall be used, the additional software dataselect (https://github.com/iris-edu/dataselect) must be installed, as well. 

#### ii. Install the package

I recommend installing 'eseis' from Github using the R package 'devtools' (install it using `install.packages("devtools")`):

```r
devtools::install_github(repo = "coffeemuggler/eseis", "dev_0.9.0")
```

## Note

**The package comes without any guarantee!**

## License

This program is free software: you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation, either version 3 of the License, or
any later version.

Copyright © 2019 Helmholtz Centre Potsdam GFZ German Research Centre for Geosciences, Potsdam, Germany

'eseis' is free software: you can redistribute it and/or modify it under the terms of the GNU General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. 'eseis' is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied. See the [GNU General Public License](https://github.com/coffeemuggler/eseis/blob/0.4.0/LICENSE) for more details.

## Citation

Please use both of the below references when citing the usage of the package. It is important to not just use one of them but the two.

Dietze, M. (2018). eseis: Environmental Seismology Toolbox. R package version 0.8.0. GFZ Data Services. http://doi.org/10.5880/GFZ.5.1.2018.001

Dietze, M.: The R package "eseis" – a software toolbox for environmental seismology, Earth Surf. Dynam., 6, 669–686, https://doi.org/10.5194/esurf-6-669-2018, 2018. 

### Contact

Michael Dietze 


University of Göttingen
Faculty of Geoscience and Geography
Goldschmidtstr. 5-7
37077 Göttingen

Germany
