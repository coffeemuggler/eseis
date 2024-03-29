# eseis

The R package 'eseis' provides a collection of R functions for environmental seismology data processing.

For further details, visit the  [official website](http://micha-dietze.de/pages/eseis.html).

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
devtools::install_github(repo = "coffeemuggler/eseis")
```

## Note

**The package comes without any guarantee!**

## License

This program is free software: you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation, either version 3 of the License, or
any later version.

Copyright © 2019 Helmholtz Centre Potsdam GFZ German Research Centre for Geosciences, Potsdam, Germany

eseis is free software: you can redistribute it and/or modify it under the terms of the GNU General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. eseis is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied. See the [GNU General Public License](https://github.com/coffeemuggler/eseis/blob/0.4.0/LICENSE) for more details.

## Citation

Please use both of the below references when citing the usage of the package. It is important to not just use one of them but the two.

Dietze, M. (2018). eseis: Environmental Seismology Toolbox. R package version 0.7.0. GFZ Data Services. http://doi.org/10.5880/GFZ.5.1.2018.001

Dietze, M.: The R package "eseis" – a software toolbox for environmental seismology, Earth Surf. Dynam., 6, 669–686, https://doi.org/10.5194/esurf-6-669-2018, 2018. 

### Contact

Michael Dietze 


Helmholtz Centre Potsdam GFZ German Research Centre for Geoscienes 

Section 4.6 Geomorphology

Telegrafenberg

14473 Potsdam

Germany