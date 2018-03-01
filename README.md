# eseis

The R package 'eseis' provides a collection of R functions for environmental seismology data processing.

For further details, visit the  [official website](http://micha-dietze.de/pages/eseis.html).

## Installation

#### i. Requirements

With release of version 0.4.0 the GNU Compiler Collection (*gcc*) 
becomes essential to install 'eseis' from source. Depending on your OS please download and install one of the following:

**Windows (32/64bit)** - 'Rtools' (provided by CRAN)

   http://cran.r-project.org/bin/windows/Rtools/

**Mac OS X** - 'Xcode' (provided by Apple)

   https://developer.apple.com/xcode/downloads/

**Linux** systems usually have *gcc* pre-installed. No further action is needed.

#### ii. Install the package

I recommend installing 'eseis' from Github using the R package 'devtools' (install it using `install.packages("devtools")`):

```r
devtools::install_github(repo = "coffeemuggler/eseis", ref = "0.4.0")
```

## Note

**The package comes without any guarantee!**

Please note that this package is at a development stage and may change day by day. 
## License

This program is free software: you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation, either version 3 of the License, or
any later version.

 This program is distributed in the hope that it will be useful,
 but WITHOUT ANY WARRANTY; without even the implied warranty of
 MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 [GNU General Public License](https://github.com/coffeemuggler/eseis/0.4.0/blob/LICENSE) for more details.