## Test environments
* local Ubuntu 16.04 LTS, R 3.4.4
* win-builder (devel and release)

## R CMD check results
There were no ERRORs

There was 1 WARNING:

* Note: significantly better compression could be obtained by using R CMD build --resave-data - which is what I added to the build routine and which solved that warning.

## Downstream dependencies
All packages could be installed