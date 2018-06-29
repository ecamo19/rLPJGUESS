[![Project Status: Active – The project has reached a stable, usable state and is being actively developed.](http://www.repostatus.org/badges/latest/active.svg)](http://www.repostatus.org/#active)
[![Build Status](https://travis-ci.org/biometry/rLPJGUESS.svg?branch=master)](https://travis-ci.org/biometry/rLPJGUESS)

# rLPJGUESS

A R package that wraps the [LPJ-GUESS model](http://iis4.nateko.lu.se/lpj-guess/)

### Recommended installation

Install the latest stable release from https://github.com/biometry/rLPJGUESS/releases.

You can download the binary and install it as follows

```{r}
install.packages("/path/to/binary/rLPJGUESS_1.1.0.tar.gz", repos = NULL, type = "source")
```
Or you can install it directly from the download link

```{r}
library(devtools)
install_url("https://github.com/biometry/rLPJGUESS/releases/download/v1.1.0/rLPJGUESS_1.1.0.tar.gz", 
dependencies = T, build_vignettes = T)
library(rLPJGUESS)
?rLPJGUESS
```

**Note**: rLPJGUESS relies on two packages for parallelization:

- snow (for SOCK cluster, if you use PC/laptop)
- Rmpi (for MPI clusters)

Installing "Rmpi" might be complicated, and it is not strictly required: If you are thinking of using rLPJGUESS on a laptop or workstation, you will be dealing with SOCK clusters and you do not need Rmpi.

### Development release 

If you want to install the current (development) version from this repository, run

```{r}
devtools::install_github(repo = "biometry/rLPJGUESS", subdir = "rLPJGUESS", 
dependencies = T, build_vignettes = T)
```
Below the status of the automatic Travis CI tests on the master branch 

[![Build Status](https://travis-ci.org/biometry/rLPJGUESS.svg?branch=master)](https://travis-ci.org/biometry/rLPJGUESS)


### Build the package yourself 

Clone the package to your computer, and build with hand or Rstudio. If you need help see here http://biometry.github.io/APES/R/R70-PackageDevelopment.html


In Rstudio, the vignette may not be built per default. You will turn this on in your project options, or run 

```{r}
devtools::build_vignettes("rLPJGUESS")
```
