[![Project Status: Active – The project has reached a stable, usable state and is being actively developed.](http://www.repostatus.org/badges/latest/active.svg)](http://www.repostatus.org/#active)
[![Build Status](https://travis-ci.org/biometry/RLPJ.svg?branch=master)](https://travis-ci.org/biometry/RLPJ)

# Rlpj

A R package that wraps the LPJ-GUESS model

### Installing Rlpj

Rlpj depends on the "zoo" package, in order to facilitate the handling of the results. Moreover, Rlpj relies on two additional packages for parallelization:

- snow (for SOCK cluster, if you use PC/laptop)
- Rmpi (for MPI clusters)

Rlpj depends on package "snow". Installing "Rmpi" might be complicated, and it is not strictly required: If you are thinking of using Rlpj on a laptop or workstation, you will be dealing with SOCK clusters and you do not need Rmpi.


### Recommended installation

Install the latest stable release form https://github.com/biometry/RLPJ/releases.

You can download the binary and install it as follows

```{r}
install.packages("/path/to/binary/Rlpj_0.1.1.tar.gz", repos = NULL, type = "source")
```
Or you can install it directly from the download link

```{r}
library(devtools)
install_url("https://github.com/biometry/RLPJ/releases/download/v0.1.3/Rlpj_0.1.3.tar.gz")
library(Rlpj)
?Rlpj
```

### Development release 

If you want to install the current (development) version from this repository, run

```{r}
devtools::install_github(repo = "biometry/RLPJ", subdir = "Rlpj", 
dependencies = T, build_vignettes = T)
```
Below the status of the automatic Travis CI tests on the master branch 

[![Build Status](https://travis-ci.org/biometry/RLPJ.svg?branch=master)](https://travis-ci.org/biometry/RLPJ)


### Build the package yourself 

Clone the package to your computer, and build with hand or Rstudio. If you need help see here http://biometry.github.io/APES/R/R70-PackageDevelopment.html


In Rstudio, the vignette may not be built per default. You will turn this on in your project options, or run 

```{r}
devtools::build_vignettes("Rlpj")
```
