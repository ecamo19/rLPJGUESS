# Rlpj

A R package that wraps the LPJ-GUESS model

#### Installing Rlpj

It is highly recommended to install the "zoo" package before installing Rlpj, in order to facilitate the handling of the results. Moreover, Rlpj relies on two packages for parallelization:

- snow (for SOCK cluster, if you use PC/laptop)
- Rmpi (for MPI clusters)

Installing Rmpi might be complicated. If you are thinking of using Rlpj on a laptop or workstation, you will be dealing with SOCK clusters and you do not need Rmpi.


#### Recommended installation

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
You may want to check if the link above is really the latest stable realease. 

#### Build the package yourself 

Clone the package to your computer, and build with hand or Rstudio. If you need help see here http://biometry.github.io/APES/R/R70-PackageDevelopment.html


In Rstudio, the vignette may not be built per default. You will turn this on in your project options, or run 

```{r}
devtools::build_vignettes("Rlpj")
```
