# RLPJ

A R package that wraps the LPJ-GUESS model

## Installing Rlpj

Rlpj depends of three packages:

- zoo
- snow
- Rmpi (only needed for MPI clusters)

We recommend to install first these packages and then install Rlpj. To install Rlpj directly from this repository, please type the following in Rstudio:

    devtools::install_github("biometry/RLPJ/Rlpj", ref = "master", build_vignettes = TRUE)

In the case you want to install the dependencies at the same time, set the dependencies flag to TRUE.

    devtools::install_github("biometry/RLPJ/Rlpj", ref = "master", build_vignettes = TRUE, dependencies = TRUE)


