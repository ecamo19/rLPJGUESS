# RLPJ

A R package that wraps the LPJ-GUESS model

## Installing Rlpj

Rlpj depends on three packages:

- zoo
- snow (for SOCK cluster)
- Rmpi (for MPI clusters)

Installing Rmpi might be complicated. If you are thinking of using Rlpj on a laptop or workstation, you will be dealing with SOCK clusters and you do not need Rmpi.

To avoid complications with installation, we recommend to install first the dependencies and then install Rlpj. For installing Rlpj directly from this repository, please type the following in Rstudio:

    devtools::install_github("biometry/RLPJ/Rlpj", ref = "master", build_vignettes = TRUE)

In the case you want to install the dependencies at the same time, set the dependencies flag to Depends

    devtools::install_github("biometry/RLPJ/Rlpj", ref = "master", build_vignettes = TRUE, dependencies = "Depends")


