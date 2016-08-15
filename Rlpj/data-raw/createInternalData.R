#Internal data
setwd("/home/trashtos/GitHub/RLPJ/Rlpj")
#------------------------------------------------------------------------------#
#       TEMPLATES
#------------------------------------------------------------------------------#
# list.files
files <- list.files("./data-raw", full.names = TRUE)
# open globals and save them
files.global <- sort(files[grepl("global", files)])
# create global object
global <- vector("list", length(files.global))
names(global) <- gsub(".ins","", gsub("./data-raw/", "", files.global))
for (i in 1:length(global)){
  global[[i]] <- readLines(files.global[i])
}
# open globals and save them
files.europe <- sort(files[grepl("europe", files)])
# create global object
europe <- vector("list", length(files.europe))
names(europe) <- gsub(".ins","", gsub("./data-raw/", "", files.europe))
for (i in 1:length(europe)){
  europe[[i]] <- readLines(files.europe[i])
}
#
templates <- list("global"= global, "europe" = europe)

rm(global)
rm(europe)

#------------------------------------------------------------------------------#
#       TYPELIST
#------------------------------------------------------------------------------#
typelist.default <- c("aaet", "agpp", "aiso", "amon", "anpp", "cflux","clitter",
                      "cmass", "cpool", "cton_leaf", "dens", "firert", "fpc",
                      "speciesheight", "lai", "maet", "mevap", "mgpp","mintercep",
                      "miso", "mlai", "mmon", "mnee", "mnpp", "mpet", "mra", "mrh",
                      "mrunoff", "mwcont_lower", "mwcont_upper", "nflux", "ngases",
                      "nlitter", "nmass", "npool", "nsources", "nuptake", "runoff",
                      "vmaxnlim")

#------------------------------------------------------------------------------#
#       FILES
#------------------------------------------------------------------------------#
# default parameters
files.parameters <- list(cf = NULL, cru = NULL)
files.parameters$cf <- list(file.co2 = c("path_to_co2", NULL),
                           file.cru = c("path_to_cru", NULL),
                           file.cru.misc = c("path_to_cru_misc", NULL),
                           file.ndep = c("path_to_ndep", NULL),
                           file.temp = c("path_to_temp", NULL),
                           file.prec = c("path_to_prec", NULL),
                           file.insol = c("path_to_rad", NULL))

files.parameters$cru <- list(file.co2 = c("path_to_co2", NULL),
                           file.cru = c("path_to_cru", NULL),
                           file.cru.misc = c("path_to_cru_misc", NULL),
                           file.ndep = c("path_to_ndep" , NULL))


#------------------------------------------------------------------------------#
#       PARAMETER LIST
#------------------------------------------------------------------------------#
#Sometimes functions need pre-computed data tables.
# If you put these in data/ they’ll also be available to package users, which is not appropriate.
# Instead, you can save them in R/sysdata.rda. For example, two coloured related packages,
# munsell and dichromat, use R/sysdata.rda to store large tables of colour data.
# You can use devtools::use_data() to create this file with the argument internal = TRUE:
#   x <- sample(1000)
#   devtools::use_data(x, mtcars, internal = TRUE)
# Again, to make this data reproducible it’s a good idea to include the code used to generate it.
# Put it in data-raw/.
# Objects in R/sysdata.rda are not exported (they shouldn’t be), so they don’t need to be documented.
# They’re only available inside your package.

dummyFather <-  read.table("./data-raw/LPJParameters.csv", header = T, sep = ",")

design.default <- list()
dummy <- dummyFather[grep("design", dummyFather[, "type"]), ]
dummy[, "value"] <- as.character(dummy[, "value"])

design.default$global <- as.matrix(dummy[ dummy$scale=="global", "value"])
rownames(design.default$global) <- dummy[dummy$scale=="global", "name"]

design.default$europe <- as.matrix(dummy[ dummy$scale=="europe", "value"])
rownames(design.default$europe) <- dummy[dummy$scale=="europe", "name"]
rm(dummy)

parameterList.default <- list()
dummy <- dummyFather[grep("parameter", dummyFather[, "type"]), ]
dummy[, "value"] <- as.numeric(as.character(dummy[, "value"]))

parameterList.default$global <- as.matrix(dummy[ dummy$scale=="global", "value"])
rownames(parameterList.default$global) <- dummy[dummy$scale=="global", "name"]

parameterList.default$europe <- as.matrix(dummy[ dummy$scale=="europe", "value"])
rownames(parameterList.default$europe) <- dummy[dummy$scale=="europe", "name"]

rm(dummyFather)




#----------------------------------------------------------------------------------------------#-
# Put the data in the package


devtools::use_data(parameterList.default, design.default,
                   files.parameters, typelist.default, templates,
                   internal = TRUE, overwrite = TRUE)

