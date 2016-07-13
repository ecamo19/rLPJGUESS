#Internal data
#setwd("/home/trashtos/GitHub/ownBranches/RLPJ/Rlpj")
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
files.parameters$cf <- list(file.co2 = c("path_to_co2_1901-2006.txt", NULL),
                           file.cru = c("path_to_cru_1901_2006.bin", NULL),
                           file.cru.misc = c("path_to_cru_1901_2006misc.bin", NULL),
                           file.ndep = c("path_to_GlobalNitrogenDeposition.bin", NULL),
                           file.temp = c("path_to_temp.nc", NULL),
                           file.prec = c("path_to_prec.nc", NULL),
                           file.insol = c("path_to_rad.nc", NULL))

files.parameters$cru <- list(file.co2 = c("path_to_co2_1901-2006.txt", NULL),
                           file.cru = c("path_to_cru_1901_2006.bin", NULL),
                           file.cru.misc = c("path_to_cru_1901_2006misc.bin", NULL),
                           file.ndep = c("path_to_GlobalNitrogenDeposition.bin" , NULL))


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

parameterList.default <-  as.matrix(read.table("./data-raw/LPJParameters.csv", header = T, sep = "\t"))
rownames(parameterList.default) <- parameterList.default[, "name"]
parameterList.default <-parameterList.default[,c("scale", "type",  "value")]

#----------------------------------------------------------------------------------------------#-
# Put the data in the package


devtools::use_data(parameterList.default, files.parameters, typelist.default, templates, internal = TRUE, overwrite = TRUE)

