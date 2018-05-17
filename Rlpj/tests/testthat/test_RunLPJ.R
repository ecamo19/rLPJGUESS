context("Test basic")
set.seed(1)
library(Rlpj)


# Here place all the general test that don't require to evaluate the model code


test_that("runLPJ Tests - general", {
  #result <-   runLPJ(mainDir, settings= settings)
  #outputTypes <- names(result["dataTypes"])
  # runtests
  #checkOutputsTypes(outputTypes)

})


# We need to specify the path of each input file inside the mainDir:
file.co2<-"/home/trashtos/GitHub/lpjRun/crudata/co2_1901-2013_FAKE.txt"
file.cru <- "/home/trashtos/GitHub/lpjRun/crudata/cru_1901_2006.bin"
file.cru.misc <- "/home/trashtos/GitHub/lpjRun/crudata/cru_1901_2006misc.bin"
file.ndep <- "/home/trashtos/GitHub/lpjRun/crudata/GlobalNitrogenDeposition.bin"
# if you are using the global_cf.ins file you neet to specify the site specific input files as well
file.temp <- "/home/trashtos/GitHub/lpjRun/inputLPJ/temp.nc"
file.prec <- "/home/trashtos/GitHub/lpjRun/inputLPJ/prec.nc"
file.insol <- "/home/trashtos/GitHub/lpjRun/inputLPJ/rad.nc"
variable.temp <- "temp"
variable.prec <-"prec"
variable.insol <- "rad"
mainDir <- "/home/trashtos/GitHub/lpjRun"
gridList <- "gridlist_geb.txt"
toremove <- list.files(mainDir,full.names = T, recursive = T,  pattern = "run")
do.call(file.remove, list(toremove))
settings <- list (gridList = gridList,mode = "cf", scale = "europe",
                  file.co2 = file.co2, file.cru = file.cru, file.cru.misc = file.cru.misc,
                  file.ndep = file.ndep, file.temp = file.temp, file.prec = file.prec,
                  file.insol = file.insol, variable.temp = variable.temp, variable.prec = variable.prec,
                  variable.insol = variable.insol,save = F, delete = T, plot.data =F,
                  save.plots=F, processing = F)


checkOutputsTypes <- function(outputTypes){
  defaultTypes <- getTypeList()

  expect_false(length(outputTypes[!outputTypes %in% defaultTypes]) > 0)
  expect_false(length(defaultTypes[!defaultTypes %in% outputTypes]) > 0)
}


test_that("runLPJ Tests - binaries", {

  skip_on_travis()

  skip_on_cran()

  #result <-   runLPJ(mainDir, settings= settings)
  #outputTypes <- names(result["dataTypes"])
  # runtests
  #checkOutputsTypes(outputTypes)

})




