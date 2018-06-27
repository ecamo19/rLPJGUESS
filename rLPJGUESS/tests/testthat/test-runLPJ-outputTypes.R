testthat::context("Test basic")
set.seed(1)
library(rLPJGUESS)

testthat::test_that("basic run", {

  file.co2<-"/some/absolute/path/crudata/co2_1901-2013.txt"
  file.cru <- "/some/absolute/path/crudata/cru_1901_2006.bin"
  file.cru.misc <- "/some/absolute/path/crudata/cru_1901_2006misc.bin"
  file.ndep <- "/some/absolute/path/crudata/GlobalNitrogenDeposition.bin"
  file.temp <- "/some/absolute/path/cfdata/temp.nc"
  file.prec <- "/some/absolute/path/cfdata/prec.nc"
  file.insol <- "/some/absolute/path/cfdata/rad.nc"

  mainDir <- "../LPJ-Example"
  list.files(mainDir)

  settings <- list (gridList = NA ,mode = "cf", scale = "global",
                    file.co2 = file.co2, file.cru = file.cru,
                    file.cru.misc = file.cru.misc, file.ndep = file.ndep,
                    file.temp = file.temp, file.prec = file.prec,
                    file.insol = file.insol, delete = F)

  # result <-  runLPJ(mainDir, settings= settings)

})


testthat::test_that("data test", {

  data <- "../LPJ-Example/runDirectory/outDirectory"

  LPJout <- getLPJData(typeList = c("cmass",  "lai"),
                        data, runInfo = list(parameter1 = 0.5, grid = 1))
  plotLPJData(LPJout, typeList = c("cmass", "lai"))
})



# getTemplate To obtain the in-package stored model templates
#
# getParameterList To obtain the parameter default values
#
# getDesign To obtain the default desgin
#
# getTypeList To obtain the default output model types
#
# getRunInfo To recover data or parameters from the runInfoDir
#
#
#
# writeTemplate To write LPJ-GUESS templates




