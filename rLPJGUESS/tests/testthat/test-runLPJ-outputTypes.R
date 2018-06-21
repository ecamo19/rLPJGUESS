context("Test basic")
set.seed(1)
library(rLPJGUESS)

# Here place all the general test that don't require to evaluate the model code
mainDir <- normalizePath("~/Github/TestCode/LPJrunTest/")

# We need to specify the path of each input file inside the mainDir:
file.co2<-normalizePath("~/Github/TestCode/LPJrunTest/crudata/co2_1765-2500_RCP3.txt")
file.cru <- normalizePath("~/Github/TestCode/LPJrunTest/crudata/cru_1901_2006.bin")
file.cru.misc <- normalizePath("~/Github/TestCode/LPJrunTest/crudata/cru_1901_2006misc.bin")
file.ndep <- normalizePath("~/Github/TestCode/LPJrunTest/crudata/GlobalNitrogenDeposition.bin")
# if you are using the global_cf.ins file you neet to specify the site specific input files as well
file.temp <- normalizePath("~/Github/TestCode/LPJrunTest/temp.nc")
file.prec <- normalizePath("~/Github/TestCode/LPJrunTest/prec.nc")
file.insol <- normalizePath("~/Github/TestCode/LPJrunTest/rad.nc")
variable.temp <- "temp"
variable.prec <-"prec"
variable.insol <- "rad"

gridList <- "gridlist.txt"
toremove <- list.files(mainDir,full.names = T, recursive = T,  pattern = "run")
do.call(file.remove, list(toremove))
settings <- list (gridList = gridList,mode = "cf", scale = "europe",
                  file.co2 = file.co2, file.cru = file.cru, file.cru.misc = file.cru.misc,
                  file.ndep = file.ndep, file.temp = file.temp, file.prec = file.prec,
                  file.insol = file.insol, variable.temp = variable.temp, variable.prec = variable.prec,
                  variable.insol = variable.insol,save = F, delete = T, plot.data =F,
                  save.plots=F, processing = F)


# Settings to test
settingName <-c("nyear_spinup", "iffire", "npatch", "patcharea", "estinterval",
                "ifdisturb", "distinterval" )
settingName <- paste("run", settingName, sep = "_")
settingValue <-c(250, 0, 5, 500, 5, 0, 50 )


checkTemplates <- function(testTemplate, runTemplate, setting){
  setting <- gsub("run_", "", setting)
  expect_true(identical( testTemplate[grepl(setting, testTemplate)],  runTemplate[grepl(setting, runTemplate)]))
}

test_that("templates are the same", {

  skip_on_travis()
  skip_on_cran()

for (i in 1:length(settingName)){
    designLPJ <- getDesign("europe", list = T)
    designLPJ[[settingName[i]]] <- settingValue[i]
    settings$design <- designLPJ
    # runLPJ
    result <-   runLPJ(mainDir, settings= settings)
    # get the folder with test data
    examplesDir <- list.dirs(system.file("extdata", package = "rLPJGUESS"), recursive = T)
    examplesDir <- examplesDir[grepl(settingName[i],examplesDir )]
    examplesFiles <- list.files(examplesDir, full.names = T, recursive = T)
    # test whether the templates are the same
    testTemplate1 <- readLines(examplesFiles[grepl("europe.ins",examplesFiles)])
    runTemplate1 <- result["runInfo"]$template1Mem
   # checkTemplates(testTemplate1, runTemplate1, settingName[i])
    #testTemplate2 <- readLines(examplesFiles[grepl("europe_cf.ins",examplesFiles)])
    #runTemplate2 <- result["runInfo"]$template2Mem
    #checkTemplates(testTemplate2, runTemplate2, settingName[i])

    # read in the templates for the example

    # test whether the ooutpus are the same


  }

})


