## -----------------------------------------------------------------------------
rm(list=ls())
library(rLPJGUESS)

## -----------------------------------------------------------------------------
mainDir <- system.file("extdata", package = "rLPJGUESS")
list.files(mainDir) 

## ----fig.show='hold'----------------------------------------------------------
file_co2 <-"/some/absolute/path/crudata/co2_1901-2006.txt"
file_cru <- "/some/absolute/path/crudata/cru_1901_2006.bin"
file_cru_misc <- "/some/absolute/path/crudata/cru_1901_2006misc.bin"
file_ndep <- "/some/absolute/path/crudata/GlobalNitrogenDeposition.bin"

## ----fig.show='hold'----------------------------------------------------------
file_temp <- "/some/absolute/path/cfdata/temp.nc"
file_prec <- "/some/absolute/path/cfdata/prec.nc"
file_insol <- "/some/absolute/path/cfdata/rad.nc"

## ----fig.show='hold'----------------------------------------------------------
gridList <- "gridlist.txt"

## ----fig.show='hold'----------------------------------------------------------
settings <- list (gridList = gridList,mode = "cf", scale = "global",
                  file.co2 = file_co2, file.cru = file_cru, file.cru.misc = file_cru_misc,
                  file.ndep = file_ndep, file.temp = file_temp, file.prec = file_prec,
                  file_insol = file_insol)

## ----eval=F-------------------------------------------------------------------
#  result <-   runLPJ(mainDir,  settings= settings)

## -----------------------------------------------------------------------------
options(error=traceback)
mySetup <- setupLPJParallel(numCores= 3, clusterType = "SOCK", mainDir=mainDir)
mySetup

## -----------------------------------------------------------------------------
list.files(mainDir) 

## -----------------------------------------------------------------------------
runDir <- file.path(mainDir, "runDirectory1")
list.files(runDir)

## -----------------------------------------------------------------------------
parameterDefault <- getParameterList("europe", list = F)
parameterDefault[40:50,]

## -----------------------------------------------------------------------------
parameterDefault <- getParameterList("europe", list = T)

## -----------------------------------------------------------------------------
parameterDefault$common_emax

## -----------------------------------------------------------------------------
parameterList <- as.matrix(seq(1,5, len = 20))
colnames(parameterList) <- "common_emax"
head(parameterList)

## -----------------------------------------------------------------------------
dummy <- list (common_emax = NULL)
par <- seq(1,5, len = 20)
parameterList <- vector("list", length(par))
for (i in 1:length(par)) {
  dummy$common_emax <- par[i]
  parameterList[[i]] <- dummy
}
print(parameterList[2:3])

## -----------------------------------------------------------------------------
typeList <- c("aaet", "nuptake")

## ----eval=F-------------------------------------------------------------------
#  result <-   runLPJ(mySetup,  settings= settings, parameterList = parameterList )

## -----------------------------------------------------------------------------
runDir <- file.path(mainDir, "runDirectory1")
list.files(runDir)

## -----------------------------------------------------------------------------
parameters <- parameterList[[1]] 
parameters

## -----------------------------------------------------------------------------
template <- getTemplate("europe", runDir)
list.files(runDir)
template <- readLines(file.path(runDir, "europe.ins"))
template [c(156:166)]

## -----------------------------------------------------------------------------
writeTemplate(template1 = "europe.ins", parameterList = parameters, runDir = runDir)

## -----------------------------------------------------------------------------
template <- readLines(file.path(runDir, "europe.ins"))
template [c(156:166)]

## ----eval=F-------------------------------------------------------------------
#  callLPJ(mainDir, runDir, template, mode)

## -----------------------------------------------------------------------------
outDir <- system.file("extdata/exampleOutputs", package = "rLPJGUESS")
list.files(outDir)

## -----------------------------------------------------------------------------
typeList <- c("aaet","lai")

## -----------------------------------------------------------------------------
LPJout <- getLPJData(x = outDir,typeList = typeList,  runInfo=list(runNumber = 1, template1 = "europe.ins", template2="europe_cf.ins", runDir = "runDirectory1"))
LPJout

## -----------------------------------------------------------------------------
slotNames(LPJout)

## -----------------------------------------------------------------------------
names(LPJout["runInfo"])

## -----------------------------------------------------------------------------
names(LPJout["dataTypes"])
summary(LPJout["lai"])

## ----eval=F-------------------------------------------------------------------
#  LPJout["runDir"]

## -----------------------------------------------------------------------------
summary(LPJout["lai"])

## ----fig.show= "hold", fig.keep ='all',  fig.width=7, fig.heigth=16-----------
plotLPJData(x = LPJout, typeList = "lai" , outDir = outDir, save.plots = FALSE)

## ----echo=FALSE---------------------------------------------------------------
files <- list.files(mainDir)
files <- files[!grepl("grid", files)]
files <- files[!grepl("example", files)]
files <- files[!grepl("LPJParameters_calibrate", files)]
unlink(file.path(mainDir, files), recursive = TRUE)

