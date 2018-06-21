#-------------------------------------------------------------------------------#
# load library
library(rLPJGUESS)
#-------------------------------------------------------------------------------#
#         GENERAL SETTINGS
#-------------------------------------------------------------------------------#
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
                  variable.insol = variable.insol,save = F, delete = F, plot.data =TRUE,
                  save.plots=F, processing = F)

                  #, fun ="met")
#-------------------------------------------------------------------------------#
#         SERIAL
#-------------------------------------------------------------------------------#
result <-   runLPJ(mainDir, settings= settings)
result <-   try(runLPJ(mainDir, settings= settings), silent = T)

suppressMessages(result <-   runLPJ(mainDir, settings= settings))
print(result)

settings$save <- F
result <-   runLPJ(mainDir, settings= settings)

settings$mode  <- "cru"
result <-   runLPJ(mainDir, settings= settings)

settings$mode  <- "cf"
design <- getDesign("europe", list = T)
design <- design[1:4]


design$run_nyear_spinup  <- 410
settings$design <- design
result <-   runLPJ(mainDir, settings= settings)

design$run_ifcalcsla  <- 1
settings$design <- design
parameterList <- list("C4_gr_include" = 0)
result <-   runLPJ(mainDir, settings= settings, parameterList = parameterList)

settings$design <- NULL
result <-   runLPJ(mainDir, settings= settings)

design <- getDesign("europe", list =T)
design <- design[{1:4}]

design$run_nyear_spinup  <- 410
settings$design <- design
result <-  runLPJ(mainDir, settings= settings)

#, fun ="met")


typeList <- getTypeList()

result <-   runLPJ(mainDir, settings= settings, typeList = typeList)

parameterList <- getParameterList("europe", list = T)
result <-   runLPJ(mainDir, settings= settings, parameterList = parameterList)

parameterList <- getParameterList("europe", list = F)
result <-   runLPJ(mainDir, settings= settings, parameterList = parameterList)

parameterList <- as.matrix(t(parameterList))
result <-   runLPJ(mainDir, settings= settings, parameterList = parameterList)

#-------------------------------------------------------------------------------#
#         PARALLEL
#-------------------------------------------------------------------------------#
settings$plot.data <- F
# Create some paramaters to test modell.
# Number of runs is proportional to number of parameter set you are testing
parameterDefault <- list (Que_rob_gdd5min_est=NULL)
#parameterDefault <- getParameterList("europe", list = T)
# I want to test 6 different values for emax.
# I want therefore to run 20 time the LPJ
par <- seq(500, 3000, len = 3)
print (par)
# I create the list object with the parameters
parameterList <- vector("list", length(par))
for (i in 1:length(par)) {
  parameterDefault$Que_rob_gdd5min_est <- par[i]
  parameterList[[i]] <- parameterDefault
}
#print(parameterList[c(2:3)])
mySetup  <- setupLPJParallel(numCores = 3, clusterType ="SOCK", mainDir = "/home/trashtos/GitHub/lpjRun")


proc1 <- proc.time()
result <-   runLPJ(mySetup,  settings= settings, parameterList = parameterList)
proc.time() - proc1


parameterList <- as.matrix(par)
colnames(parameterList) <- "Que_rob_gdd5min_est"
rownames(parameterList) <- NULL


result <-   runLPJ(mySetup,  settings= settings, parameterList = parameterList)



