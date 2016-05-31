# load library
library(Rlpj)

# We need to specify the path of each input file inside the mainDir:

file.co2<-"/home/trashtos/GitHub/lpjRun/crudata/co2_1901-2013_FAKE.txt"
file.cru <- "/home/trashtos/GitHub/lpjRun/crudata/cru_1901_2006.bin"
file.cru.misc <- "/home/trashtos/GitHub/lpjRun/crudata/cru_1901_2006misc.bin"
file.ndep <- "/home/trashtos/GitHub/lpjRun/crudata/GlobalNitrogenDeposition.bin"

# if you are using the global_cf.ins file you neet to specify the site specific input files as well
file.temp <- "/home/trashtos/GitHub/lpjRun/inputLPJ/temp.nc"
file.prec <- "/home/trashtos/GitHub/lpjRun/inputLPJ/prec.nc"
file.insol <- "/home/trashtos/GitHub/lpjRun/inputLPJ/rad.nc"
mainDir <- "/home/trashtos/GitHub/lpjRun"
gridList <- "gridlist_geb.txt"


settings <- list (gridList = gridList,mode = "cf", scale = "europe",
                  file.co2 = file.co2, file.cru = file.cru, file.cru.misc = file.cru.misc,
                  file.ndep = file.ndep, file.temp = file.temp, file.prec = file.prec,
                  file.insol = file.insol, delete = F, plot.data =T, save.plots=F)
result <-   runLPJ(mainDir, settings= settings, typeList = c("aaet"))






# Create some paramaters to test modell.
# Number of runs is proportional to number of parameter set you are testing
parameterDefault <- list (run_lamda_max = NULL, run_reprfrac=NULL, run_emax = NULL,
                          run_wscal_min = NULL, run_drought_tolerance=NULL)
# I want to test 6 different values for emax.
# I want therefore to run 20 time the LPJ
par <- seq(1,5, len = 3)
print (par)
# I create the list object with the parameters
parameterList <- vector("list", length(par))
for (i in 1:length(par)) {
  parameterDefault$run_emax <- par[i]
  parameterList[[i]] <- parameterDefault
}
#print(parameterList[c(2:3)])
mySetup  <- setupLPJParallel(numCores = 3, clusterType ="SOCK", mainDir = "/home/trashtos/GitHub/lpjRun")

proc1 <- proc.time()
result <-   runLPJ(mySetup,  settings= settings, parameterList = parameterList,
                   typeList = c("aaet"))
proc.time() - proc1


# call library for setup object




