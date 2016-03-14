# load library
library(Rlpj)

# We need to specify the path of each input file inside the mainDir:
file.co2<-"/co2_1901-2013_FAKE.txt"
file.cru <- "/crudata/cru_1901_2006.bin"
file.cru.misc <- "/crudata/cru_1901_2006misc.bin"
file.ndep <- "/crudata/GlobalNitrogenDeposition.bin"

# if you are using the global_cf.ins file you neet to specify the site specific input files as well
file.temp <- "/inputLPJ/temp.nc"
file.prec <- "/inputLPJ/prec.nc"
file.insol <- "/inputLPJ/rad.nc"
mainDir <- "/home/trashtos/GitHub/lpjRun"
gridList <- "gridlist_geb.txt"


# Create some paramaters to test modell.
# Number of runs is proportional to number of parameter set you are testing
parameterDefault <- list (run_lamda_max = NULL, run_reprfrac=NULL, run_emax = NULL,
                          run_wscal_min = NULL, run_drought_tolerance=NULL)
# I want to test 6 different values for emax.
# I want therefore to run 20 time the LPJ
par <- seq(1,5, len = 6)
print (par)
# I create the list object with the parameters
parameterList <- vector("list", length(par))
for (i in 1:length(par)) {
  parameterDefault$run_emax <- par[i]
  parameterList[[i]] <- parameterDefault
}
#print(parameterList[c(2:3)])


# call library for setup object
suspect  <- setupParallel(3, "SOCK", "global", "cf", gridList = "gridlist_geb.txt", mainDir = "/home/trashtos/GitHub/lpjRun")

# run the modell
proc1 <- proc.time()
result <- runLPJParallel(setupObject = suspect, plot.data = FALSE, save.plots = FALSE,
               parameterList=parameterList, file.co2, file.cru, file.cru.misc,
               file.ndep, file.temp, file.prec, file.insol)

proc.time() - proc1
