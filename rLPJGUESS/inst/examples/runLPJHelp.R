\dontrun{
file.co2<-"/some/absolute/path/crudata/co2_1901-2013.txt"
file.cru <- "/some/absolute/path/crudata/cru_1901_2006.bin"
file.cru.misc <- "/some/absolute/path/crudata/cru_1901_2006misc.bin"
file.ndep <- "/some/absolute/path/crudata/GlobalNitrogenDeposition.bin"
file.temp <- "/some/absolute/path/cfdata/temp.nc"
file.prec <- "/some/absolute/path/cfdata/prec.nc"
file.insol <- "/some/absolute/path/cfdata/rad.nc"

mainDir <- "/some/absolute/path/mainDir"
list.files(mainDir)
[1] "guess" or "guesscmd.exe"  # link to the model executable
[2] "gridlist.txt"      # list of gridcells
[3] "global.ins"        # template1 (optional)
[4] "global_cru.ins"    # template2 (optional)

# General settings
settings <- list (gridList = gridList,mode = "cf", scale = "global",
               file.co2 = file.co2, file.cru = file.cru,
               file.cru.misc = file.cru.misc, file.ndep = file.ndep,
               file.temp = file.temp, file.prec = file.prec,
               file.insol = file.insol, delete = FALSE)


# Single  Run
result <-  runLPJ(mainDir, settings= settings)
result
    class              : LPJData
    LPJ template 1     : global.ins
    LPJ template 2     : global_cf.ins
    grid cells         : 99  Somewhere
    run directory      : /some/absolute/path/mainDir/runDirectory
    LPJ model outputs  : 39 outputs
    aaet agpp aiso amon anpp cflux clitter cmass cpool cton_leaf dens
    firert fpc speciesheight lai maet mevap mgpp mintercep miso mlai mmon
    mnee mnpp mpet mra mrh mrunoff mwcont_lower mwcont_upper nflux ngases
    nlitter nmass npool nsources nuptake runoff vmaxnlim


#  Parallel Run
# Create some paramaters to test the model.
# Number of runs is proportional to number of parameter being testet
parameterDefault <- list (run_emax = NULL)

# Test 6 different values for emax.
par <- seq(1,5, len = 6)
# Create the list object with the parameters
parameterList <- vector("list", length(par))
for (i in 1:length(par)) {
  parameterDefault$run_emax <- par[i]
  parameterList[[i]] <- parameterDefault
}

# Call setupParallel
mySetup  <- setupLPJParallel(3, "SOCK", "cf",
                            mainDir = "/some/absolute/path/mainDir")

# Call runLPJ
result <- runLPJ(mySetup, settings= settings, parameterList = parameterList)
str(result,1)
  List of 6
  $ :Formal class 'LPJData' [package "rLPJGUESS"] with 2 slots
  $ :Formal class 'LPJData' [package "rLPJGUESS"] with 2 slots
  $ :Formal class 'LPJData' [package "rLPJGUESS"] with 2 slots
  $ :Formal class 'LPJData' [package "rLPJGUESS"] with 2 slots
  $ :Formal class 'LPJData' [package "rLPJGUESS"] with 2 slots
  $ :Formal class 'LPJData' [package "rLPJGUESS"] with 2 slots


}
