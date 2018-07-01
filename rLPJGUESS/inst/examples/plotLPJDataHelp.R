\dontrun{
### Plot LPJData object  ###

# After runLPJ
result <-   runLPJ(mainDir,  settings= settings)
plotLPJData(result, save.plots = FALSE)

# After getLPJData
result <- getLPJData(x = outDir, typeList = typeList, runInfo = runInfo, processing = TRUE)
result <- getLPJData(x = outDir, typeList = typeList, runInfo = runInfo, processing = FALSE)
plotLPJData(result, save.plots = FALSE)


# Plot specific outputs
plotLPJData(result, typeList = c("aaet", "lai"), save.plots = FALSE)

# Save plots
plotLPJData(result,  outDir = "/runDir/outDir", save.plots = TRUE)


### Plot from runLPJ  ###
LPJsettings <- list(file.co2 = file.path(mainDir, "crudata", "co2_1765-2500_RCP3.txt"),
                    file.cru = file.path(mainDir, "crudata", "cru_1901_2006.bin"),
                    file.cru.misc = file.path(mainDir, "crudata", "cru_1901_2006misc.bin"),
                    file.ndep = file.path(mainDir, "crudata", "GlobalNitrogenDeposition.bin"),
                    file.temp = file.path(mainDir, "temp.nc"), file.prec = file.path(mainDir, "prec.nc"),
                    file.insol = file.path(mainDir, "rad.nc"), variable.temp = "temp", variable.insol = "rad",
                    variable.prec = "prec", delete = FALSE, save = FALSE, processing = TRUE,
                    plot.data = FALSE, save.plots = FALSE, scale = scaleLPJ, mode = "cf",
                    gridList = "gridlist.txt")

# Activate plot option
LPJsettings$plot.data <- TRUE

# Save plots
LPJsettings$plot.data <- TRUE
LPJsettings$save.plots <- TRUE

# runLPJ and plot
results <- runLPJ(x = mainDur, settings = LPJsettings)

}
