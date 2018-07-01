\dontrun{
# Select the model scale
scaleLPJ <- "europe"

# Create the general settings
LPJsettings <- list(file.co2 = file.path(mainDir, "crudata", "co2_1765-2500_RCP3.txt"),
                    file.cru = file.path(mainDir, "crudata", "cru_1901_2006.bin"),
                    file.cru.misc = file.path(mainDir, "crudata", "cru_1901_2006misc.bin"),
                    file.ndep = file.path(mainDir, "crudata", "GlobalNitrogenDeposition.bin"),
                    file.temp = file.path(mainDir, "temp.nc"),
                    file.prec = file.path(mainDir, "prec.nc"),
                    file.insol = file.path(mainDir, "rad.nc"),
                    variable.temp = "temp", variable.insol = "rad",
                    variable.prec = "prec", delete = FALSE, save = FALSE, processing = TRUE,
                    plot.data = FALSE, save.plots = FALSE, scale = scaleLPJ, mode = "cf",
                    gridList = "gridlist.txt")

# Define the design for the simulation:  number of patches simulated,
# disturbances, and other simulation options

  # Obtain the standard design
designLPJ <- getDesign(scaleLPJ, list = TRUE)

  # Modify the desired options
designLPJ$run_vegmode <- "cohort"
designLPJ$run_ifcentury <- 0
designLPJ$run_iffire <- 0
designLPJ$run_ifnlim <- 0
designLPJ$run_ifstochestab <- 0
designLPJ$run_ifstochmort <- 0
designLPJ$run_patcharea <- 25^2
designLPJ$run_npatch <- 1
designLPJ$run_ifdisturb <- 0
designLPJ$run_nyear_spinup <- 1
designLPJ$run_freenyears <- 0
designLPJ$run_save_state <- 0
designLPJ$run_restart <- 0
designLPJ$run_state_path <- mainDir

# Add the desing to the settings
LPJsettings$design <- designLPJ

# Run the model
results_SPP <- runLPJ(x = mainDur, parameterList = parameters,
                      typeList = typeList, settings = LPJsettings)

}
