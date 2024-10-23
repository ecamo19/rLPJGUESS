\dontrun{
#  Specify folder where the ASCII outputs produced by LPJ-GUESS files are located
outDir <- "/some/absolute/path/runDirectory/outDirectory"
list.files(outDir)

# Any specific output you are interest in
typeList <- c("aaet","lai")

# What information should be associated to the data
runInfo <-  list(parameters = 0.5, grid = 1)

# Call the function and obtain the outputs as LPJData and
  # stored as zoo time series
LPJout <- getLPJData(x = outDir, typeList = typeList, runInfo = runInfo, processing = TRUE)
  # stored as data.frame
LPJout <- getLPJData(x = outDir, typeList = typeList, runInfo = runInfo, processing = FALSE)

# Plot the data
plotLPJData(LPJout)
  }
