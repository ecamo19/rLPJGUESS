\dontrun{
# Get the default parameters as a list
parameterList <- getParameterList("global", list = TRUE)
parameterList <- getParameterList("europe", list = TRUE)

# Modify any parameter
parameterList$common_emax <- 1
parameterList[["common_emax"]] <- 1


# Get the default parameters as a matrix
parameterList <- getParameterList("global", list = FALSE)
parameterList <- getParameterList("europe", list = FALSE)

# Modify any parameter
parameterList["common_emax",] <- 1

# Call runLPJ with the desired parameter
result <-  runLPJ(x=mainDir, settings=settings, parameterList = parameterList)

}
