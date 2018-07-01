\dontrun{
# Define the parametes to be written
parameterList <- list(run_lamda_max = 0.5, run_emax= 5)

# write the template
writeTemplate(template1 = "global.ins", parameterList = parameterList,
              runDir = "/some/absolute/path/mainDir/runDirectory", check = "serial")
}
