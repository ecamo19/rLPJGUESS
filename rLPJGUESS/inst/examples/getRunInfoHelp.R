\dontrun{
# recover the data from the runInfo folder
result <- getRunInfo("/some/absolute/path/runInfo_2016_08_11_121507")

# recover only the paramaters from the runInfo folder
parameters <- getRunInfo("/some/absolute/path/runInfo_2016_08_11_121507", parameters =T)

# recover only the parameters from the result
parameters <- getRunInfo(result, parameters =T)
}
