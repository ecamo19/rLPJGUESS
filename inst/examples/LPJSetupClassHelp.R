\dontrun{
# Create a parallel setup for MPI cluster
mySetup <- setupLPJParallel(numCores= 20, clusterType = "MPI", mainDir=mainDir)

# Create a parallel setup for SOCK cluster
mySetup <- setupLPJParallel(numCores= 3, clusterType = "SOCK", mainDir=mainDir)

# Run LPJ-GUESS in parallel
result <-   runLPJ(x = mySetup,  settings= settings, parameterList = parameterList )


# Accessing information
mySetup["clusterType"]
mySetup["numCores"]
mySetup["mainDir"]
mySetup["runDir"]
mySetup["outDir"]

}
