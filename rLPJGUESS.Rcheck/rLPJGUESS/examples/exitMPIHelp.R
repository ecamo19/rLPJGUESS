\dontrun{
##### MPI CLUSTER ######
# Create a parallel setup for MPI cluster
mySetup <- setupLPJParallel(numCores= 20, clusterType = "MPI", mainDir=mainDir)
mySetup
# 20 different parameters to test (the number must be larger than the number of cores)
  # as a matrix
parameterList <- as.matrix(seq(1,5, len = 20))
colnames(parameterList) <- "common_emax"
  # as a list
dummy <- list (common_emax = NULL)
par <- seq(1,5, len = 20)
parameterList <- vector("list", length(par))
for (i in 1:length(par)) {
  dummy$common_emax <- par[i]
  parameterList[[i]] <- dummy
}

# Run the model
result <-   runLPJ(mySetup,  settings= settings, parameterList = parameterList )

# At the end of the script call exitMPI()
exitMPI()
}
