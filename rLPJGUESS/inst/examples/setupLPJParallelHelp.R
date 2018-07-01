\dontrun{
# 20 different parameters or parameters conbinations to test
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


# Create a parallel setup for MPI cluster
# Number of cores must be equal or larger than the parameter combinations
mySetup <- setupLPJParallel(numCores= 20, clusterType = "MPI", mainDir=mainDir)

# Create a parallel setup for SOCK cluster
# Number of cores must be equal or larger than the parameter combinations
mySetup <- setupLPJParallel(numCores= 3, clusterType = "SOCK", mainDir=mainDir)

# Run LPJ-GUESS in parallel
result <- runLPJ(x = mySetup,  settings= settings, parameterList = parameterList )

# After running runLPJ (20 simulations)
str(result,1)

  List of 20

  $ :Formal class 'LPJData' [package "rLPJGUESS"] with 2 slots

  $ :Formal class 'LPJData' [package "rLPJGUESS"] with 2 slots

  [...]



}
