#' @title The function to run the LPJ in parallel
#' @description This function reads the setup parallel object and creates a
#'  cluster to which it submits the model wrapper function with its respective
#'  parameters.
#' @param setupObject a named list created by the setupLPJParallel function containing
#' the following: mainDir, numCores, clusterType, runDir, outDir
#' @param singleRun a named list created by the setupParallel function containing
#' the following: mainDir, template1, template2, gridList, clusterType, numCores,
#'  typeList, mode and scale.
#' @param parameterList a named list containing the parameters to be calibrated
#' @return a list holding the outputs of the runLPJwrapper (see function help)
#' @details The runLPJparallel assumes the existence of a folder containing all
#' the inputs data and templates for LPJ-GUESS and a directory structure for
#' storing inputs and outputs of each single run. The setupParallel function is
#' ought to be run before calling the runLPJparallel.
#' Running the LPJ parallel involves two steps. First, to create a parallel
#' setup (setupLPJParallel function), and second, to actually run in parallel the model
#' (runLPJparallel function).  The parallelization requires the packages snow and
#'  also the Rmpi package, if you aim at using a MPI cluster.
#' @seealso  \url{https://cran.r-project.org/web/packages/Rmpi/Rmpi.pdf},
#'  \url{https://cran.r-project.org/web/packages/snow/snow.pdf}
#' @keywords Rlpj
#' @author Florian Hartig, Ramiro Silveyra Gonzalez
runLPJParallel <- function(setupObject = NULL, singleRun = NULL, parameterList = NULL)
  {
  #----------------------------------------------------------------------------#
  # CHECK INPUTS AND EXIT IF ANY ERROR
  #----------------------------------------------------------------------------#
  # Checking conditions before start processing
  cat("\nChecking conditions")
  if ( is.null(setupObject)){
    stop("Please provide a valid setupObject")
  }
  if ( is.null(singleRun)){
    stop("Please provide a valid singleRun")
  }
  if ( is.null(parameterList)){
    stop("Please provide a valid parameterList")
  }
  if ( is.null(setupObject[["clusterType"]])){
    stop("Please provide a valid cluster type: MPI or SOCK")
  }
  if ( is.null(setupObject[["numCores"]])){
    stop("Please provide a valid number of cores.")
  }
  # Checking packages availability
  if (!requireNamespace("snow", quietly = TRUE)){
    stop("Can't load required library 'snow', runLPJparallel will now exit.")
  }
  if (setupObject$clusterType=="MPI"){
    if (!requireNamespace("Rmpi", quietly = TRUE)){
      stop("Can't load required library 'Rmpi', runLPJparallel will now exit.")
    }else{
      # check cluster size
      numCores.available <- Rmpi::mpi.universe.size() - 1
      if ( numCores.available == 0) {
        stop("There are not enough available cores to create a cluster")
      }else if ( numCores.available != setupObject$numCores) {
        message(paste("There are", numCores.available,"cores available ", sep = " "))
        message(paste("You requested", setupObject$numCores,  "cores", sep = " "))
        message("The number of cores will be set to meet the available resources")
        setupObject$numCores <- numCores.available
      }
    }
  }
  # Check cores with runs
  if (length(parameterList) < setupObject$numCores){
    stop("The number of cores requested exceeds the number of runs")
  }
  #----------------------------------------------------------------------------#
  # READ SETUP AND CREATE THE RUN PARAMETER LIST
  #----------------------------------------------------------------------------#
  cat("\n\nReading the parallel object structure")
  # Creating list that will hold data. It is faster to first create objects,
  # and then fill them with values, instead of grow then withing a loop.
  runDir <- vector("character", length(parameterList))
  outDir <- vector("character", length(parameterList))
  # the actual list that will hold the information need for all runs
  runParameters <- rep(list(), length(parameterList))
  for (i in 1:setupObject[["numCores"]]) {
    for (index in seq(i, length(parameterList), setupObject[["numCores"]] )){
      runDir[index] <- setupObject[["runDir"]][i]
      outDir[index] <- setupObject[["outDir"]][i]
    }
  }
  cat("\nCreating the single run objects")#single run objects
  progessBar <- txtProgressBar(min = 0, max = length(parameterList), style = 3)
  for (i in 1:length(parameterList)){
    setTxtProgressBar(progessBar, i)
    singleRun <- fillSingleObject(singleRun, runDir[i], outDir[i],parameterList[[i]],
                                  ID = i )
    runParameters[[i]] <- singleRun
  }
  close(progessBar)
  #----------------------------------------------------------------------------#
  # SOCK CLUSTER
  #----------------------------------------------------------------------------#
  # Initialisation of snowfall.
  # Create cluster
  if (setupObject$clusterType =="SOCK"){
    cat( paste ("\nCreating a", setupObject$clusterType, "cluster with",
                setupObject$numCores, "cores", sep = " " ))
    cl <-  snow::makeSOCKcluster(setupObject$numCores)
    # Exporting needed data and loading required
    # packages on workers. --> If daa is loaded firs it can be exporte to all workers
    snow::clusterEvalQ(cl, library(Rlpj))
    snow::clusterEvalQ(cl, "runParameters")
    # Distribute calculation: will return values as a list object
    cat ("\nSending tasks to the cores\n")
    result <- snow::clusterMap(cl, runLPJWrapper,  runParameters )
    #result <- snow::clusterApply(cl, runParameters, runLPJwrapper )
    # Destroy cluster
    snow::stopCluster(cl)
    # deliver data to clusters
    # Snow's close command, shuts down and quits from script
  #----------------------------------------------------------------------------#
  # MPI CLUSTER
  #----------------------------------------------------------------------------#
  }else if (setupObject$clusterType =="MPI"){
    # Use Rmpi to spawn and close the slaves
    # Broadcast the data to the slaves and
    # Using own MPISapply with mpi.parsSapply. mpi.parSapply takes a list
    # "cores", so that there is one task for each core.
    # Then each core is aware of how many task he has to carry and applies
    # MPISapply on its tassk. Result is a list of list, thus, it must be
    # unlisted
    # needlog avoids fork call
    if(is.loaded ("mpi_initialize")){
      if (Rmpi::mpi.comm.size() < 1 ){
        cat( paste ("\nCreating a", setupObject$clusterType, "cluster with",
                    setupObject$numCores, "cores", sep = " " ))
        cat("\nPlease call exit_mpi at the end of you script")
        Rmpi::mpi.spawn.Rslaves(nslaves = setupObject$numCores, needlog = FALSE)
      }else{
        cat(paste("\nUsing the existing", setupObject$clusterType, "cluster with",
                    setupObject$numCores, "cores", sep = " " ))
      }
    }
    cores <- rep(setupObject$numCores, setupObject$numCores)
    Rmpi::mpi.bcast.Robj2slave(cores)
    Rmpi::mpi.bcast.Robj2slave(runParameters)
    Rmpi::mpi.bcast.cmd(library(Rlpj))
    result <- Rmpi::mpi.parSapply(cores, MPISapply, runParameters = runParameters)
  }
  #----------------------------------------------------------------------------#
  # END
  #----------------------------------------------------------------------------#
  return(unlist(result))
}


#' @title The function to run the LPJ in parallel
#' @description This function tells each core which are its tasks and makes the
#' core run the runLPJwrapper on them using sapply
#' @param numcores a integer specifying number of cores of the cluster
#' @param runParameters a list of lists, each list containing the following information:
#' mainDir, template1, template2, gridList, runDir, outDir, mode, scale,
#'  typeList, parameterList, runID and gridFilename
#' @keywords Rlpj
#' @author Ramiro Silveyra Gonzalez
#' @note based on lapplys from M. T. Morgan (mtmorgan@fhcrc.org) (Parallel R)
#' @examples \dontrun{#'
#' result <- MPISapply(numcores = 6, runParameters = runParameters)
#' }
MPISapply <- function(numcores, runParameters) {
  rank <- Rmpi::mpi.comm.rank()
  # master doesnt work the data
  if (rank > 0){
    mywork <- runParameters[seq(rank, length(runParameters), numcores)]
    result <- sapply(mywork, runLPJWrapper)
    return(result)
  }
}
