#' @title The function to run the LPJ in parallel
#' @description This function reads the setup parallel object and creates a
#'  cluster to which it submits the model wrapper function with its respective
#'  parameters.
#' @param setupObject a named list created by the setupParallel function containing
#' the following: mainDir, template1, template2, gridList, clusterType, numCores,
#'  typeList, mode and scale.
#' @param parameterList a named list containing the parameters to be calibrated
#' @param file.co2 a character string providing the absolute path to the C02 input file
#' @param file.cru a character string providing the absolute path to the cur? input file
#' @param file.cru.misc a character string providing the absolute path to the cru?
#'  input file
#' @param file.ndep a character string providing the absolute path to the nitrogen
#'  deposition input file
#' @param file.temp a character string providing the absolute path to the temperature
#'  input file
#' @param file.prec a character string providing the absolute path to the
#' precipitation input file
#' @param file.insol a character string providing the absolute path to the
#'  insolation input file
#' @param plot.data  a boolean indicating whether the ouput data will be plotted.
#' @param save.plots  a boolean indicating whether the plots will be saved.
#' @return a list holding the outputs of the runLPJwrapper (see function help)
#' @details The runLPJparallel assumes the existence of a folder containing all
#' the inputs data and templates for LPJ-GUESS and a directory structure for
#' storing inputs and outputs of each single run. The setupParallel function is
#' ought to be run before calling the runLPJparallel.
#' Running the LPJ parallel involves two steps. First, to create a parallel
#' setup (setupParallel function), and second, to actually run in parallel the model
#' (runLPJparallel function).  The parallelization requires the packages snow and
#'  also the Rmpi package, if you aim at using a MPI cluster.
#' @seealso  \url{https://cran.r-project.org/web/packages/Rmpi/Rmpi.pdf},
#'  \url{https://cran.r-project.org/web/packages/snow/snow.pdf}
#' @export
#' @keywords Rlpj
#' @author Florian Hartig, Ramiro Silveyra Gonzalez
#'
#' @examples \dontrun{
#' # You need to specify the absolute path of each input file:
#' file.co2<-"/some/absolute/path/crudata/co2_1901-2013_FAKE.txt"
#' file.cru <- "/some/absolute/path/crudata/cru_1901_2006.bin"
#' file.cru.misc <- "/some/absolute/path/crudata/cru_1901_2006misc.bin"
#' file.ndep <- "/some/absolute/path/crudata/GlobalNitrogenDeposition.bin"
#'
#' # If you are using the global_cf.ins file you need to specify the site
#' # specific input files as well
#' file.temp <- "/some/absolute/path/cfdata/temp.nc"
#' file.prec <- "/some/absolute/path/cfdata/prec.nc"
#' file.insol <- "/some/absolute/path/cfdata/rad.nc"
#'
#' # Create some paramaters to test modell.
#' # Number of runs is proportional to number of parameter set you are testing
#' parameterDefault <- list (run_emax = NULL)
#'
#' # Test 6 different values for emax.
#' par <- seq(1,5, len = 6)
#' # Create the list object with the parameters
#' parameterList <- vector("list", length(par))
#' for (i in 1:length(par)) {
#'    parameterDefault$run_emax <- par[i]
#'    parameterList[[i]] <- parameterDefault
#'  }
#'
#' # Call setupParallel
#' setupObject  <- setupParallel(3, "SOCK", "global", "cf", gridList = "gridlist_geb.txt",
#'                              mainDir = "/some/absolute/path/mainDir")
#'
#' # Call the runLPjParallel
#' result <- runLPJParallel(setupObject , plot.data = FALSE, save.plots = FALSE,
#'                          parameterList=parameterList, file.co2, file.cru,
#'                          file.cru.misc, file.ndep, file.temp, file.prec, file.insol)
#'
#'    Checking conditions
#'    Reading the parallel object structure
#'    Creating the single run objects....1....2....3....4....5....6
#'    Creating a SOCK cluster with 3 cores
#'    Sending tasks to the cores
#'
#'    Processing ended!
#'
#' str(result,1)
#'    List of 6
#'    $ :Formal class 'LPJData' [package "Rlpj"] with 2 slots
#'    $ :Formal class 'LPJData' [package "Rlpj"] with 2 slots
#'    $ :Formal class 'LPJData' [package "Rlpj"] with 2 slots
#'    $ :Formal class 'LPJData' [package "Rlpj"] with 2 slots
#'    $ :Formal class 'LPJData' [package "Rlpj"] with 2 slots
#'    $ :Formal class 'LPJData' [package "Rlpj"] with 2 slots
#'
#' str(result[[1]], 2 )
#'    Formal class 'LPJData' [package "Rlpj"] with 2 slots
#'    ..@ runInfo  :List of 15
#'    ..@ dataTypes:List of 39
#'
#'
#'  }
runLPJParallel <- function(setupObject, plot.data = FALSE, save.plots = FALSE,
                           parameterList=NULL, file.co2 = NULL, file.cru = NULL,
                           file.cru.misc = NULL, file.ndep= NULL,
                           file.temp = NULL, file.prec = NULL, file.insol = NULL){
  # Checking conditions before start processing
  cat("\nChecking conditions")
  if ( is.null(setupObject[["clusterType"]])){
    stop("Please provide a valid cluster type: MPI or SOCK")
  }
  if ( is.null(setupObject[["numCores"]]) == TRUE){
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
  # create a runinfo dir
  runInfoDir <- file.path(setupObject$mainDir, paste("runInfo",
                                         format(Sys.time(), "%Y_%m_%d_%H%M%S"),
                                         sep = "_"))
  dir.create(runInfoDir, showWarnings = FALSE)
  # Pack up all files that user should have provided
  # Get the default list from internal data , that contains the characters stings
  # to replace in the template
  # Go throught the files and check whether they provided, if so add them to
  # default list, otherwise stop the function
  files <- list(file.co2 = file.co2, file.cru = file.cru,
                file.cru.misc = file.cru.misc, file.ndep = file.ndep,
                file.temp = file.temp, file.prec = file.prec,
                file.insol = file.insol)
  files.default <-   files.parameters[[setupObject$mode]]
  files.names <- names(files.default)
  for (i in 1:length(files.names)){
    if (is.null(files[[files.names[i]]])){
      stop(paste("The", files.names[i], "has not been provided", sep = " "))
    }else if(!file.exists(files[[files.names[i]]])){
      stop(paste("The", files.names[i], "does not exist", sep = " "))
    }else{
      files.default[[files.names[i]]][2] <- files[[files.names[i]]]
    }
  }
  cat("\nReading the parallel object structure")
  # Creating list that will hold data. It is faster to first create objects,
  # and then fill them with values, instead of grow then withing a loop.
  runDir <- vector("character", length(parameterList))
  outDir <- vector("character", length(parameterList))
  # dummy for passing to runParameters: parameters for a single run
  singleRun <- list (mainDir = setupObject$mainDir,  template1 = NULL, template2 = NULL,
                     gridList= NULL, runDir = NULL, outDir = NULL, mode = setupObject$mode,
                     scale = setupObject$scale,  typeList = setupObject$typeList, parameterList=NULL,
                     runID = NULL, gridFilename = NULL, runInfoDir =  runInfoDir)
  # the actual list that will hold the information need for all runs
  runParameters <- rep(list(), length(parameterList))
  for (i in 1:setupObject[["numCores"]]) {
    for (index in seq(i, length(parameterList), setupObject[["numCores"]] )){
      runDir[index] <- file.path(singleRun$mainDir, paste("runDirectory",i,sep=""))
      outDir[index] <- file.path(singleRun$mainDir, paste("runDirectory",i,sep=""), paste("outDirectory",i,sep=""))
    }
  }
  cat("\nCreating the single run objects")#single run objects
  for (i in 1:length(parameterList)){
    cat(paste("....", i, sep = ""))
    singleRun$runID <- i
    singleRun$parameterList <- parameterList[[i]]
    singleRun$runDir <- runDir[i]
    singleRun$outDir <- outDir[i]
    #Put templates in the folders
    # template 1: global template
    template1 <- readLines(file.path(singleRun$mainDir, setupObject$template1))
    template1 <- sub("path_to_output/", paste(singleRun$outDir, "/", sep =""),template1)
    for ( j in 1:length(singleRun$typeList)) {
      template1 <- sub(paste("! file", singleRun$typeList[j], sep="_"), paste("file",  singleRun$typeList[j], sep="_") , template1)
    }
    singleRun$template1 <- template1
    # template 2: the cru or cf template
    template2 <- readLines(file.path(singleRun$mainDir,setupObject$template2))
    template2 <- sub("path_to_globalTemplate", paste(singleRun$runDir, "/", setupObject$template1, sep=""), template2)
    template2 <- sub("path_to_gridlist",paste(singleRun$runDir,"/", setupObject$gridList, sep=""),template2)
    for ( j in 1:length(files.names)){
      template2 <- gsub(files.default[[j]][1],  files.default[[j]][2], template2)
    }
    singleRun$template2 <- template2
    # grid file
    grid <- readLines(file.path(singleRun$mainDir,setupObject$gridList))
    singleRun$gridList <- grid
    singleRun$gridFilename <- setupObject$gridList
    # add to run parameters (list for parallel)
    runParameters[[i]] <- singleRun
  }
  # If packages are available, start the parallel configuration and model run
  # Prepare list if plots
  if (plot.data){
    if (save.plots){
      for (i in 1:length(runParameters)){
        runParameters[[i]]$plot.data <- TRUE
        runParameters[[i]]$save.plots <- TRUE
      }
    }else if (!save.plots){
      for (i in 1:length(runParameters)){
        runParameters[[i]]$plot.data <- TRUE
        message("\nYou are plotting data but plots are not being saved")
      }
    }
  }
  # Initialisation of snowfall.
  # Create cluster
  cat( paste ("\nCreating a", setupObject$clusterType, "cluster with",
             setupObject$numCores, "cores", sep = " " ))
  if (setupObject$clusterType =="SOCK"){
    cl <-  snow::makeSOCKcluster(setupObject$numCores)
    # Exporting needed data and loading required
    # packages on workers. --> If daa is loaded firs it can be exporte to all workers
    snow::clusterEvalQ(cl, library(Rlpj))
    snow::clusterEvalQ(cl, "runParameters")
    # Distribute calculation: will return values as a list object
    cat ("\nSending tasks to the cores\n")
    #result <- snow::clusterMap(cl, runLPJwrapper,  runParameters )
    result <- snow::clusterApply(cl, runParameters, runLPJwrapper  )
    # Destroy cluster
    snow::stopCluster(cl)
    # deliver data to clusters
    # Snow's close command, shuts down and quits from script

  }else if (setupObject$clusterType =="MPI"){
    # Use Rmpi to spawn and close the slaves
    # Broadcast the data to the slaves and
    # Using own MPISapply with mpi.parsSapply. mpi.parSapply takes a list
    # "cores", so that there is one task for each core.
    # Then each core is aware of how many task he has to carry and applies
    # MPISapply on its tassk. Result is a list of list, thus, it must be
    # unlisted
    # needlog avoids fork call
    if(is.loaded ("mpi_initialize") == TRUE){
      if (Rmpi::mpi.comm.size() < 1 ){
        cat("\nPlease call exit_mpi at the end of you script")
        Rmpi::mpi.spawn.Rslaves(nslaves = setupObject$numCores, needlog = FALSE)
      }
    }
    cores <- rep(setupObject$numCores, setupObject$numCores)
    Rmpi::mpi.bcast.Robj2slave(cores)
    Rmpi::mpi.bcast.Robj2slave(runParameters)
    Rmpi::mpi.bcast.cmd(library(Rlpj))
    result <- Rmpi::mpi.parSapply(cores, MPISapply, runParameters = runParameters)
    #Rmpi::mpi.close.Rslaves(dellog = FALSE)
    #Rmpi::mpi.finalize() # Dont need to specify type
  }
  cat("\nProcessing ended!")
  return(unlist(result))
}

#' @title The function to run the LPJ in parallel
#' @description This function tells each core which are its tasks and makes the
#' core run the runLPJwrapper on them using sapply
#' @param numcores a integer specifying number of cores of the cluster
#' @param runParameters a list of lists, each list containing the following information:
#' mainDir, template1, template2, gridList, runDir, outDir, mode, scale,
#'  typeList, parameterList, runID and gridFilename
#' @export
#' @keywords Rlpj
#' @author Ramiro Silveyra Gonzalez
#' @note based on lapplys from M. T. Morgan (mtmorgan@fhcrc.org) (Parallel R)
#' @examples \dontrun{#'
#' result <- MPISapply(numcores = 6, runParameters = runParameters)
#' }
#'
#'
MPISapply <- function(numcores, runParameters) {
  rank <- Rmpi::mpi.comm.rank()
  # master doesnt work the data
  if (rank > 0){
    mywork <- runParameters[seq(rank, length(runParameters), numcores)]
    result <- sapply(mywork, runLPJwrapper)
    return(result)
  }
}
