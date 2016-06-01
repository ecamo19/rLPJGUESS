#' @title The function to run the LPJ-GUESS in parallel
#' @description This function reads the setup parallel object and creates a
#'  cluster to which it submits the model wrapper function with its respective
#'  parameters.
#' @param x either a LPJSetup object created with the setupLPJParallel function or a
#'  character string indicating the path to the directory where
#'  all input data and template are located and in which the function will create
#'  the directory structure for the outputs
#' @param parameterList a named list containing the parameters to be calibrated
#' @param typeList a character vector with the outputs to be analyzed.
#' Default value is all outputs
#' @param settings additional parameters \itemize{
#' \item  gridList a character string providing the name of the text file with
#' the grids to be included in the model, e.g gridlist.txt. It must be in the mainDir.
#' Provide only the file name, not the path
#' \item mode a character string indicating whether using cru or cf data
#' \item scale a character string indicating whether the model runs global or
#' for europe
#' \item  mode a character string indicating whether using cru or cf data
#' \item file.co2 a character string providing the absolute path to the C02 input file
#' \item file.cru a character string providing the absolute path to the cru input file
#' \item file.cru.misc a character string providing the absolute path to the cru
#'  misc  input file
#' \item file.ndep a character string providing the absolute path to the nitrogen
#'  deposition input file
#' \item file.temp a character string providing the absolute path to the temperature
#'  input file
#' \item file.prec a character string providing the absolute path to the
#' precipitation input file
#' \item file.insol a character string providing the absolute path to the
#'  insolation input file
#' \item template1  character string providing the general model template,
#'  e.g, global.ins. It must be in the mainDir. Provide only the file name,
#'   not the path. If not provided, package templates will be used
#' \item template2 a character string providing the  "specific" model template,
#'  e.g, global_cf.ins or global_cru.ins. It must be in the mainDir. Provide
#'  only the file name, not the path. If not provided, package templates will be
#'   used
#' \item plot.data  a boolean indicating whether the ouput data will be plotted
#'  (default FALSE)
#' \item save.plots  a boolean indicating whether the plots will be saved (default
#'  FALSE)
#' \item processing a boolean indicating whether output files will be turned into zoo
#'  time series (default FALSE). This is only supported when running the model
#'  for one grid cell. For several grid cells, please set processing to FALSE.
#' \item delete a boolean indicating whether output files should be deleted after
#'  processing (default TRUE). Saved plots will not be deleted
#' \item runID an integer after which the output directory will be named (default empty).
#' If parallel TRUE, ID is ignored and defined by setupLPJParallel
#' }
#' @return a object of class LPJData. The LPJData object will be automatically stored as RData
#' in a folder in the mainDir. The folder will be named as runInfo plus the date in format %Y_%m_%d_%H%M%S.
#' @export
#' @section Warning: When using MPI clusters, please call the function \code{\link{exitMPI}}
#' before terminating your R session.
#' @section Model templates:  The provided templates can be either the ones provided by the package or
#' a self edited templates. The function assumes a specific coding for writing the
#' parameters values. For this reason, we recommend to use the package templates.
#' If using self edited templates, please take the package templates as a reference (\code{\link{getTemplate}})
#' @details The runLPJ in parallel assumes the existence of a folder the model templates
#'  for LPJ-GUESS (optional) and link to the model executable (not for Windows).
#' Running the LPJ parallel involves two steps. First, to create a parallel
#' setup (\code{\link{setupLPJParallel}}), and second, to actually run in parallel the model
#' (\code{\link{runLPJ}}).  The parallelization requires the package \emph{snow} for SOCK clusters or
#' the package \emph{Rmpi} for MPI clusters.
#' @seealso  \url{https://cran.r-project.org/web/packages/Rmpi/Rmpi.pdf},
#'  \url{https://cran.r-project.org/web/packages/snow/snow.pdf},
#'  \code{\link{setupLPJParallel}}, \code{\link{exitMPI}}, \linkS4class{LPJData},
#'  \linkS4class{LPJSetup}
#' @export
#' @keywords Rlpj
#' @author Florian Hartig, Ramiro Silveyra Gonzalez
#' @examples \dontrun{
#'
#' file.co2<-"/some/absolute/path/crudata/co2_1901-2013.txt"
#' file.cru <- "/some/absolute/path/crudata/cru_1901_2006.bin"
#' file.cru.misc <- "/some/absolute/path/crudata/cru_1901_2006misc.bin"
#' file.ndep <- "/some/absolute/path/crudata/GlobalNitrogenDeposition.bin"
#' file.temp <- "/some/absolute/path/cfdata/temp.nc"
#' file.prec <- "/some/absolute/path/cfdata/prec.nc"
#' file.insol <- "/some/absolute/path/cfdata/rad.nc"
#'
#' mainDir <- "/some/absolute/path/mainDir"
#' list.files(mainDir)
#' [1] "guess" or "guesscmd.exe"  # link to the model executable
#' [2] "gridlist.txt"      # list of gridcells
#' [3] "global.ins"        # template1 (optional)
#' [4] "global_cru.ins"    # template2 (optional)
#'
#' # General settings
#' settings <- list (gridList = gridList,mode = "cf", scale = "global",
#'                 file.co2 = file.co2, file.cru = file.cru,
#'                 file.cru.misc = file.cru.misc, file.ndep = file.ndep,
#'                 file.temp = file.temp, file.prec = file.prec,
#'                 file.insol = file.insol, delete = F)
#'
#'
#' # Single  Run

#'  result <-  runLPJ(mainDir, settings= settings)
#'  result
#'      class              : LPJData
#'      LPJ template 1     : global.ins
#'      LPJ template 2     : global_cf.ins
#'      grid cells         : 99  Somewhere
#'      run directory      : /some/absolute/path/mainDir/runDirectory
#'      LPJ model outputs  : 39 outputs
#'      aaet agpp aiso amon anpp cflux clitter cmass cpool cton_leaf dens
#'      firert fpc speciesheight lai maet mevap mgpp mintercep miso mlai mmon
#'      mnee mnpp mpet mra mrh mrunoff mwcont_lower mwcont_upper nflux ngases
#'      nlitter nmass npool nsources nuptake runoff vmaxnlim
#'
#'
#' #  Parallel Run
#' # Create some paramaters to test the model.
#' # Number of runs is proportional to number of parameter being testet
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
#' mySetup  <- setupLPJParallel(3, "SOCK", "cf",
#'                              mainDir = "/some/absolute/path/mainDir")
#'
#' # Call runLPJ
#' result <- runLPJ(mySetup, settings= settings, parameterList = parameterList)
#' str(result,1)
#'    List of 6
#'    $ :Formal class 'LPJData' [package "Rlpj"] with 2 slots
#'    $ :Formal class 'LPJData' [package "Rlpj"] with 2 slots
#'    $ :Formal class 'LPJData' [package "Rlpj"] with 2 slots
#'    $ :Formal class 'LPJData' [package "Rlpj"] with 2 slots
#'    $ :Formal class 'LPJData' [package "Rlpj"] with 2 slots
#'    $ :Formal class 'LPJData' [package "Rlpj"] with 2 slots
#'
#'
#'  }

runLPJ <-  function(x=NULL, typeList=NULL, parameterList=NULL, settings = NULL){

  if (is.null(x)){
    stop("Please provide a valid value for x")

  }else if (class(x) == "character"){
  #----------------------------------------------------------------------------#
  # SERIAL RUNLPJ
  #----------------------------------------------------------------------------#
    if (is.null(settings) || !class(settings) == "list"){
        stop("Invalid settings provided")
    }
    if(!file.exists(x)){
      stop("Invalid main directory")
    }
    if ( is.null(parameterList)){
      cat ("\n\nYou have not provided a parameter list")
      cat ("\nModel will run with default values")
      parameterList <- getParameterList(singleRun$scale)

    }else if(!class(parameterList) == "list"){
      stop("Please provide a valid parameter list")
    }else{

    }

    # do the settings check
    singleRun <- try(createSingleObject(x, typeList, settings), FALSE)
    if ('try-error' %in% class(singleRun)){
      stop("Invalid settings provided")
    }
    singleRun$parameterList <- parameterList



    dir.create(singleRun$runInfoDir, showWarnings = FALSE)
    # Need to create an output folder named after ID
    singleRun$runDir <- file.path(x, paste("runDirectory", singleRun$runID, sep=""))
    singleRun$outDir <- file.path(x, paste("runDirectory", singleRun$runID, sep=""),
                                  paste("outDirectory", singleRun$runID, sep=""))
    dir.create(singleRun$runDir, showWarnings = FALSE)
    dir.create(singleRun$outDir, showWarnings = FALSE)




    #
    singleRun$template1Mem <- readLines(file.path(singleRun$mainDir, singleRun$template1))
    singleRun$template1Mem <- sub("path_to_output/",
                                  paste(singleRun$outDir, "/", sep =""), singleRun$template1Mem)
    for ( j in 1:length(singleRun$typeList)) {
      singleRun$template1Mem <- sub(paste("! file", singleRun$typeList[j], sep="_"),
                                    paste("file",  singleRun$typeList[j], sep="_") , singleRun$template1Mem)
    }
    # template 2: the cru or cf template
    singleRun$template2Mem <- readLines(file.path(singleRun$mainDir,singleRun$template2))
    singleRun$template2Mem <- sub("path_to_globalTemplate",
                                  paste(singleRun$runDir, "/", singleRun$template1, sep=""),
                                  singleRun$template2Mem )
    singleRun$template2Mem  <- sub("path_to_gridlist",
                                   paste(singleRun$runDir,"/", singleRun$gridList, sep=""),
                                   singleRun$template2Mem )
    for ( j in 1:length(singleRun$filesNames)){
      singleRun$template2Mem  <- sub(singleRun$filesNames[[j]][1],
                                     singleRun$filesNames[[j]][2],
                                     singleRun$template2Mem)
    }
    result <- runLPJWrapper(singleRun)
    return(result)

  #----------------------------------------------------------------------------#
  # PARALLEL RUNLPJ
  #----------------------------------------------------------------------------#
  }else if(class(x) == "LPJSetup"){

  if (is.null(settings) || !class(settings) == "list"){
      stop("Invalid settings provided")
    }
  if ( is.null(parameterList) || !class(parameterList) == "list"){
      stop("Please provide a valid parameter list")
    }
  # do the settings check
  singleRun <- try(createSingleObject(x@mainDir, typeList, settings ), FALSE)
  if ('try-error' %in% class(singleRun)){
    stop("Invalid settings provided")
  }
  # setup object has all needed for pallel structure
  # Checking packages availability
  if (!requireNamespace("snow", quietly = TRUE)){
    stop("Can't load required library 'snow', runLPJparallel will now exit")
  }
  if (x@clusterType=="MPI"){
    if (!requireNamespace("Rmpi", quietly = TRUE)){
      stop("Can't load required library 'Rmpi', runLPJparallel will now exit")
    }else{
      # check cluster size
      numCores.available <- Rmpi::mpi.universe.size() - 1
      if ( numCores.available == 0) {
        stop("There are not enough available cores to create a cluster")
      }else if ( numCores.available != x@numCores) {
        message(paste("There are", numCores.available,"cores available ", sep = " "))
        message(paste("You requested", x@umCores,  "cores", sep = " "))
        message("The number of cores will be set to meet the available resources")
        x@numCores <- numCores.available
      }
    }
  }
  # Check cores with runs
  if (length(parameterList) < x@numCores){
    stop("The number of cores requested exceeds the number of runs")
  }
  # Need to create an output folder named after ID
  dir.create(singleRun$runInfoDir, showWarnings = FALSE)
    # READ SETUP AND CREATE THE RUNPARAMETER
  #----------------------------------------------------------------------------#
  cat("\n\nReading the parallel object structure")
  # Creating list that will hold data. It is faster to first create objects,
  # and then fill them with values, instead of grow then withing a loop.
  runDir <- vector("character", length(parameterList))
  outDir <- vector("character", length(parameterList))
  # the actual list that will hold the information need for all runs
  runParameters <- rep(list(), length(parameterList))
  for (i in 1:x@numCores) {
    for (index in seq(i, length(parameterList), x@numCores )){
      runDir[index] <- x@runDir[i]
      outDir[index] <- x@outDir[i]
    }
  }
  cat("\nCreating the single run objects")#single run objects
  progessBar <- txtProgressBar(min = 0, max = length(parameterList), style = 3)
  for (i in 1:length(parameterList)){
    setTxtProgressBar(progessBar, i)
    singleRun$runID <- i
    singleRun$runDir <- runDir[i]
    singleRun$outDir <- outDir[i]
    singleRun$parameterList <- parameterList[[i]]
    #
    singleRun$template1Mem <- readLines(file.path(singleRun$mainDir, singleRun$template1))
    singleRun$template1Mem <- sub("path_to_output/",
                                  paste(singleRun$outDir, "/", sep =""), singleRun$template1Mem)
    for ( j in 1:length(singleRun$typeList)) {
      singleRun$template1Mem <- sub(paste("! file", singleRun$typeList[j], sep="_"),
                                 paste("file",  singleRun$typeList[j], sep="_") , singleRun$template1Mem)
    }
    # template 2: the cru or cf template
    singleRun$template2Mem<- readLines(file.path(singleRun$mainDir,singleRun$template2))
    singleRun$template2Mem <- sub("path_to_globalTemplate",
                                  paste(singleRun$runDir, "/", singleRun$template1, sep=""),
                                  singleRun$template2Mem )
    singleRun$template2Mem  <- sub("path_to_gridlist",
                                   paste(singleRun$runDir,"/", singleRun$gridList, sep=""),
                                   singleRun$template2Mem )
    for ( j in 1:length(singleRun$filesNames)){
      singleRun$template2Mem  <- sub(singleRun$filesNames[[j]][1],
                                     singleRun$filesNames[[j]][2],
                                     singleRun$template2Mem)
    }
    runParameters[[i]] <- singleRun
  }
  close(progessBar)
  # SOCK CLUSTER
  #----------------------------------------------------------------------------#
  # Initialisation of snowfall.
  # Create cluster
  if (x@clusterType =="SOCK"){
    cat( paste ("\nCreating a", x@clusterType, "cluster with",
                x@numCores, " cores", sep = " " ))
    cl <-  snow::makeSOCKcluster(x@numCores)
    # Exporting needed data and loading required
    # packages on workers. --> If daa is loaded firs it can be exporte to all workers
    snow::clusterEvalQ(cl, library(Rlpj))
    snow::clusterEvalQ(cl, "runParameters")
    # Distribute calculation: will return values as a list object
    cat ("\nSending tasks to the cores\n")
    result <- snow::clusterMap(cl, runLPJWrapper,  runParameters )
    #result <- snow::clusterApply(cl, runParameters, runLPJWrapper )
    # Destroy cluster
    snow::stopCluster(cl)
    # deliver data to clusters
    # Snow's close command, shuts down and quits from script
    # MPI CLUSTER
    #----------------------------------------------------------------------------#
  }else if (x@clusterType =="MPI"){
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
        cat( paste ("\nCreating a", x@clusterType, "cluster with",
                    x@numCores, "cores", sep = " " ))
        cat("\nPlease call exit_mpi at the end of you script")
        Rmpi::mpi.spawn.Rslaves(nslaves = x@numCores, needlog = FALSE)
      }else{
        cat(paste("\nUsing the existing", x@clusterType, "cluster with",
                  x@numCores, " cores", sep = " " ))
      }
    }
    cores <- rep(x@numCores, x@numCores)
    Rmpi::mpi.bcast.Robj2slave(cores)
    Rmpi::mpi.bcast.Robj2slave(runParameters)
    Rmpi::mpi.bcast.cmd(library(Rlpj))
    result <- Rmpi::mpi.parSapply(cores, MPISapply, runParameters = runParameters)
  }
  # END
  #----------------------------------------------------------------------------#
  return(unlist(result))
  }else{
    stop("Please provide a valid value for x")
  }
}


