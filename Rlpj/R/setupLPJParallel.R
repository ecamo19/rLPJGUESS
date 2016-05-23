#' @title The function to create a setup for parallel runs of the LPJ-GUESS
#'
#' @description This function first creates a setup for running the LPJ in parallel
#'  based on the provided input parameters. The function assumes a specific initial
#'  configuration. A folder (mainDir) containing the input data, the templates,
#'  and link to the model must exist. A directory structure for the outputs will
#'  be build wihtin this folder by the function.
#' @param numCores  a integer specifying number of cores of the cluster
#' @param clusterType a character string indicating the type of cluster to be created. If running in bwHPC,
#'  type must be "MPI". If running in personal computer, type must be "SOCK".
#' @param mainDir a character string indicating the path to the directory where
#'  all input data and template are located and in which the function will create
#'  the directory structure for the outputs
#' @return a setup object or named list containing the setup parameters to run
#' the LPJ in parallel
#' @seealso  \url{https://cran.r-project.org/web/packages/Rmpi/Rmpi.pdf},
#'  \url{https://cran.r-project.org/web/packages/snow/snow.pdf}
#' @export
#' @keywords Rlpj
#' @author Florian Hartig, Ramiro Silveyra Gonzalez, Maurizio Bagnara
#' @examples \dontrun{
#' mainDir <- "/some/absolute/path/mainDir"
#' list.files(mainDir)
#' [1] "guess" or "guesscmd.exe"  # link to the model executable
#' [2] "gridlist.txt"      # list of gridcells
#' [3] "global.ins"        # template1 (optional)
#' [4] "global_cru.ins"    # template2 (optional)
#'
#' mySetup <- setupLPJParallel(numCores= 3, clusterType = "SOCK", mainDir=mainDir)
#'    class              : LPJSetup
#'    cluster type       : SOCK
#'    number of cores    : 3
#'    output directories :
#'    /home/trashtos/GitHub/lpjRun/runDirectory1
#'    /home/trashtos/GitHub/lpjRun/runDirectory2
#'    /home/trashtos/GitHub/lpjRun/runDirectory3
#'}
setupLPJParallel <- function(numCores=NULL, clusterType = NULL, mainDir=NULL)
  {
  #----------------------------------------------------------------------------#
  # CHECK INPUTS AND EXIT IF ANY ERROR
  #----------------------------------------------------------------------------#
  # number of cores
  if (is.null(numCores) | (typeof(numCores) != "double") ){
    stop( "Please provide a valid number of cores.")
  }
  # cluster type
  if (is.null(clusterType) || clusterType != "SOCK" & clusterType != "MPI" ){
    stop("Please provide a valid cluster type: SOCK or MPI.")
  }
  if (clusterType == "MPI"){
    if (!requireNamespace("Rmpi", quietly = TRUE)){
      stop("Can't load required library 'Rmpi', runLPJparallel will now exit.")
    }else{
      # check cluster size
      numCores.available <- Rmpi::mpi.universe.size() - 1
      if ( numCores.available == 0) {
        stop("\nThere are not enough available cores  to create a cluster.")
      }else if ( numCores.available != numCores) {
        message(paste("There are", numCores.available,"cores available. ", sep = " "))
        message(paste("You requested", numCores,  "cores.", sep = " "))
        message("The number of cores will be set to meet the available resources.")
        numCores <- numCores.available
      }
    }
  }
  # mainDir
  if (is.null(mainDir) || !file.exists(mainDir)){
    stop("Please provide a valid main directory.")
  }
  #----------------------------------------------------------------------------#
  # CREATE THE FOLDER STRUCTURE
  #----------------------------------------------------------------------------#
  #cat("\n\nCreating the foder structure")
  runDir <-  vector("character", length(numCores))
  outDir <-  vector("character", length(numCores))
  #progessBar <- txtProgressBar(min = 0, max = length(numCores), style = 3)
  for (i in 1:numCores) {
    #setTxtProgressBar(progessBar, i)
    runDir[i] <- file.path(mainDir, paste("runDirectory",i,sep=""))
    outDir[i] <- file.path(mainDir, paste("runDirectory",i,sep=""), paste("outDirectory",i,sep=""))
    dir.create(file.path(mainDir, paste("runDirectory",i,sep="")), showWarnings = FALSE)
    dir.create(file.path(mainDir, paste("runDirectory",i,sep=""), paste("outDirectory",i,sep="")), showWarnings = FALSE)
  }
  #close(progessBar)
  #----------------------------------------------------------------------------#
  # END
  #----------------------------------------------------------------------------#

  setupObject <- new(Class="LPJSetup",
                clusterType = clusterType,
                numCores= numCores,
                mainDir = mainDir,
                runDir = runDir,
                outDir = outDir)

  return (setupObject)
}
