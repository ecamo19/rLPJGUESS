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
#' @param scale a character string indicating whether the model runs global or
#' for europe.
#' @param mode a character string indicating whether using cru or cf data
#' @param template1 a character string providing the general model template,
#'  e.g, global.ins. Provide only the file name, not the path.
#'  If not provided, package templates will be used.
#' @param template2 a character string providing the  "specific" model template,
#'  e.g, global_cf.ins or global_cru.ins. Provide only the file name, not the path.
#'  If not provided, package templates will be used.
#' @param gridList  a character string providing the name of the text file with
#' the grids to be included in the model, e.g gridlist.txt.
#' Provide only the file name, not the path.
#' @param mainDir a character string indicating the path to the directory where
#'  all input data and template are located and in which the function will create
#'  the directory structure for the outputs
#' @param typeList  a character vector with the outputs to be analyzed.
#' Default value is all outputs.
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
#' setupObject <- setupParallel(numCores= 10, clusterType = "SOCK",
#'                              gridList= "gridlist.txt", mainDir=mainDir,typeList =NULL,
#'                              scale = "europe", mode = "cf")
#'
#'      Output type list has not been provided.
#'      Setting type list to default values.
#'
#'      Using package template (template 1).
#'      Saving package template in the mainDir.
#'
#'      Using package template (template 2).
#'      Saving package template in the mainDir.
#'
#'      Creating the foder structure....1....2....3....4....5....6....7....8....9....10
#'
#'str(setupObject)
#'      List of 9
#'      $ mainDir    : chr "/some/absolute/path/mainDir"
#'      $ template1  : chr "europe.ins"
#'      $ template2  : chr "europe_cf.ins"
#'      $ gridList   : chr "gridlist.txt"
#'      $ clusterType: chr "SOCK"
#'      $ numCores   : num 10
#'      $ typeList   : chr [1:39] "aaet" "agpp" "aiso" "amon" ...
#'      $ mode       : chr "cf"
#'      $ scale      : chr "europe"
#'}
setupParallel <- function(numCores=NULL, clusterType = NULL,  scale = NULL, mode = NULL , template1 = NULL, template2=NULL,
                          gridList= NULL, mainDir=NULL, typeList = NULL){

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
  # mode
  if (is.null(mode) || mode != "cf" & mode != "cru"){
    stop("Please provide a valid cluster type: cf or cru.")
  }
  if ( is.null(scale) || scale != "global" & scale != "europe"){
    stop("Please provide a valid scale: global or europe.")
  }
  if (is.null(typeList)){
    typeList <-  typelist.default
    cat("\nOutput typeList has not been provided.")
    cat("\nSetting typeList to default values.")
  }
  # checking template1
  if (is.null(template1)){
    cat("\n\nUsing package template (template 1).")
    cat("\nSaving package template in the mainDir.")
    template1 <- getTemplate (scale, mainDir)
  }else if (!file.exists(file.path(mainDir, template1))){
    warning ("The provided template (template1) does not exist.")
    stop("Please provide a valid template name.")
  }
  # checkign template 2: either cru or cf
  if ( is.null(template2)){
    template2 <- getTemplate (type = paste(scale,"_", mode, sep = ""), outputDir = mainDir)
    cat("\n\nUsing package template (template 2).")
    cat("\nSaving package template in the mainDir.")
  }else if (!file.exists(file.path(mainDir, template2))){
    warning ("The provided template (template2) does not exist.")
    stop("Please provide a valid template name.")
  }
  # checking gridlist
  if ( is.null(gridList) || !file.exists(file.path(mainDir, gridList))){
    stop ("Please provide a valid grid list.")
  }
  # creatiing the folder structure
  cat("\n\nCreating the foder structure")
  for (i in 1:numCores) {
    cat(paste("....", i, sep = ""))
    dir.create(file.path(mainDir, paste("runDirectory",i,sep="")), showWarnings = FALSE)
    dir.create(file.path(mainDir, paste("runDirectory",i,sep=""),
                         paste("outDirectory",i,sep="")), showWarnings = FALSE)
  }

  # starting the function itself
  return ( list(mainDir=mainDir,template1=template1,template2=template2,
                gridList=gridList, clusterType = clusterType, numCores= numCores,
                typeList=typeList, mode = mode, scale = scale ))
}
