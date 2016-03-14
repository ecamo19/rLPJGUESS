#' @title The function to create a setup for parallel runs of the LPJ-GUESS
#'
#' @description This function first creates a setup for running the LPJ in parallel
#'  based o the provided input parameters, and then runs the model in parallel.
#'  The function asumes a specific initial configuration. A folder (mainDir)
#'  containing the input data, the templates, and link to the model must exist.
#'  A directory structure for the outputs will be build wihtin this folder by
#'  the function.
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
#' @return a setup object or names list containing the setup paremeters to run
#' the LPJ in parallel
#' @seealso  \url{https://cran.r-project.org/web/packages/Rmpi/Rmpi.pdf},
#'  \url{https://cran.r-project.org/web/packages/snow/snow.pdf}
#' @export
#' @keywords Rlpj
#' @author Florian Hartig, Ramiro Silveyra Gonzalez, Maurizio Bagnara
#' @examples \dontrun{
#'  typeList <- c("aaet", "nuptake")
#'  setupObject <- setupParallel(numCores= 10, clusterType = "SOCK",
#'                   gridList= "gridlist.txt", mainDir=mainDir,typeList = typeList,
#'                   scale = "europe", mode = "cf")
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
        stop("\nThere are not enough available cores  to create a cluster")
      }else if ( numCores.available != numCores) {
        message(paste("There are", numCores.available,"cores available ", sep = " "))
        message(paste("You requested", numCores,  "cores", sep = " "))
        message("The number of cores will be set to meet the available resources")
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
    stop("Please provide a valid cluster type: cf or cru.\n")
  }
  if ( is.null(scale) || scale != "global" & scale != "europe"){
    stop("Please provide a valid scale: global or europe")
  }
  if (is.null(typeList)){
    typeList <-  typelist.default
    cat("\nOutput type list has not been provided.")
    cat("\nSetting type list to default values")
  }
  # checking template1
  if (is.null(template1)){
    cat("\nUsing package template (template 1)")
    template1 <- getTemplate (scale, mainDir)
  }else if (!file.exists(file.path(mainDir, template1))){
    warning ("The provided template (template1) does not exits")
    stop("Please provide a valid template name.")
  }
  # checkign template 2: either cru or cf
  if ( is.null(template2)){
    template2 <- getTemplate (type = paste(scale,"_", mode, sep = ""), outputDir = mainDir)
    cat("\nUsing package template (template 2)")
  }else if (!file.exists(file.path(mainDir, template2))){
    warning ("The provided template (template2) does not exits.")
    stop("Please provide a valid template name.")
  }
  # checking gridlist
  if ( is.null(gridList) || !file.exists(file.path(mainDir, gridList))){
    stop ("Please provide a valid grid list.")
  }
  # creatiing the folder structure
  cat("\nCreating the foder structure")
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

