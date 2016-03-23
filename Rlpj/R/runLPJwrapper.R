#' @title A wrapper to run the LPJ-GUESS and processed the ouputs
#'
#' @description This function modifies the lpj template with the desired parameter
#'  values and then does a system call for running the lpj modell.
#'  The modell outputs are processed, plotted (if set to TRUE)
#' @param runObject a named list created by the runLPJParallel function containing
#' the following: mainDir, template1, template2, gridList, runDir, outDir, mode,
#' scale, typeList, runID,  gridFilename, plot.data and save.plots
#' @return a a data class object with the slots runInfo and dataTypes. The runInfo
#' slot contains the provided run information and parameters; the dataTypes holds
#' the processed output data from the run.
#' @export
#' @keywords Rlpj
#' @author Florian Hartig, Ramiro Silveyra Gonzalez, Maurizio Bagnara
#' @note Based an older code of Istem Fer, Uni Potsdam
#' @examples \dontrun{
#' runLPJwrapper(runObject)
#' }
runLPJwrapper <- function(runObject){
  # checking conditions
  if (is.null(runObject[["runDir"]]) || !file.exists(runObject$runDir)){
    stop("Please provide a valid run directory.")
  }
  if (is.null(runObject[["outDir"]]) || !file.exists(runObject$outDir)){
    stop("Please provide a valid output directory.")
  }
  if (is.null(runObject[["template1"]])){
    stop("Please provide a valid  template name.")
  }
  if (is.null(runObject[["template2"]])){
    stop("Please provide a valid  template name.")
  }
  if (is.null(runObject[["parameterList"]])){
    stop("Please provide a valid parameter list.")
  }
  if (is.null(runObject[["plot.data"]])){
    warning("The plot.data boolean has not been provided. It will be set to FALSE.")
    runObject$plot.data <- FALSE
    runObject$save.plots <- FALSE
  }
  if ( is.null(runObject[["save.plots"]])){
    warning("The save.plots boolean has not been provided. It will be set to FALSE.")
    runObject$save.plots <- FALSE
  }
  if (is.null(runObject[["typeList"]])){
    runObject$typeList <- typelist.default
    warning("The output type list has not been provided")
    warning("Setting type list to default values.")
  }
  # checking directory existence
  # starting the function itself
  # print out info message
  message(paste("\nStarting run ", runObject$runID,  "\n", sep = ""))
  # set the wd
  setwd(runObject$runDir)
  # write out files
  writeLines(runObject$template1,file.path(runObject$runDir,
             paste(runObject$scale, ".ins", sep = "") ))
  writeLines(runObject$template2,file.path(runObject$runDir,
             paste(runObject$scale,"_", runObject$mode, ".ins", sep = "") ))
  writeLines(runObject$gridList, file.path(runObject$runDir, runObject$gridFilename ))
  # writing template
  writeTemplate(paste(runObject$scale, ".ins", sep = "") , runObject$parameterList,
                  runObject$runDir)
  # calling the model
  runLPJ(runObject$mainDir, runObject$runDir,
         paste(runObject$scale,"_", runObject$mode, ".ins", sep = "") )
  # getting data
  LPJout <- getData (runObject$typeList, runObject$outDir, runObject)
  # plotting data
  # delete all files
  files.delete <- list.files(runObject$runDir, full.names = TRUE, recursive = TRUE)
  files.delete <- files.delete[!grepl("runInfo", files.delete)]
  do.call("unlink", list(files.delete))
  # Sav the run info --> gotta go somewhere else
  runObject$output <- LPJout@dataTypes
  save(runObject, file = file.path(runObject$runInfoDir,
                                    paste("runInfo", runObject$runID,".Rdata", sep = "")))
  #For example, make a list with all the info provided to runLPJ, and store it with save()
  if (runObject[["plot.data"]] == TRUE){
    plotData(data = LPJout@dataTypes, typeList = runObject$typeList,
             outDir = runObject$outDir, save.plots = runObject$save.plots)
  }
  message(paste("\nFinished run ", runObject$runID,  "\n", sep = ""))
  return (LPJout)
}



