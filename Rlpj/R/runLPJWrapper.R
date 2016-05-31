# @title A wrapper to run the LPJ-GUESS and processed the ouputs
#
# @description This function modifies the lpj template with the desired parameter
#  values and then does a system call for running the lpj modell.
#  The modell outputs are processed, plotted (if set to TRUE)
# @param runObject a named list created by the runLPJParallel function containing
# the following: mainDir, template1, template2, gridList, runDir, outDir, mode,
# scale, typeList, runID,  gridFilename, plot.data and save.plots
# @return a a data class object with the slots runInfo and dataTypes. The runInfo
# slot contains the provided run information and parameters; the dataTypes holds
# the processed output data from the run.
# @keywords Rlpj
# @author Florian Hartig, Ramiro Silveyra Gonzalez, Maurizio Bagnara
# @note Based an older code of Istem Fer, Uni Potsdam
# @examples \dontrun{
# runLPJwrapper(runObject)
# }
runLPJWrapper <- function(runObject){
  #----------------------------------------------------------------------------#
  # CHECK INPUTS AND EXIT IF ANY ERROR
  #----------------------------------------------------------------------------#
  # checking conditions
  if (is.null(runObject[["runDir"]]) || !file.exists(runObject$runDir)){
    stop("Please provide a valid run directory")
  }
  if (is.null(runObject[["outDir"]]) || !file.exists(runObject$outDir)){
    stop("Please provide a valid output directory")
  }
  if (is.null(runObject[["template1"]])){
    stop("Please provide a valid  template1 name")
  }
  if (is.null(runObject[["template2"]])){
    stop("Please provide a valid  template2 name")
  }
  #if (is.null(runObject[["parameterList"]])){ No because it could run with default values
  #  stop("Please provide a valid parameter list.")
  #}
  if (is.null(runObject[["plot.data"]])){
    warning("The plot.data boolean has not been provided. It will be set to FALSE")
    runObject$plot.data <- FALSE
    runObject$save.plots <- FALSE
  }
  if ( is.null(runObject[["save.plots"]])){
    warning("The save.plots boolean has not been provided. It will be set to FALSE")
    runObject$save.plots <- FALSE
  }
  if (is.null(runObject[["typeList"]])){
    runObject$typeList <- typelist.default
    warning("The output type list has not been provided")
    warning("Setting type list to default values")
  }
  #----------------------------------------------------------------------------#
  # WRAP THE FUNCTIONS
  #----------------------------------------------------------------------------#
  # checking directory existence
  # starting the function itself
  # print out info message
  cat(paste("\n\nStarting run ", runObject$runID,  "\n", sep = ""))
  # set the wd
  setwd(runObject$runDir)
  # write out files
  writeLines(runObject$template1Mem,file.path(runObject$runDir,runObject$template1))
  writeLines(runObject$template2Mem,file.path(runObject$runDir,runObject$template2))
  writeLines(runObject$gridListMem, file.path(runObject$runDir, runObject$gridList))
  # writing template
  writeTemplate(runObject$template1, runObject$parameterList,
                  runObject$runDir)
  # calling the model
  callLPJ(runObject$mainDir, runObject$runDir,runObject$template2, runObject$mode)
  # getting data
  LPJout <- getData(runObject$outDir, runObject$typeList, runObject,
                     runObject$processing)


  #----------------------------------------------------------------------------#
  # CLEAN UP RUNDIR
  #----------------------------------------------------------------------------#
  # delete all files
  if ( runObject$delete == TRUE){
    files.delete <- list.files(runObject$runDir, full.names = TRUE, recursive = TRUE)
    #files.delete <- files.delete[!grepl("runInfo", files.delete)]
    files.delete <- files.delete[!grepl("png", files.delete)]
    do.call("unlink", list(files.delete))
  }
  #----------------------------------------------------------------------------#
  # SAVE RUNINFO AND PLOT
  #----------------------------------------------------------------------------#
  runObject$output <- LPJout@dataTypes
  # Sav the run info
  #For example, make a list with all the info provided to runLPJ, and store it with save()
  save(runObject, file = file.path(runObject$runInfoDir,
                                   paste("runInfo", runObject$runID,".Rdata", sep = "")))
  if (runObject[["plot.data"]] == TRUE){
    plotLPJData(x = LPJout, typeList = runObject$typeList,
             outDir = runObject$outDir, save.plots = runObject$save.plots,
             prefix = paste("run",runObject$runID, "_", sep=""))
  }
  cat(paste("\nFinished run ", runObject$runID,  "\n", sep = ""))
  #----------------------------------------------------------------------------#
  # END
  #----------------------------------------------------------------------------#
  return (LPJout)
}



