#' @title The function to run the LPJ in parallel
#' @description This function reads the setup parallel object and creates a
#'  cluster to which it submits the model wrapper function with its respective
#'  parameters.
#' @param mainDir a character string indicating the path to the directory where
#'  all input data and template are located and in which the function will create
#'  the directory structure for the outputs
#' @param gridList a character string providing the name of the text file with
#' the grids to be included in the model, e.g gridlist.txt. It must be in the mainDir.
#' Provide only the file name, not the path.
#' @param scale a character string indicating whether the model runs global or
#' for europe.
#' @param mode a character string indicating whether using cru or cf data
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
#' @param typeList a character vector with the outputs to be analyzed.
#' Default value is all outputs.
#' @param parameterList a named list containing the parameters to be calibrated
#' @param template1  character string providing the general model template,
#'  e.g, global.ins. It must be in the mainDir. Provide only the file name,
#'   not the path. If not provided, package templates will be used.
#' @param template2 a character string providing the  "specific" model template,
#'  e.g, global_cf.ins or global_cru.ins. It must be in the mainDir. Provide
#'  only the file name, not the path. If not provided, package templates will be
#'   used.
#' @param plot.data  a boolean indicating whether the ouput data will be plotted
#'  (default FALSE).
#' @param save.plots  a boolean indicating whether the plots will be saved (default
#'  FALSE).
#' @param processing a boolean indicating whether output files will be turned into
#'  time series (default FALSE).
#' @param delete a boolean indicating whether output files should be deleted after
#'  processing (default TRUE).
#' Saved plots will not be deleted.
#' @param parallel a boolean indicating whether the function must run in parallel
#' (default FALSE). If parallel TRUE, the setupObject must be provided.
#' @param setupObject a names list created with the setupLPJParallel function.
#' @param ID an integer after which the output directory will be named (default empty).
#' If parallel TRUE, ID is ignored and defined by setupLPJParallel.
#' @return a list holding the outputs of the runLPJwrapper (see function help)
#' @details The runLPJ in parallel assumes the existence of a folder containing all
#' the inputs data and templates for LPJ-GUESS and a directory structure for
#' storing inputs and outputs of each single run. The setupLPJParallel function is
#' ought to be run before calling the runLPJparallel.
#' Running the LPJ parallel involves two steps. First, to create a parallel
#' setup (setupParallel function), and second, to actually run in parallel the model
#' (runLPJparallel function).  The parallelization requires the packages snow and
#'  also the Rmpi package, if you aim at using a MPI cluster.
#' @seealso  \url{https://cran.r-project.org/web/packages/Rmpi/Rmpi.pdf},
#'  \url{https://cran.r-project.org/web/packages/snow/snow.pdf,
#'  \code{\link{setupLPJParallel}}}
#' @export
#' @keywords Rlpj
#' @author Florian Hartig, Ramiro Silveyra Gonzalez
#' @examples \dontrun{
#' #' # You need to specify the absolute path of each input file:
#' file.co2<-"/some/absolute/path/crudata/co2_1901-2013.txt"
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
#' #' mainDir <- "/some/absolute/path/mainDir"
#' list.files(mainDir)
#'    [1] "guess" or "guesscmd.exe"  # link to the model executable
#'    [2] "gridlist.txt"             # list of gridcells
#'    [3] "global.ins"               # template1 (optional)
#'    [4] "global_cru.ins"           # template2 (optional)
#'
#'
#' # Single  Run
#' result <- runLPJ(mainDir, gridList, scale = "global",mode = "cf", file.co2,
#'                  file.cru, file.cru.misc, file.ndep, file.temp , file.prec,
#'                  file.insol)
#'
#'
#'        Output typeList has not been provided.
#'        Setting typeList to default values.
#'
#'        Using package template (template 1).
#'        Saving package template in the mainDir.
#'
#'        Using package template (template 2).
#'        Saving package template in the mainDir.
#'
#'        You have not provided a parameter list.
#'        Model will run with default values
#'
#'        Starting run 1
#'        Calling "/some/absolute/path/guess -input cf "/some/absolute/path/runDirectory/global_cf.ins
#'        Finished run 1
#'
#'  str(result,2)
#'        Formal class 'LPJData' [package "Rlpj"] with 2 slots
#'        ..@ runInfo  :List of 17
#'        ..@ dataTypes:List of 39
#'
#'
#' #Parallel Run
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
#' mySetup  <- setupLPJParallel(3, "SOCK", "cf", mainDir = "/some/absolute/path/mainDir")
#'
#' # Call runLPJ
#' result <- runLPJ(mainDir, gridList, scale = "global",mode = "cf", file.co2,
#'                  file.cru, file.cru.misc, file.ndep, file.temp, file.prec,
#'                  file.insol, parameterList = parameterList, parallel = TRUE,
#'                  setupObject = mySetup)
#'
#'    Output typeList has not been provided
#'    Setting typeList to default values
#'
#'    Using package template (template 1)
#'    Saving package template in the mainDir
#'
#'    Using package template (template 2)
#'    Saving package template in the mainDir
#'
#'    Checking conditions
#'    Reading the parallel object structure
#'    |=============================================================| 100%
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
runLPJ <- function(mainDir=NULL, gridList= NULL, scale = NULL, mode = NULL,
                   file.co2 = NULL, file.cru = NULL, file.cru.misc = NULL,
                   file.ndep= NULL, file.temp = NULL, file.prec = NULL, file.insol = NULL,
                   typeList = NULL, parameterList=NULL, template1 = NULL,
                   template2=NULL,  plot.data = FALSE, save.plots = FALSE,
                   processing = FALSE, delete = TRUE, parallel = FALSE,
                   setupObject = NULL, ID = ""){
  #----------------------------------------------------------------------------#
  # CHECK INPUTS AND EXIT IF ANY ERROR
  #----------------------------------------------------------------------------#
  # mainDir
    if (is.null(mainDir) || !file.exists(mainDir)){
      stop("Please provide a valid main directory")
    }
    # mode
    if (is.null(mode) || mode != "cf" & mode != "cru"){
      stop("Please provide a valid cluster type: cf or cru")
    }
    if ( is.null(scale) || scale != "global" & scale != "europe"){ # this is relevant if getting template
      stop("Please provide a valid scale: global or europe")
    }
    if (is.null(typeList)){
      typeList <-  typelist.default
      cat("\n\nOutput typeList has not been provided")
      cat("\nSetting typeList to default values")
    }
    # checking template1
    if (is.null(template1)){
      # writing out template and storing name
      template1 <- getTemplate (scale, outputDir = mainDir)
      cat("\n\nUsing package template (template 1).")
      cat("\nSaving package template in the mainDir.")
    }else if (!file.exists(file.path(mainDir, template1))){
      warning ("The provided template (template1) does not exist")
      stop("Please provide a valid template name")
    }
    # checkign template 2: either cru or cf
    if ( is.null(template2)){
      # writing out template and storing name
      template2 <- getTemplate (type = paste(scale,"_", mode, sep = ""),
                                outputDir = mainDir)
      cat("\n\nUsing package template (template 2).")
      cat("\nSaving package template in the mainDir.")
    }else if (!file.exists(file.path(mainDir, template2))){
      warning ("The provided template (template2) does not exist")
      stop("Please provide a valid template name")
    }
    # checking gridlist
    if ( is.null(gridList) || !file.exists(file.path(mainDir, gridList))){
      stop ("Please provide a valid grid list.")
    }
    # Parallel/single specific check before doing anything else
    if(parallel){
      if (is.null(setupObject)){
        stop("Please provide a setup object")
      }
      if (is.null(parameterList)){
        stop("Please provide a valid parameter list")
      }
    }else{
      if ( is.null(parameterList)){
        cat ("\n\nYou have not provided a parameter list.")
        cat ("\nModel will run with default values")
      }
    }
    # Pack up all files that user should have provided
    # Get the default list from internal data , that contains the characters stings
    # to replace in the template
    # Go throught the files and check whether they provided, if so add them to
    # default list, otherwise stop the function
    files <- list(file.co2 = file.co2, file.cru = file.cru,
                  file.cru.misc = file.cru.misc, file.ndep = file.ndep,
                  file.temp = file.temp, file.prec = file.prec,
                  file.insol = file.insol)
    files.default <-   files.parameters[[mode]]
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

  #----------------------------------------------------------------------------#
  # DO WHAT IS COMMON TO SINGLE AND PARALLEL
  #----------------------------------------------------------------------------#
  # create a runinfo dir
  runInfoDir <- file.path(mainDir, paste("runInfo",
                                         format(Sys.time(), "%Y_%m_%d_%H%M%S"),
                                         sep = "_"))
  dir.create(runInfoDir, showWarnings = FALSE)
  # create the single object
  singleRun <- list (mainDir = mainDir,  template1 = NULL, template1Name = template1,
                     template2 = NULL, template2Name = template2, gridList= NULL,
                     gridListName = gridList, runDir = NULL, outDir = NULL,
                     mode = mode, scale = scale,  typeList = typeList, parameterList=NULL,
                     runID = NULL,  runInfoDir = runInfoDir,
                     processing = processing, delete = delete, plot.data = plot.data,
                     save.plots = save.plots, files.names = files.default)
  singleRun$gridList <- readLines(file.path(mainDir,gridList))
  #----------------------------------------------------------------------------#
  # PARALLEL
  #----------------------------------------------------------------------------#
  if(parallel){
    # setup object has all needed for pallel structure
    result <- runLPJParallel(setupObject, singleRun, parameterList)
  #----------------------------------------------------------------------------#
  # SINGLE
  #----------------------------------------------------------------------------#
  }else{
    # Need to create an output folder named after ID
    runDir <- file.path(mainDir, paste("runDirectory", ID, sep=""))
    outDir <- file.path(mainDir, paste("runDirectory", ID, sep=""), paste("outDirectory", ID, sep=""))
    #outDir <- file.path(mainDir, paste("outDirectory", ID, sep=""))
    dir.create(runDir, showWarnings = FALSE)
    dir.create(outDir, showWarnings = FALSE)
    singleRun <- fillSingleObject(singleRun, runDir, outDir, parameterList, ID)
    result <- runLPJWrapper(singleRun)
  }
  #----------------------------------------------------------------------------#
  # END
  #----------------------------------------------------------------------------#
  cat("\nProcessing ended!")
  return(result)
}
