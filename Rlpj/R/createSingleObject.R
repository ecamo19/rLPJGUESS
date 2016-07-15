# @title The function to fill the single run object
#
# @description This function TODO
# @param mainDir a character string indicating the path to the main directory
# @param typeList a character vector with the outputs to be analyzed.
#  Default value is all outputs.
# @param settings additional parameters
# @seealso  \code{\link{runLPJ}}
# @author Ramiro Silveyra Gonzalez, Maurizio Bagnara, Florian Hartig
# @return TODO
createSingleObject <- function(mainDir, typeList, settings){

  defaultSettings <- list(gridList= NULL, scale = NULL, mode = NULL,
                        file.co2 = NULL, file.cru = NULL, file.cru.misc = NULL,
                        file.ndep= NULL, file.temp = NULL, file.prec = NULL,
                        file.insol = NULL, template1 = NULL, template2=NULL,
                        plot.data = FALSE, save.plots = FALSE, processing = FALSE,
                        delete = TRUE,  runID = "", parallel = "auto", checkParameters = "serial",
                        design = NULL)
  #, fun = NULL) # This would be to allow havin own functions in parallel.

  settings <- c(settings[names(settings) %in% names(defaultSettings)],
                defaultSettings[ !names(defaultSettings) %in% names(settings)])

  # mode
  if (is.null(settings[["mode"]]) || settings[["mode"]] != "cf" & settings[["mode"]] != "cru"){
    stop("Please provide a valid cluster type: cf or cru")
  }
  if ( is.null(settings[["scale"]]) || settings[["scale"]] != "global" & settings[["scale"]] != "europe"){ # this is relevant if getting template
    stop("Please provide a valid scale: global or europe")
  }
  if (is.null(typeList) || !class(typeList) == "character"){
    settings$typeList <-  typelist.default
    cat("\n\nOutput typeList has not been provided")
    cat("\nSetting typeList to default values")
  }else{
    settings$typeList <- typeList
  }
  if ( settings[["parallel"]] != "auto" & settings[["parallel"]] != "grids" & settings[["parallel"]] != "parameters"  & settings[["parallel"]] != "both"){ # this is relevant if getting template
    stop("Please provide a valid parallel value")
  }
#  # will potential provide more parameters
#  if (!is.null(settings[["fun"]])){
#
#    if (class(settings[["fun"]]) == "character"){
#      if (settings[["fun"]] == "met"){
#        settings$fun  <- calculateMet
#      }else{
#        settings$fun <- NULL
#      }
#    }else if(class(settings[["fun"]]) == "function"){
#      cat("\nAdded user defined fun")
#    }else{
#      warning("The prodived fun argument is not provided")
#      setings$fun <- NULL
#    }
#  }


  # checking template1
  if (is.null(settings[["template1"]])){
    # writing out template and storing name
    settings$template1 <- getTemplate (settings[["scale"]], outputDir = mainDir)
    cat("\n\nUsing package template (template 1)")
    cat("\nSaving package template in the mainDir")
  }else if (!file.exists(file.path(mainDir, settings[["template1"]]))){
    warning ("The provided template (template1) does not exist")
    stop("Please provide a valid template name")
  }
  # checkign template 2: either cru or cf
  if ( is.null(settings[["template2"]])){
    # writing out template and storing name
    settings$template2 <- getTemplate (type = paste(settings[["scale"]],"_", settings[["mode"]], sep = ""),
                                       outputDir = mainDir)
    cat("\n\nUsing package template (template 2)")
    cat("\nSaving package template in the mainDir")
  }else if (!file.exists(file.path(mainDir, settings[["template2"]]))){
    warning ("The provided template (template2) does not exist")
    stop("Please provide a valid template name")
  }

  # Check the design
  if(is.null(settings[["design"]])){
    cat("\n\nUsing standard LPJ-GUESS design")
    settings$design <- getDesign(settings$scale, list= T )
  }else if(class(settings[["design"]]) == "list" ){
    # If something was provided, the complete the list
    design.default <- getDesign(settings$scale, list= T )
    settings$design <- c(settings$design[names(settings$design) %in% names(design.default) ],
                         design.default[ !names(design.default) %in% names(settings$design)])
  }else{
    stop("Please provide a valid design")
  }

  # Pack up all files that user should have provided
  # Get the default list from internal data , that contains the characters stings
  # to replace in the template
  # Go throught the files and check whether they provided, if so add them to
  # default list, otherwise stop the function
  files <- settings[grepl("file", names(settings))]
  files.default <-   files.parameters[[settings[["mode"]]]]
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
  singleObject <- settings[!grepl("file", names(settings))]
  singleObject$filesNames <- files.default
  singleObject$mainDir <- mainDir
  singleObject$runInfoDir <-  file.path(singleObject$mainDir,
                                        paste("runInfo",
                                              format(Sys.time(), "%Y_%m_%d_%H%M%S"),
                                              sep = "_"))
  # Read template one and replace desing
  singleObject$template1Mem <- readLines(file.path(singleObject$mainDir, singleObject$template1))

  designNames<- names(settings$design)
  for(i in 1:length(settings$design))  {
    singleObject$template1Mem <- sub(designNames[i], settings$design[[i]], singleObject$template1Mem)
  }
  if(settings$design[["run_ifcalcsla"]]==as.character(0)){
    singleObject$template1Mem <- sub("!sla", "sla", singleObject$template1Mem)
  }



  singleObject$template2Mem <- readLines(file.path(singleObject$mainDir,singleObject$template2))

  return(singleObject)
}


