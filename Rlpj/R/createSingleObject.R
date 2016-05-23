#' @title The function to fill the single run object
#'
#' @description This function TODO
#' @param singleObject TODO
#' @param runDir TODO
#' @param outDir TODO
#' @param parameterList TODO
#' @param ID TODO
#' @export
#' @return TODO
createSingleObject <- function(mainDir, typeList, settings){

  defaultSettings <- list(gridList= NULL, scale = NULL, mode = NULL,
                        file.co2 = NULL, file.cru = NULL, file.cru.misc = NULL,
                        file.ndep= NULL, file.temp = NULL, file.prec = NULL,
                        file.insol = NULL, template1 = NULL, template2=NULL,
                        plot.data = FALSE, save.plots = FALSE, processing = FALSE,
                        delete = TRUE,  runID = "")

  settings <- c(settings, defaultSettings[ !names(defaultSettings) %in% settings])

  # mode
  if (is.null(settings[["mode"]]) || settings[["mode"]] != "cf" & settings[["mode"]] != "cru"){
    stop("Please provide a valid cluster type: cf or cru")
  }
  if ( is.null(settings[["scale"]]) || settings[["scale"]] != "global" & settings[["scale"]] != "europe"){ # this is relevant if getting template
    stop("Please provide a valid scale: global or europe")
  }
  if (is.null(typeList)){
    settings$typeList <-  typelist.default
    cat("\n\nOutput typeList has not been provided")
    cat("\nSetting typeList to default values")
  }
  # checking template1
  if (is.null(settings[["template1"]])){
    # writing out template and storing name
    settings$template1 <- getTemplate (settings[["scale"]], outputDir = mainDir)
    cat("\n\nUsing package template (template 1).")
    cat("\nSaving package template in the mainDir.")
  }else if (!file.exists(file.path(mainDir, settings[["template1"]]))){
    warning ("The provided template (template1) does not exist")
    stop("Please provide a valid template name")
  }
  # checkign template 2: either cru or cf
  if ( is.null(settings[["template2"]])){
    # writing out template and storing name
    settings$template2 <- getTemplate (type = paste(settings[["scale"]],"_", settings[["mode"]], sep = ""),
                              outputDir = mainDir)
    cat("\n\nUsing package template (template 2).")
    cat("\nSaving package template in the mainDir.")
  }else if (!file.exists(file.path(mainDir, settings[["template2"]]))){
    warning ("The provided template (template2) does not exist")
    stop("Please provide a valid template name")
  }
  # checking gridlist
  if ( is.null(settings[["gridList"]]) || !file.exists(file.path(mainDir, settings[["gridList"]]))){
    stop ("Please provide a valid grid list.")
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
  singleObject$gridListMem <- readLines(file.path(singleObject$mainDir,singleObject$gridList))
  singleObject$runInfoDir <-  file.path(singleObject$mainDir,
                                        paste("runInfo",
                                              format(Sys.time(), "%Y_%m_%d_%H%M%S"),
                                              sep = "_"))

  #
  return(singleObject)
}


