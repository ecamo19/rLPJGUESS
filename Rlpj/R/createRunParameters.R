# @title The function to create the runParameters
#
# @description This function TODO
# @param x a LPJ setup object
# @param singleRun see createSingleObject.
# @param parameterList parameterList either a named list containing the parameters to be calibrated
# or a matrix.
# @seealso  \code{\link{runLPJ}}
# @author Ramiro Silveyra Gonzalez, Maurizio Bagnara, Florian Hartig
# @return TODO



createRunParameters <- function(x, singleRun, parameterList){
  # Check the parameters: length, type and names
  if ( is.null(parameterList)){
    cat ("\n\nYou have not provided a parameter list")
    cat ("\nModel will run with default values")
    parameterList <- getParameterList(singleRun$scale)
    runsParameters  <- 1
    parameterNames <- names(parameterList)
  }else if (class(parameterList[[1]])== "list"){
    runsParameters <- length(parameterList)
    parameterNames <- lapply(parameterList, function(x){
      parNames <-names(x)
      return(parNames)
      })
    parameterNames <- unique(unlist(parameterNames))
  }else if (class(parameterList)== "list"){
    runsParameters  <- 1
    parameterNames <- names(parameterList)
  }else if (class(parameterList)== "matrix"){
    if (is.null(colnames(parameterList))){
      stop("Matrix should have parameter names as column names")
    }else{
    runsParameters  <- nrow(parameterList)
    # convert to a list
    parameterNames <- colnames(parameterList)
    indices <- c(1:nrow(parameterList))
    parameterList <- lapply(indices, function(x){
      parComb <- as.list(parameterList[x,])
      names(parComb) <-parameterNames
      return(parComb)
      })
    }
  }else{
    stop ("Invalid parameter list")
  }
  # Based on parameter Names write the general template
  # and if no parameter is present raise and error?
  parameterCommon <- checkParameters(scale= singleRun$scale, parameterNames, type = "names")

  if(length(parameterCommon) > 0){
    # write common template
    parameterCommonNames <- names(parameterCommon)
    for(i in 1:length(parameterCommon))  {
      singleRun$template1Mem <- sub(parameterCommonNames[i], parameterCommon[[i]], singleRun$template1Mem)
    }

  }

  # Check the grids
  gridListCell <- readLines(file.path(singleRun$mainDir,singleRun$gridList))
  gridListCell <- gridListCell[!grepl("!", gridListCell)]
  gridListCell <- gridListCell[!is.na(gridListCell)]
  runsGrids <- length(gridListCell)

  # FIGURE OUT HOW MANY PARAMETERS AND GRIDS
  #----------------------------------------------------------------------------#
  parallel <- "serial"
  if (runsGrids == 1){
    if (runsParameters == 1){
      stop("Error: Your required parallelization is not feasible")
    }else{
      parallel <- "parameters"
    }
  }else{
    if (runsParameters == 1){
      parallel <- "grids"
    }else{
      parallel <- "both"
    }
  }
  # Compare to user request
  if (singleRun$parallel  != "auto"){
    if (parallel  == "both"){
      if (singleRun$parallel == "grids"){
        warning("Please check the number of cells and/or parameters provided")
        stop("Error: Your required parallelization is not feasible")
      }else if (singleRun$parallel == "parameters"){
        runsGrids <- 1
        parallel <- singleRun$parallel
      }else{
        parallel <- singleRun$parallel
      }
    }else if(parallel !=  singleRun$parallel) {
      warning("Please check the number of cells and/or parameters provided")
      stop("Error: Your required parallelization is not feasible")
    }
  }

  numberRuns <- runsParameters * runsGrids
  if (numberRuns < x@numCores){
    stop("The number of cores requested exceeds the number of runs")
  }
  # So far so good,
  singleRun$checkParameters <- "parallel"
  # Create an output folder named after ID
  dir.create(singleRun$runInfoDir, showWarnings = FALSE)
  # Distirbute directories along runs
  runDir <- vector("character", numberRuns)
  outDir <- vector("character", numberRuns)
  for (i in 1:x@numCores) {
    for (index in seq(i, numberRuns, x@numCores )){
      runDir[index] <- x@runDir[i]
      outDir[index] <- x@outDir[i]
    }
  }
  # CREATE SINGLE RUN OBJECTS
  #----------------------------------------------------------------------------#
  runParameters <- vector("list", numberRuns)
  if (parallel == "parameters"){
    cat("\nParallelization of parameters\n")
    cat("\nCreating the single run objects")#single run objects
    progessBar <- txtProgressBar(min = 0, max = numberRuns, style = 3)
    for (i in 1:numberRuns){
      setTxtProgressBar(progessBar, i)
      singleRun$runID <- i
      singleRun$runDir <- runDir[i]
      singleRun$outDir <- outDir[i]
      singleRun$parameterList <- parameterList[[i]]
      singleRun$gridListCell <- gridListCell
      singleRun$gridListName <- paste(unlist(strsplit(gridListCell, " ")), collapse = "_")
      runParameters[[i]] <- singleRun
    }
    close(progessBar)
  }else if (parallel == "both"){
    cat("\nParallelization of both parameters and grids\n")
    cat("\nCreating the single run objects")#single run objects
    progessBar <- txtProgressBar(min = 0, max = numberRuns, style = 3)
    for (i in 1:numberRuns){
      setTxtProgressBar(progessBar, i)
      singleRun$runID <- i
      singleRun$runDir <- runDir[i]
      singleRun$outDir <- outDir[i]
      singleRun$parameterList <- parameterList[[ceiling(i/length(gridListCell))]]
      singleRun$gridListCell <- gridListCell[ceiling(i/length(parameterList))]
      singleRun$gridListName <- paste(unlist(strsplit(gridListCell[ceiling(i/length(parameterList))][i], " ")), collapse = "_")
      runParameters[[i]] <- singleRun
    }
    close(progessBar)
  }else if (parallel == "grids"){
    cat("\nParallelization of grids\n")
    cat("\nCreating the single run objects")#single run objects


    progessBar <- txtProgressBar(min = 0, max = numberRuns, style = 3)
    singleRun$parameterList <- parameterList
    for (i in 1:numberRuns){
      setTxtProgressBar(progessBar, i)
      singleRun$runID <- i
      singleRun$runDir <- runDir[i]
      singleRun$outDir <- outDir[i]
      singleRun$gridListCell <- gridListCell[i]
      singleRun$gridListName <- paste(unlist(strsplit(gridListCell[i], " ")), collapse = "_")
      runParameters[[i]] <- singleRun
    }
    close(progessBar)
  }

  return(runParameters)
}



