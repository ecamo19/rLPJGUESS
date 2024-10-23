#' @title A get runInfo data
#' @description  This function reads the \code{\link{runLPJ}} outputs stored in the runInfo file. Additionally,
#' allows to retrieve the parameters from the runInfoDir files.
#' @param x a character string indicating the absolute path to the runInfoDir folder or
#' a R object produced by \code{\link{runLPJ}}
#' @param parameters a character boolean to specify whether to return the parameters
#' instead of the \linkS4class{LPJData} objects
#' @seealso \code{\link{runLPJ}}, \linkS4class{LPJData}
#' @export
#' @author Ramiro Silveyra Gonzalez
#' @example /inst/examples/getRunInfoHelp.R
getRunInfo <- function(x, parameters = F){

  if(parameters){
    if (class(x) == "LPJData"){
      tmp <- as.matrix(x["parameterList"])
    }else{
      if (class(x) == "character"){ x <- readRunInfo(x)}

      if (class(x) == "LPJData"){
        tmp <- as.matrix(x["parameterList"])
      }else if (class(x) == "list"){
        if (!class(x[[1]]) == "LPJData")stop("Invalid value for x")

        tmp <- lapply(x, function(x) t(as.matrix(x["parameterList"])))

        tmp <- do.call(rbind, tmp)
      }else{
        stop("Invalid value for x")
      }
    }
  }else{
    tmp <- readRunInfo(x)
  }
  return(tmp)
}



# @title A read runInfo data
# @description  This function reads the runLPj outputs stored in the runInfoDir
# and returns as a list, with the same structure as the runLPJ outputs.
# @param x a character string indicating the absolute path to the runInfoDir folder
# @author Ramiro Silveyra Gonzalez
# @examples \dontrun{
# # recover the data from the runInfo folder
# result <- getRunInfo("/some/absolute/path/runInfo_2016_08_11_121507")
#
# }
readRunInfo <- function(x){
  if (is.null(x) || !file.exists(x)){stop("Invalid runDirInfo folder")}

  outputs <- list.files(x, full.names = T)
  if (length(outputs)==0 || !grepl("RData", outputs) & !grepl("Rdata", outputs) ){stop("runDirInfo folder is empty")}

  result <- try(lapply(outputs, function(x){
                LPJout <- loadRData(x)
              }), F)
  if ('try-error' %in% class(result)){ stop("Invalid runDirInfo folder")  }

  if (length(result)==1){ result <- result[[1]] }

  return(result)
}

# @title A function to load data and assign a name
# @description  https://stackoverflow.com/questions/5577221/how-can-i-load-an-object-into-a-variable-name-that-i-specify-from-an-r-data-file
# @param x a character string indicating the absolute path to the runInfoDir folder
# @author Ramiro Silveyra Gonzalez but based on https://stackoverflow.com/users/1453172/ricardo work
# @examples \dontrun{
# LPJout <- loadRData("/some/absolute/path/runInfo_2016_08_11_121507/runInfo.RData")
#
# }
loadRData <- function(RdataFile){
  #loads an RData file, and returns it
  load(RdataFile)
  get(ls()[ls() != "RdataFile"])
}
