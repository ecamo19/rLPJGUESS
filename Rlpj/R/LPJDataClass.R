#' @title A LPJ-GUESS data class object#'
#' @description  This is a data class object
#' @return a data class object
#' @export
#' @keywords Rlpj
#' @author Florian Hartig, Ramiro Silveyra Gonzalez
#' @note Based an older code of Istem Fer, Uni Potsdam
setClass("LPJData",
         representation(
           runInfo = "list",
           dataTypes = "list"
           )
)

LPJData <- function(runInfo=list(run = "This is run one", parameters = "XX paramereters used"),
                    dataTypes = list(aaet = "aaet",  agpp = "agpp")
                      )
  new("LPJData", runInfo=runInfo,  dataTypes=dataTypes)

#Defining slot setters1!
setGeneric("runInfo<-", function(x, value) standardGeneric("runInfo<-"))
setReplaceMethod("runInfo", "LPJData", function(x, value) {x@runInfo <- value; validObject(x); x})

setGeneric("dataTypes<-", function(x, value) standardGeneric("dataTypes<-"))
setReplaceMethod("dataTypes", "LPJData", function(x, value) {x@dataTypes <- value; validObject(x); x})
