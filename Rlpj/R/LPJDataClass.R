#' @title A LPJ-GUESS data class object
#' @description  This is a data class object to store both the LPJ-GUESS outputs
#' and the information that was passed to LPJ-GUESS.
#' @return a data class object with the slots runInfo and dataTypes
#' @export
#' @keywords Rlpj
#' #' @author Ramiro Silveyra Gonzalez
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
