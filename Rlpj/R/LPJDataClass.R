#' @title A LPJData class object
#'
#' @description  This is a S4 class object to store both the LPJ-GUESS outputs
#' and the information that was passed to LPJ-GUESS.
#'
#' @return a S4 class object with two slots \itemize{
#' \item  runInfo contains a list with the information used to run the model such as templates and parameters
#' \item  dataTypes contains a list with the model outputs
#' }
#' @keywords Rlpj
#' @export
#' @author Ramiro Silveyra Gonzalez
setClass("LPJData",
         representation(
           runInfo = "list",
           dataTypes = "list"
           ),
         prototype (
           runInfo= list(),
           dataTypes = list()
         )
)

setMethod(f= "initialize",
           signature="LPJData",
           definition= function(.Object,
                                runInfo=list(run = "This is run one", parameters = "XX paramereters used"),
                                dataTypes = list(par = "parameters")){
            .Object@runInfo <- runInfo
            .Object@dataTypes <- dataTypes
             # validaditate stuff
             #validObject(.Object)
             return(.Object)
             }
)



setGeneric("runInfo<-", function(object, value) standardGeneric("runInfo<-"))
setReplaceMethod(f ="runInfo",
                 signature = "LPJData",
                 definition = function(object, value){
                   object@runInfo <- value
                   #validObject(object); object
                   }
                 )

setGeneric("dataTypes<-", function(object, value) standardGeneric("dataTypes<-"))
setReplaceMethod(f = "dataTypes",
                 signature ="LPJData",
                 definition = function(object, value){
                   object@dataTypes <- value
                   #validObject(object); object
                   }
                 )
# Extract parts of LPJData
#
#setMethod(f ="[[",
#          signature(x = "LPJData", i = "ANY", j="ANY"),
#          definition = function(x,i,j,drop){
#            if(i=="runInfo"){return(x@runInfo)}else {}
#            if(i=="dataTypes"){return(x@dataTypes)}else {}
#          }
#          )


setMethod (f= "show",
             signature ="LPJData",
             function(object){
                   cat("class              : LPJData\n")
                   cat("LPJ template 1     : ");cat(object@runInfo$template1); cat("\n")
                   cat("LPJ template 2     : ");cat(object@runInfo$template2); cat("\n")
                   cat("grid cells         : ");cat(object@runInfo$gridListMem); cat("\n")
                   cat("run directory      : ");cat(object@runInfo$runDir); cat("\n")
                   cat("LPJ model outputs  : "); cat(length(names(object@dataTypes)));cat(" outputs\n")
                   cat(names(object@dataTypes), sep = "\t", fill = TRUE)
               }
  )

