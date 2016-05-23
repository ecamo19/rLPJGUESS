#' @title A LPJ-GUESS data class object
#' @description  This is a data class object to store both the LPJ-GUESS outputs
#' and the information that was passed to LPJ-GUESS.
#' @return a data class object with the slots runInfo and dataTypes
#' @keywords Rlpj
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


#Defining slot setters1!
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

setMethod(f ="[[",
          signature ="LPJData",
          definition = function(x,i,j,drop){
            if(i=="runInfo"){return(x@runInfo)}else {}
            if(i=="dataTypes"){return(x@dataTypes)}else {}
          }
          )


setGeneric("getDataTypes",
           function(object){
             standardGeneric("getDataTypes")
             }
           )

setMethod(f= "getDataTypes",
          signature ="LPJData",
          definition = function(object){
            return(object@dataTypes)
            }
          )

setMethod (f= "show",
             signature ="LPJData",
             function(object){
                   cat("class              : LPJData\n")
                   cat("LPJ template 1     : ");cat(object@runInfo$template1); cat("\n")
                   cat("grid cells         : ");cat(object@runInfo$gridList); cat("\n")
                   cat("LPJ model outputs  : "); cat(length(names(object@dataTypes)));cat(" outputs\n")
                   cat(names(object@dataTypes), sep = "\t", fill = TRUE)
               }
  )

