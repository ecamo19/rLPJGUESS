#' @title A LPJ-GUESS data class object
#' @description  This is a data class object to store both the LPJ-GUESS outputs
#' and the information that was passed to LPJ-GUESS.
#' @return a data class object with the slots runInfo and dataTypes
#' @keywords Rlpj
#' @author Ramiro Silveyra Gonzalez
setClass("LPJSetup",
         representation(
           clusterType = "character",
           numCores = "numeric",
           mainDir = "character",
           runDir = "character",
           outDir = "character"
           ),
         prototype (
           clusterType= " ",
           numCores = 1,
           mainDir = "",
           runDir = "",
           outDir = ""
           ),
         validity = function(object)	{
           condition1 <- (length(object@runDir) != object@numCores)
           if (condition1) { stop('invalid number of run directories') }
           condition2 <- (length(object@outDir) != object@numCores)
           if (condition2) { stop('invalid number of output directories') }
           return(condition1 & condition2)
         }
)


#' @docType methods
#' @rdname intialize-LPJSetup
#' @title TODO
#' @name TODO
setMethod(f= "initialize",
           signature="LPJSetup",
           definition= function(.Object, clusterType, numCores, mainDir, runDir,
                                outDir){
             .Object@clusterType <- clusterType
             .Object@numCores <- numCores
             .Object@mainDir <- mainDir
             .Object@runDir <- runDir
             .Object@outDir <- outDir
             # validaditate stuff
             #validObject(.Object)
             return(.Object)
             }
)


#Defining slot setters1!
setGeneric("clusterType<-", function(object, value) standardGeneric("clusterType<-"))
setReplaceMethod(f ="clusterType",
                 signature = "LPJSetup",
                 definition = function(object, value){
                   object@runInfo <- value
                   #validObject(object); object
                   }
                 )
setGeneric("numCores<-", function(object, value) standardGeneric("numCores<-"))
setReplaceMethod(f = "numCores",
                 signature ="LPJSetup",
                 definition = function(object, value){
                   object@dataTypes <- value
                   #validObject(object); object
                   }
                 )

setGeneric("mainDir<-", function(object, value) standardGeneric("mainDir<-"))
setReplaceMethod(f = "mainDir",
                 signature ="LPJSetup",
                 definition = function(object, value){
                   object@dataTypes <- value
                   #validObject(object); object
                 }
)

setGeneric("runDir<-", function(object, value) standardGeneric("runDir<-"))
setReplaceMethod(f = "runDir",
                 signature ="LPJSetup",
                 definition = function(object, value){
                   object@dataTypes <- value
                   #validObject(object); object
                 }
)

setGeneric("outDir<-", function(object, value) standardGeneric("outDir<-"))
setReplaceMethod(f = "outDir",
                 signature ="LPJSetup",
                 definition = function(object, value){
                   object@dataTypes <- value
                   #validObject(object); object
                 }
)

setMethod(f ="[[",
          signature ="LPJSetup",
          definition = function(x,i,j,drop){
            if(i=="clusterType"){return(x@clusterType)}else {}
            if(i=="numCores"){return(x@numCores)}else {}
            if(i=="mainDir"){return(x@mainDir)}else {}
            if(i=="runDir"){return(x@runDir)}else {}
            if(i=="outDir"){return(x@outDir)}else {}
          }
          )


setMethod (f= "show",
             signature ="LPJSetup",
             function(object){
                   cat("class              : LPJSetup\n")
                   cat("cluster type       : ");cat(object@clusterType);cat("\n")
                   cat("number of cores    : ");cat(object@numCores);cat("\n")
                   cat("output directories : \n")
                   cat(object@runDir, sep= "\n")
               }
  )

