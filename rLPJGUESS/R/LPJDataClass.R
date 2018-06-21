#' @title A LPJData class object
#'
#' @description  This is a S4 class object to store both the LPJ-GUESS outputs
#' and the information that was passed to LPJ-GUESS.
#'
#' @return a S4 class object with two slots \itemize{
#' \item  runInfo contains a list with the information used to run the model such as templates and parameters
#' \item  dataTypes contains a list with the model outputs
#' }
#' @keywords rLPJGUESS
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



#' Extract parts of LPJData
#'
#' @method [, LPJ class objects
#' @rdname extract-methods
#' @param x an \code{LPJData} or an \code{LPJSetup} object
#' @param i a character string indicating the slot name of an LPJData class object,
#' an LPJ-GUESS output or a character string indicating the slot name of an LPJSetup
#'  class object
#' @param j a character string indicating a sublevel of any slot (only needed if i is provided)
#' @param drop unused
#' @param ... additional arguments (none implemented)
#'
#'
setMethod("[",
          signature(x="LPJData", i='character', j="ANY", drop="ANY"),
          definition = function(x, i, j, ..., drop) {
            if(i=="runInfo"){
              return(x@runInfo)
            }else if(i=="dataTypes"){
              return(x@dataTypes)
            }else{
              value <-x@dataTypes[[i]]
              if (is.null(value)){
                value <-x@runInfo[[i]]
              }
              return(value)
            }
          }
          )

#' Replace parts of LPJData
#'
#' @method [<-, LPJ class objects
#' @rdname replace-methods
#' @param x an \code{LPJData} object
#' @param i a character string indicating the LPJ-GUESS output
#' @param value any value to create or replace and existing
#' @param j a character string indicating a sublevel of the dataType slot (only needed if i is provided)
#' @param ... additional arguments (none implemented)
#'
#'
setMethod("[<-",
          signature(x="LPJData", i='character', j="ANY"),
          definition = function(x, i, j, ... ) {
          if(i=="runInfo"){
              x@runInfo <- value
              return(x)
          }else if(i=="dataTypes"){
              x@dataTypes <- value
              return(x)
          }else{
              x@dataTypes[i] <- value
              return(x)
            }
          }
)


setMethod (f= "show",
             signature ="LPJData",
             function(object){
                   cat("class              : LPJData\n")
                   if( length(object@runInfo$runI) > 0){
                     cat("run ID             : ");cat(object@runInfo$runID); cat("\n")
                   }
                   cat("run directory      : ");cat(object@runInfo$runDir); cat("\n")
                   cat("LPJ template 1     : ");cat(object@runInfo$template1); cat("\n")
                   cat("LPJ template 2     : ");cat(object@runInfo$template2); cat("\n")
                   if( length(object@runInfo$gridListCell) > 0){
                     cat("grid cells         : ");cat(length(object@runInfo$gridListCell));cat(" cell(s) \n");cat(object@runInfo$gridListCell, sep = "\n")
                   }
                   cat("LPJ model outputs  : "); cat(length(names(object@dataTypes)));cat(" output(s)\n")
                   cat(sort(names(object@dataTypes)), sep = ", ", fill = TRUE)
               }
  )


