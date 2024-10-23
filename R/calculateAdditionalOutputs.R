# @title The function to fill the single run object
#
# @description This function TODO
# @param x a LPJData object
# @param output a character string  with the additional outputs to be calculated
#  Default value is all outputs.
# @author Ramiro Silveyra Gonzalez, Maurizio Bagnara,
# @return the LPJData object with the with the additional outputs

#calculateMet <- function(x){
#  if (!class(x)=="LPJData"){
#    stop("Invalid data has been provided")
#  }
#   if (zoo::is.zoo(x@dataTypes$met)){
#       zoo::coredata(x@dataTypes$met) <- zoo::coredata(x@dataTypes$maet) +
#                                          zoo::coredata(x@dataTypes$mevap) +
#                                          zoo::coredata(x@dataTypes$mintercep)
#   }else{
#        x@dataTypes$met <- x@dataTypes$maet + x@dataTypes$mevap + x@dataTypes$mintercep
#   }
#    return(x)
#}





#applyFun <- function(x, fun){
#  result <- try(fun(x), FALSE)
#  if ('try-error' %in% class(result)){
#    warning("Fun could not be applied")
#    return(x)
#  }else{
#    return(result)
#  }
#}

