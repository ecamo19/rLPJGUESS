# @title A parameter list function
# @description  This function checks the provided parameter list against the
# complete parameters list. If any parameter is not provided, the function will
# add it to the parameter list and return a complete parameter list with the values
# provided as input and the default values for those parameters that were not provided.
# @param scale a character string indicating whether the model runs global or for europe.
# @param parameterList a named list holding the parameter or combination or parameters
#  to be tested.
# @return a named list holding the values of the template parameter.
# @keywords Rlpj
# @author Ramiro Silveyra Gonzalez, Maurizio Bagnara, Florian Hartig
# @examples \dontrun{
# parameterList <- list(run_emax = 4, run_lambda_max = 1)
# parameterList.checked <- checkParameters(scale= "global", parameterList)
# }
checkParameters <- function(scale = NULL, parameterList = NULL){
  # include check
  if ( scale != "global" & scale != "europe"){
    stop("checkParameters: Cannot recognize the template: neither global nor europe")
  }
  # call the default template
  default <- parameterList.default[[scale]]
  # get names for doing the posterior chekc
  parameterNames <- names(default)
  # check now
  if( is.null(parameterList)){
    # get the defalut template
    parameterList <- default
  }else{
    # check the provided parameter list
    # if any paramater is not provided, then added it.
    for (i in 1:length(parameterNames)){
      if (is.null(parameterList[[parameterNames[i]]])){
        parameterList[[parameterNames[i]]] <- default[[parameterNames[i]]]
      }
    }
  }
  return(parameterList)
}

