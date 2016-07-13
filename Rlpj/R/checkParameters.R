# @title A parameter list function
# @description  This function checks the provided parameter list against the
# complete parameters list. If any parameter is not provided, the function will
# add it to the parameter list and return a complete parameter list with the values
# provided as input and the default values for those parameters that were not provided.
# @param scale a character string indicating whether the model runs global or for europe.
# @param parameterList a named list holding the parameter or combination or parameters
#  to be tested.
# @param type serial, parallel, names
# In this case, parameterList is a vector of names.
# @return a named list holding the values of the template parameter.
# @keywords Rlpj
# @author Ramiro Silveyra Gonzalez, Maurizio Bagnara, Florian Hartig
# @examples \dontrun{
# parameterList <- list(run_emax = 4, run_lambda_max = 1)
# parameterList.checked <- checkParameters(scale= "global", parameterList)
# }
checkParameters <- function(scale = NULL, parameterList = NULL, type = "normal"){
  # include check
  if ( scale != "global" & scale != "europe"){
    stop("checkParameters: Cannot recognize the template: neither global nor europe")
  }
  # call the default template
  default <- getParameterList(scale)
  # get names for doing the posterior chekc
  #parameterNames <- names(default)

  # check now
  if( is.null(parameterList)){
    # get the defalut template
    parameterList <- default
  }else{
    if (type =="names"){
      # if inverse I am passing only names
      parameterList <- default[ !names(default) %in% parameterList]
    }else if (type == "parallel"){
      parameterList <- parameterList[names(parameterList) %in% names(default) ]
      if(length(parameterList) == 0){
        stop("Invalid parameters")
      }
    }else if (type == "serial"){
      anyParameter <- parameterList[names(parameterList) %in% names(default) ]
      if(length(anyParameter) == 0){
        stop("Invalid parameters")
      }
      parameterList <- c(parameterList[names(parameterList) %in% names(default) ],
                          default[ !names(default) %in% names(parameterList)])
      #

    }else{
      stop("Invalid parameters")
    }
  }

  return(parameterList)
}

