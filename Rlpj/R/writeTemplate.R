#' @title A writing template function for LPJ
#' @description  This function reads a template, and replaces parameters with the
#' provides parameters list.If any parameters values is not provided, the function will
#' set it to the default values. The function assumes that a copy of the template
#'  is already placed in the run folder.
#' @param template1 a character string providing the general model template,
#'  e.g, global.ins. Provide only the file name, not the path.
#' @param parameterList  a named list containing the parameters to be calibrated
#' @param runDir a character string indicating path to the run directory
#' @return none
#' @export
#' @keywords Rlpj
#' @author Florian Hartig, Ramiro Silveyra Gonzalez, Maurizio Bagnara
#' @note Based an older code of Istem Fer, Uni Potsdam
#' @examples \dontrun{
#' writeTemplate("global.ins", list(run_lamda_max = 0.5, run_emax= 5),
#'               "/home/lpjRun/runDirectory1")
#' }
writeTemplate <- function(template1 = NULL, parameterList = NULL, runDir = NULL){

  # Checking provided parameters
  if (is.null(runDir) || !file.exists(runDir) ){
    stop("Please provide a valid run directory.")
  }
  if (is.null(template1) || !file.exists(file.path(runDir, template1))){
    stop("The provided template  does not exits. Please provide a template name.")
  }
  # call the function
  if (grepl("global",template1)){
      # Checking paramaterList and if null, setting to default values
      # 256 physiological parameters can be calibrated right now. Exceptions are the fine roots distributions for all PFTs
      parameterList <- checkParameters(scale= "global", parameterList)
  }else if (grepl("europe", template1)){
      # Checking paramaterList and if null, setting to default values
      # 585 physiological parameters, both general and species-specific, can be calibrated right now.
      # Exceptions are the fine roots distributions for all species and PFTs
      parameterList <- checkParameters(scale = "europe", parameterList)
  }else{
    stop("writeParameters: Cannot recognize the template: global?? europe??")
  }
  # getting parameters names
  parameterNames <- names(parameterList)
  # looping over parameters ## Faster
  template <- readLines(file.path(runDir, template1))
  for(i in 1:length(parameterNames))  {
    template <- sub(parameterNames[i], parameterList[i], template)
  }
  writeLines(template, file.path(runDir, template1))
}


#' @title A parameter list function
#' @description  This function checks the provided parameter list against the
#' the complete parameters list. If any parameter is not provided, the fucntion will
#' add it to the paramter list and return a complete parameter list with the values
#' provided as input and the default values for those parametes that were not provided.
#' @param scale a character string indicating whether the model runs global or for europe.
#' @param parameterList a named list holding the parameter or combination or parameters
#'  to be tested.
#' @return none
#' @details TODO (names of list)
#' @export
#' @keywords Rlpj
#' @author Florian Hartig, Ramiro Silveyra Gonzalez, Maurizio Bagnara
#' @examples \dontrun{
#' parameterList <- list(run_emax = 4, run_lambda_max = 1)
#' parameterList.checked <- checkParameters(scale= "global", parameterList)
#' }
checkParameters <- function(scale = NULL, parameterList = NULL){
    # include check
  if ( scale != "global" & scale != "europe"){
    stop("Cannot recognize the template: global?? europe??")
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

