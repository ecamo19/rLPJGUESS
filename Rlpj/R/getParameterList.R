#' @title A get paremeter list function
#' @description  This function returns the LPJ-GUESS parameter list required for the writeTemplate function.
#' It also allows users to see the default values of the templates.
#' @param scale a character string indicating whether the parameters are for the global or europe template
#' @return a list with parameter names and their values
#' @export
#' @author Ramiro Silveyra Gonzalez, Maurizio Bagnara
#' @examples \dontrun{
#' parameterList <- getParameterList("global")
#' }
getParameterList <- function(scale = NULL){
  if ( is.null(scale) || scale != "global" & scale != "europe"){
    stop("Please provide a valid scale: global or europe")
  }
  tmp <- parameterList.default [[scale]]
  return(tmp)
}




