#' @title A get parameter list function
#' @description  This function returns the LPJ-GUESS parameter list required for the writeTemplate function.
#' It also allows users to see the default values of the templates.
#' @param scale a character string indicating whether the parameters are for the global or europe template
#' @param list a character boolean to specify the returned format. If TRUE the parameter will be returned
#' as a list, otherwise as a matrix.
#' @return a list or a matrix with parameter names and their values
#' @export
#' @author Ramiro Silveyra Gonzalez, Maurizio Bagnara
#' @examples \dontrun{
#' parameterList <- getParameterList("global")
#' }
getParameterList <- function(scale, list =  TRUE){
  if ( is.null(scale) || scale != "global" & scale != "europe"){
    stop("Please provide a valid scale: global or europe")
  }
  tmp <- parameterList.default[[scale]]


  if (list){
    tmp.names <- rownames(tmp)
    tmp <- as.vector(tmp, mode = "list")
    names(tmp) <- tmp.names
  }
  return(tmp)
}




