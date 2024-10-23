#' @title A get type list function
#' @description  This function returns the LPJ-GUESS output type list
#' @return a character vector with the default output types
#' @export
#' @author Ramiro Silveyra Gonzalez, Maurizio Bagnara
#' @example /inst/examples/getTypeListHelp.R
getTypeList <- function(){
  tmp <- typelist.default
  return(tmp)

}
