#' @title A get type list function
#' @description  This function returns the LPJ-GUESS output type list
#' @return a character vector with the default output types
#' @export
#' @examples \dontrun{
#' typelist <- getTypeList()
#' }
getTypeList <- function(){
  tmp <- typelist.default
  return(tmp)

}
