#' @title A get template function
#' @description  This function returns the LPJ-GUESS templates required for the
#'  writeTemplate function.
#' It also allows users to download the templates.
#' @param type a character string indicating the template name: global,
#'  global_cf, global_cru, europe, europe_cf, europe_cru.
#' @param outputDir a character string indicating path to the output directory
#' @return a template object or template.ins file in the specified folder
#' @export
#' @keywords Rlpj
#' @author Florian Hartig, Ramiro Silveyra Gonzalez
#' @examples \dontrun{
#' template <- getTemplate("global.ins")
#' getTemplate("global.ins", "/home/LPJTemplates/")
#' }
getTemplate <- function(type = NULL, outputDir = NULL){
  if (is.null(type) || type != "global" & type != "global_cf" & type != "global_cru"
      &  type != "europe" &  type != "europe_cf" &  type != "europe_cru"){
    stop("Cannot recognize the template: global?? europe??")
  }
  tmp <- templates[[strsplit(type, "_")[[1]][1] ]][[type]]
  if(is.null(tmp)){
    stop("Please provide a valid template name")
  }
  if (is.null(outputDir)){
    return(tmp)
  }else{
    writeLines(tmp, file.path(outputDir, paste(type, ".ins", sep = "")) )
    return(paste(type, ".ins", sep = ""))
  }
}


#' @title A get paremeter list function
#' @description  This function returns the LPJ-GUESS parameter list required for the writeTemplate function.
#' It also allows users to see the default values of the templates.
#' @param scale a character string indicating whether the parameters are for the global or europe template
#' @return a list with parameter names and their values
#' @export
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




