#' @title A get template function
#' @description  This function returns the LPJ-GUESS templates.
#' @param type a character string indicating the template name: global,
#'  global_cf, global_cru, europe, europe_cf, europe_cru
#' @param outputDir a character string indicating path to the output directory  (optional)
#' @return a template object or template.ins file in the specified folder
#' @export
#' @keywords Rlpj
#' @author Ramiro Silveyra Gonzalez, Maurizio Bagnara, Florian Hartig
#' @examples \dontrun{
#' template <- getTemplate("global.ins")
#' getTemplate("global.ins", "/home/LPJTemplates/")
#' }
getTemplate <- function(type, outputDir = NULL){
  if (is.null(type) || type != "global" & type != "global_cf" & type != "global_cru"
      &  type != "europe" &  type != "europe_cf" &  type != "europe_cru"){
    stop("Invalid template type")
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
