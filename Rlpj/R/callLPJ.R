#' @title A function to call the LPJ modell
#' @description This function does a system call, passing a template to the LPJ
#'  model.
#' @param mainDir a character string indicating the path to LPJ
#' @param runDir a character string indicating the path to the runDirectory
#' @param template2 a character string providing the  "specific" model template,
#'  e.g, global_cf.ins or global_cru.ins. Provide only the file name, not the path.
#'  If not provided, package templates will be used
#' @param mode a character string indicating whether using cru or cf data
#' @return none
#' @keywords Rlpj
#' @author Florian Hartig, Ramiro Silveyra Gonzalez
#' @note Based an older code of Istem Fer, Uni Potsdam
#' @examples \dontrun{
#' callLPJ("/home/LPJrun", "/home/LPJrun/runDirectory1", "global_cru.ins")
#' }
callLPJ <- function(mainDir = NULL, runDir = NULL, template2 = NULL, mode = NULL){
  #----------------------------------------------------------------------------#
  # CHECK INPUTS AND EXIT IF ANY ERROR
  #----------------------------------------------------------------------------#
  # Checking provided parameters
  if (is.null(mainDir) || !file.exists(mainDir)){
    stop ("Please provide a valid main directory")
  }
  if (is.null(runDir) || !file.exists(runDir)){
    stop ("Please provide a valid run directory")
  }
  if (is.null(template2) ||  !file.exists(file.path(runDir, template2))){
    stop ("Please provide a valid template name")
  }
  if (is.null(mode) || mode != "cf" & mode != "cru"){
    stop("Please provide a valid cluster type: cf or cru")
  }

  #----------------------------------------------------------------------------#
  # CALL MODELL
  #----------------------------------------------------------------------------#
  # starting the function itself
  # system call
  if (Sys.info()[["sysname"]] == "Windows"){
    submit <- paste(file.path(mainDir, "guesscmd.exe"), "-input", mode,
                    file.path(runDir, template2), sep = " ")
  }else{
    submit <- paste(file.path(mainDir, "guess"), "-input", mode,
                    file.path(runDir, template2), sep = " ")
  }
  # here I try-catch?
  cat("Calling LPJ-GUESS")
  cat(paste(submit, "\n", sep = ""))
  try(system (submit, intern = TRUE), FALSE)
}
