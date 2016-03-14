#' @title A function to call the LPJ modell'
#' @description This function does a system call, passing a template to the LPJ
#'  model.
#' @param mainDir a character string indicating the path to LPJ
#' @param runDir a character string indicating the path to the runDirectory
#' @param template2 a character string providing the  "specific" model template,
#'  e.g, global_cf.ins or global_cru.ins. Provide only the file name, not the path.
#'  If not provided, package templates will be used.
#' @return none
#' @export
#' @keywords Rlpj
#' @author Florian Hartig, Ramiro Silveyra Gonzalez
#' @note Based an older code of Istem Fer, Uni Potsdam
#' @examples \dontrun{
#' runLPJ("/home/LPJrun", "/home/LPJrun/runDirectory1", "global_cru.ins")
#' }
runLPJ <- function(mainDir = NULL, runDir = NULL, template2 = NULL){

  # Checking provided parameters
  if (is.null(mainDir) || !file.exists(mainDir)){
    stop ("Please provide a valid main directory.")
  }
  if (is.null(runDir) || !file.exists(runDir)){
    stop ("Please provide a valid run directory.")
  }
  if (is.null(template2) ||  !file.exists(file.path(runDir, template2))){
    stop ("Please provide a valid template name.")
  }

  # starting the function itself
  # checking what input file is provided
  model.input <- NULL
  if (grepl("cf", template2)){
    model.input <- "cf"
  }else if(grepl("cru", template2)){
    model.input <- "cru"
  }else{
    stop("runLPJ: Cannot recognize the template!!")
  }
  # system call
  if (Sys.info()[["sysname"]] == "Windows"){
    submit <- paste(file.path(mainDir, "guesscmd.exe"), "-input", model.input,
                    file.path(runDir, template2), sep = " ")
  }else{
    submit <- paste(file.path(mainDir, "guess"), "-input", model.input,
                    file.path(runDir, template2), sep = " ")
  }
  cat(paste(submit, "\n", sep = ""))
  # here I try-catch?
  tryCatch(
  {# call the system
    message("Calling LPJ-GUESS")
    system (submit, intern = TRUE)
  },
  error=function(cond) {
    message("Error when calling LPJ-GUESS")
    message("Here is your call:")
    message(submit)
    message("Here's the original error message:")
    message(cond)
    # Choose a return value in case of error
  },
  warning=function(cond) {
    message("Your call caused a warning:")
    message(submit)
    message("Here's the original warning message:")
    message(cond)
    # Choose a return value in case of warning
  }
  )
}
