# @title A check parallel function
# @description  This function checks whether the request parallelization meets the
# the provided inputs (grids and parameterList)
# @param userParallel user parallel request
# @param number of grids
# @param parameters number of parameter combinations
# @keywords rLPJGUESS
# @author Ramiro Silveyra Gonzalez
# @examples \dontrun{
# parameterList <- list(run_emax = 4, run_lambda_max = 1)
# parameterList.checked <- checkParameters(scale= "global", parameterList)
# }


checkParallel <- function(userParallel, grids, parameters){

  # Find out
  parallel <- "serial"
  if (grids == 1 & parameters > 1){
      parallel <- "parameters"
  }else if (grids > 1 & parameters == 1){
    parallel <- "grids"
  }else if (grids > 1 & parameters > 1){
    parallel <- "both"
  }else{
    warning("Please check the number of grids and/or parameters provided")
    warning(paste("Number of grids =", grids, " number of parameter combinations =", parameters, sep = " " ))
    stop("Your requested parallelization is not feasible")
  }

  # Compare to user request
  if (userParallel != "auto"){
    if (parallel  == "both"){
      if (userParallel == "grids"){
        warning("Please check the number of grids and/or parameters provided")
        warning(paste("Number of grids =", grids, " number of parameter combinations =", parameters, sep = " " ))
        stop("Your requested parallelization is not feasible")
      }else if (userParallel == "parameters"){
        warning("Please check the number of grids provided")
        warning(paste("Number of grids =", grids,  sep = " " ))
        parallel <- userParallel
      }
    }else if(parallel !=  userParallel) {
      warning("Please check the number of grids and/or parameters provided")
      warning(paste("Number of grids =", grids, " number of parameter combinations =", parameters, sep = " " ))
      stop("Your requested parallelization is not feasible")
    }
  }
  if (parallel == "serial"){stop("BUG!")}
  return(parallel)
}
