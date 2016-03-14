#' @title A plot function for LPJdata objects
#' @description  This function reads data from a LPJ data object, turns it into
#' a time series with zoo and plots the variables against time.
#' Plots are saved in the output folder, if the save.plots boolean is set to TRUE.
#' Data in the data object is already a time series object, but stored as a matrix.
#' @param dataList a list of matrices contained in the LPJ data object.
#' @param outDir a character string indicating the folder where the plots will be
#' saved, if save.plot set to TRUE
#' @param save.plots a boolean indicating whether the plots are saved in the outDir
#' @param typeList a character vector with the outputs to be plotted
#' @return plots for data types included in typeList
#' @seealso \url{https://cran.r-project.org/web/packages/zoo/zoo.pdf}
#' @export
#' @keywords Rlpj
#' @author Florian Hartig, Ramiro Silveyra Gonzalez
#' @examples \dontrun{
#' plotData(dataList = list( aaet = aaet), typeList = c("aaet"),
#'  outDir = "/runDir/outDir", save.plots = FALSE)
#' }
plotData <- function(dataList = NULL, typeList = NULL, outDir= NULL, save.plots = TRUE){

  # checking input parameters
  if (is.null(typeList)){
    stop("No typeList has been provided")
  }
  if (save.plots){
    if ( is.null(outDir) || !file.exists(outDir)){
      stop("No outDir has been provided")
    }
  }
  if (!requireNamespace("zoo", quietly = TRUE)){
    stop("Can't load required library 'zoo'.")
  }
  ## if plotting true, start the fucntion
  # Checking existentce of data types # in theory do not need if your plotting from class object
  for (i in 1:length(typeList)){
    data <- dataList[[typeList[[i]] ]]
    if (save.plots){
      png(file.path(outDir, paste(typeList[[i]], ".png", sep="")),width=1000,height=750)
      plot(zoo::zoo(data, rownames(data)), main =paste("Variable:", typeList[[i]]),xlab="Years" )
      dev.off()
    }else{
      plot(zoo::zoo(data, rownames(data)), main =paste("Variable:", typeList[[i]]),xlab="Years" )
    }
  }
}






