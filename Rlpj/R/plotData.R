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
plotData <- function(data = NULL, typeList = NULL, outDir= NULL,
                     save.plots = FALSE){

  # checking input parameters
  if (is.null(data)){
    stop("No data has been provided")
  }
  if(grepl("Rdata", data )==TRUE){
    if(!file.exists(data)){
      stop("No valid data has been provided")
    }else{
      load(data)
      # Check what is available
      data <- runObject$output
    }
  }
  if (save.plots){
    if ( is.null(outDir) || !file.exists(outDir)){
      stop("No outDir has been provided")
    }
  }
  if (!requireNamespace("zoo", quietly = TRUE)){
    stop("Can't load required library 'zoo'.")
  }
  # Plot from
  typeList.available <- names(data)
  if (is.null(typeList)){
    cat("\nNo typeList has been provided")
    cat("\nWriting out all data")
    typeList.valid <- typeList.available
  }else{
    keep <- rep(FALSE, length(typeList))
    for (i in 1:length(typeList)){
      if (typeList[i] %in% typeList.available){
        keep[i] <- TRUE
      }
    }
    if (any(keep)){
      typeList.valid <- typeList[keep]
    }else{
      stop("None of the requested output types exists")
    }
  }
  ## if plotting true, start the fucntion
  # Checking existentce of data types # in theory do not need if your plotting from class object
  for (i in 1:length(typeList.available)){
    df <- data[[typeList.available[[i]] ]]
    if (save.plots){
      png(file.path(outDir, paste(typeList.available[[i]], ".png", sep="")),width=1000,height=750)
      if(length(colnames(df))==1){
        plot(zoo::zoo(df, rownames(df)), main =paste("Variable:", typeList.available[[i]]),xlab="Years", ylab="")
      }else{
        plot(zoo::zoo(df, rownames(df)), main =paste("Variable:", typeList.available[[i]]),xlab="Years")
      }
      dev.off()
    }else{
      if(length(colnames(df))==1){
        plot(zoo::zoo(df, rownames(df)), main =paste("Variable:", typeList.available[[i]]),xlab="Years", ylab="")
      }else{
        plot(zoo::zoo(df, rownames(df)), main =paste("Variable:", typeList.available[[i]]),xlab="Years")
      }
    }
  }
}






