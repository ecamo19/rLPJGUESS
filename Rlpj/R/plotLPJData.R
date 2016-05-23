#' @title A plot function for LPJdata objects
#' @description  This function reads data from a LPJ data object, turns it into
#' a time series with zoo and plots the variables against time.
#' Plots are saved in the output folder, if the save.plots boolean is set to TRUE.
#' Data in the data object is already a time series object, but stored as a matrix.
#' @param data a list of data contained in the LPJ data object.
#' @param outDir a character string indicating the folder where the plots will be
#' saved, if save.plot set to TRUE
#' @param save.plots a boolean indicating whether the plots are saved in the outDir
#' @param typeList a character vector with the outputs to be plotted
#' @param prefix a character string specifying the prefix to be added to the plots files.
#' Only relevant if saving plots.
#' @return plots for data types included in typeList
#' @seealso \url{https://cran.r-project.org/web/packages/zoo/zoo.pdf}
#' @export
#' @keywords Rlpj
#' @author Florian Hartig, Ramiro Silveyra Gonzalez
#' @examples \dontrun{
#' plotLPJData(dataList = list( aaet = aaet), typeList = c("aaet"),
#'  outDir = "/runDir/outDir", save.plots = FALSE)
#' }
plotLPJData <- function(data = NULL, typeList = NULL, outDir= NULL,
                     save.plots = FALSE, prefix = ""){

  # checking input parameters
  if (is.null(data)){
    stop("No data has been provided")
  }
  if (save.plots){
    if ( is.null(outDir) || !file.exists(outDir)){
      stop("No outDir has been provided")
    }
  }
  if (!requireNamespace("zoo", quietly = TRUE)){
    stop("Can't load required library 'zoo'.")
  }

  #if(grepl("Rdata", data )==TRUE){
  #  if(!file.exists(data)){
  #    stop("No valid data has been provided")
  #  }else{
  #    load(data)
      # Check what is available
  #    data <- runObject$output
  #  }
  #}
  # Plot from
  typeList.available <- names(data)
  if (is.null(typeList)){
    cat("\nNo typeList has been provided")
    cat("\nPlotting all data")
    typeList.valid <- typeList.available
  }else{
    keep <- rep(FALSE, length(typeList))
    for (i in 1:length(typeList)){
      if (typeList[i] %in% typeList.available){
        keep[i] <- TRUE
      }
    }
    if (any(keep)){
      typeList.valid <-typeList[keep]
    }else{
      stop("None of the requested output types exists")
    }
  }
  ## if plotting true, start the fucntion
  # Check if data is a df
  # Checking existentce of data types # in theory do not need if your plotting from class object
  for (i in 1:length(typeList.valid)){
    df <- data[[typeList.valid[[i]] ]]
    # something like is zoo
    if(zoo::is.zoo(df) == FALSE){
      df <- convertTS(df)
    }
    if (save.plots){
      png(file.path(outDir, paste(prefix, typeList.valid[[i]], ".png", sep="")),width=1000,height=750)
      if(length(colnames(df))==1){
        plot(df, main =paste("Variable:", typeList.valid[[i]]),xlab="Years", ylab="NULL")
      }else{
        plot(df, main =paste("Variable:", typeList.valid[[i]]),xlab="Years")
      }
      dev.off()
    }else{
      if(length(colnames(df))==1){
        plot(df,  main =paste("Variable:", typeList.valid[[i]]),xlab="Years", ylab="NULL")
      }else{
        plot(df,  main =paste("Variable:", typeList.valid[[i]]),xlab="Years")
      }
    }
  }
}






