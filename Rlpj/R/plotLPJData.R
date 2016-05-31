#' @title A plot function for LPJdata objects
#' @description  This function reads data from a LPJData object and plots the
#'  variables against time. Plots are saved in the output folder, if the save.plots is set to TRUE.
#' @param x a LPJData object.
#' @param outDir a character string indicating the folder where the plots will be
#' saved, if save.plot set to TRUE
#' @param save.plots a boolean indicating whether the plots are saved in the outDir
#' @param typeList a character vector with the outputs to be plotted
#' @param prefix a character string specifying the prefix to be added to the plots files.
#' Only relevant if saving plots
#' @return plots for data types included in typeList. The grid cells will be plotted independently
#' @seealso \url{https://cran.r-project.org/web/packages/zoo/zoo.pdf}
#' @export
#' @keywords Rlpj
#' @author Florian Hartig, Ramiro Silveyra Gonzalez
#' @examples \dontrun{
#' plotLPJData(data, typeList = c("aaet", "lai"),
#'  outDir = "/runDir/outDir", save.plots = FALSE)
#' }
plotLPJData <- function(x=NULL, typeList = NULL, outDir= NULL,
                     save.plots = FALSE, prefix = ""){

  # checking input parameters
  if (is.null(x)){
    stop("No data has been provided")
  }
  if (!class(x)=="LPJData"){
    stop("Invalid data has been provided")
  }
  if (save.plots){
    if ( is.null(outDir) || !file.exists(outDir)){
      stop("No outDir has been provided")
    }
  }
  if (!requireNamespace("zoo", quietly = TRUE)){
    stop("Can't load required library 'zoo'.")
  }

  data <- x@dataTypes
  # Plot from
  typeList.available <- names(data)
  if (is.null(typeList) || !class(typeList) == "character"){

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
    # check how many coordinates
    coordinates <- unique(paste(df$Lat, df$Lon, sep = "_"))
    if(length(coordinates) > 1 ){
      # if only one grid, simplify the list
      #      if (length(listData) == 1){
      # listData <- listData[[1]]
      # Adding Data to listData
      # looping over data types, reading files, processing data and adding it to the Data Class
      # list append data, probably will have to use the name() function to give it the right name
      # in the end, make the list of class LPJData
      # Add data to LPJout Data class
      coordinates <- lapply(coordinates, function(x){as.numeric(unlist(strsplit(x, "_")))})
      #keep <- rep(TRUE, length(coord))
      #sub_data <- coord[coord>=min(lon.extent) & Lon<=max(lon.extent) & Lat <=max(lat.extent) & Lat>=min(lat.extent))
      for (k in 1:length(coordinates)){
        df_subset <- df[df$Lat==coordinates[[k]][1] & df$Lon==coordinates[[k]][2],]
        if(zoo::is.zoo(df_subset) == FALSE){
          df_subset <- convertTS(df_subset)
        }
        if (save.plots){
          png(file.path(outDir, paste(prefix, typeList.valid[[i]], ".png", sep="")),width=1000,height=750)
          if(length(colnames(df_subset))==1){
            plot(df_subset, main =paste("Grid", coordinates[[k]][1], coordinates[[k]][2],
                                        "Variable:", typeList.valid[[i]]),xlab="Years", ylab="NULL")
          }else{
            plot(df_subset, main =paste("Grid", coordinates[[k]][1], coordinates[[k]][2],
                                        "Variable:", typeList.valid[[i]]),xlab="Years")
          }
          dev.off()
        }else{
          if(length(colnames(df_subset))==1){
            plot(df_subset,  main =paste("Grid", coordinates[[k]][1], coordinates[[k]][2],
                                         "Variable:", typeList.valid[[i]]),xlab="Years", ylab="NULL")
          }else{
            plot(df_subset,  main =paste("Grid", coordinates[[k]][1], coordinates[[k]][2],
                                  "Variable:", typeList.valid[[i]]),xlab="Years")
          }
        }
      }
    }else{
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
}






