#' @title A function to process LPJ outputs
#' @description This function reads the ASCII outputs produced by LPJ-GUESS.
#' It takes a list of output types (typeList) and finds them in the specified path.
#' The data is stored as a matrix with the shape of a zoo time series and
#' packed in a data class object: LPJData.
#' @param typeList  a character vector with the outputs to be analyzed.
#' Default value is all outputs that were cretaed
#' @param outDir a character string indicating path to the output directory
#' @param runInfo, a named list with the information of the LPJ run.
#' The runInfo it will be stored by the function as RData along with
#' the processed outputs of the model
#' @param processing a boolean indicating whether output files will be turned into time series
#' @return the processed data returned in a S4 Class: LPJData Class
#' @seealso \url{https://cran.r-project.org/web/packages/zoo/zoo.pdf}
#' @author Florian Hartig, Ramiro Silveyra Gonzalez, Maurizio Bagnara
#' @note Based on an older code of Joerg Steinkamp
#' @export
#' @examples \dontrun{
#' LPJout <- getData( typeList = c("aaet",  "cflux","lai", "nflux"),
#'           "~/path/to/output/files", runInfo = list(parameter1 = 0.5, grid = 1))
#' }
getData <- function(typeList = NULL, outDir=NULL, runInfo=NULL, processing = TRUE){
                    #lon.extent=c(-180, 180), lat.extent=c(-90, 90),
                    #area.weighted=FALSE, year.offset=0 ) {
  # @param lon.extent a numeric vector containing the min and max values used
  #  for west-east extent(default -180 to 180)
  # @param lat.extent a numeric vector containing the max and min values used
  #  for north-south extent (default 90 to -90).
  # @param area.weighted a boolean indicating whether the gridcells should be
  # weighted by their size (regular grids only, default FALSE).
  # @param year.offset a integer indicating the value to be added to the 'Year'
  #  column in the LPJ-GUESS output.
  # checking provided parameters
  #----------------------------------------------------------------------------#
  # CHECK INPUTS AND EXIT IF ANY ERROR
  #----------------------------------------------------------------------------#
  if (is.null(outDir) || !file.exists(outDir)){
    stop("Please provide a valid output directory.")
  }
  if (is.null(typeList == TRUE)){
    typeList <- typelist.default
    warning("Output type list has not been provided.")
    warning("Setting type list to default values:\n")
  }
  # checking directory existence
  if (!requireNamespace("zoo", quietly = TRUE)){
    stop("Can't load required library 'zoo'.")
  }
  #----------------------------------------------------------------------------#
  # CHECK OUTPUTS IF NONE DATA IS NA
  #----------------------------------------------------------------------------#
  run.function <- TRUE
  # Finding the files in path and checking if they exist
  keep <- rep(FALSE, length(typeList))
  for (i in 1:length(typeList)){
    if (file.exists(file.path(outDir, paste(typeList[[i]], ".out", sep="")))){
        if ( file.info( file.path(outDir, paste(typeList[[i]], ".out", sep="")) )[['size']] == 0){
        message( paste("The",  typeList[[i]], ".out is empty!\n", sep = " ") )
      }else{
        keep[i] <- TRUE
      }
    }else{
      message( paste("There is no ",  typeList[[i]], ".out\n", sep = "") )
      }
    }
  if (any(keep)){
    typeList.valid <- typeList[keep]
    # Creating a list to hold data
    listData <- vector(mode="list", length=length(typeList.valid))
    names(listData) <- typeList.valid
  }else{
    warning("There are not model outputs. Please check the guess.log files.")
    run.function <- FALSE
    listData  <- list(NA)
  }

    # storing run info
  #LPJout@runInfo <- runInfo
  #----------------------------------------------------------------------------#
  # OBTAIN OUTPUTS:
  #----------------------------------------------------------------------------#
  if(run.function){
    # starting tclass!

    if (processing == FALSE){
      # Adding Data to listData
      # looping over data types, reading files,  no processing data and adding it to the Data Class
      # list append data, probably will have to use the name() function to give it the right name
      # in the end, make the list of class LPJData
      # Add data to LPJout Data class
      for (j in 1:length(typeList.valid)) {
        data <- read.table(file.path(outDir, paste( typeList.valid[[j]], ".out", sep="")),header=T)
        listData[[typeList.valid[[j]]]] <- data
      }
    }else{
      # Chekc how many grids in output
      data <- read.table(file.path(outDir, paste( typeList.valid[[1]], ".out", sep="")),header=T)
      coordinates <- unique(paste(data$Lat, data$Lon, sep = "_"))
      listData <- rep(list(listData), length(coordinates))
      names(listData) <- paste("grid", coordinates, sep = "_") # will need better named
      # Adding Data to listData
      # looping over data types, reading files, processing data and adding it to the Data Class
      # list append data, probably will have to use the name() function to give it the right name
      # in the end, make the list of class LPJData
      # Add data to LPJout Data class
      coordinates <- lapply(coordinates, function(x){as.numeric(unlist(strsplit(x, "_")))})
      #keep <- rep(TRUE, length(coord))
      #sub_data <- coord[coord>=min(lon.extent) & Lon<=max(lon.extent) & Lat <=max(lat.extent) & Lat>=min(lat.extent))
      for (k in 1:length(listData)){
        for (j in 1:length(typeList.valid)) {
          # reading output
          data <- read.table(file.path(outDir, paste(typeList.valid[[j]], ".out", sep="")),header=T)
          data.ts  <- convertTS(data)
          # choosing the spatial subset
          #data <- subset(data, Lon>=min(lon.extent) & Lon<=max(lon.extent) & Lat <=max(lat.extent) & Lat>=min(lat.extent))
          #data <- data[data$Lat==coordinates[[k]][1] & data$Lon==coordinates[[k]][2],]
          # setting annual true or false
          #annual <- TRUE
          #if (colnames(data)[4] == "Jan"){
          #  annual <- FALSE
          #}
          #data$Year <- data$Year + year.offset
          # create the area weight if desired
          #data$area <- 1.
          #if (area.weighted) {
          #  data$area <- NA
          #  uniq.lon <- sort(unique(data$Lon))
          #  uniq.lat <- sort(unique(data$Lat), decreasing = TRUE)
          #  uniq.lon <- seq(min(uniq.lon), max(uniq.lon),
          #                  min(uniq.lon[2:length(uniq.lon)] - uniq.lon[1:(length(uniq.lon)-1)]))
          #  uniq.lat <- seq(max(uniq.lat), min(uniq.lat),
          #                  max(uniq.lat[2:length(uniq.lat)] - uniq.lat[1:(length(uniq.lat)-1)]))
          #  area1d <- gridarea1d(uniq.lat, abs(uniq.lon[2]-uniq.lon[1]))*1.e-6
          #  for (i in 1:length(uniq.lat))
          #    data$area[data$Lat == uniq.lat[i]] = area1d[i]
          #}
          #uniq.year <- sort(unique(data$Year))
          #cnames <- colnames(data)
          #data.tmp <- NULL
          #for (l in 1:length(uniq.year)){
          #  data.tmp <- rbind(data.tmp, data.frame(t(colMeans(data[data$Year == uniq.year[l], ]))))
          #}
          # remove the unused columns
          #data <- data.tmp[, !(cnames=="Lon" | cnames=="Lat" | cnames=="Year" | cnames=="area")]
          #rm(data.tmp)
#          if (annual) {
            #if annual remove columns with unique values
            # keep <- rep(TRUE, ncol(data))
            # remove columns with unique values
            # for (i in 1:ncol(data)){
            #    if (min(data[,i]) == max (data[,i])) keep[i] = FALSE
            # }
            # data <-  data[, keep]
#            data.ts <- ts(data, start=min(uniq.year), frequency=1)
#            data.ts <- zoo::zoo(data.ts)
#          }else {
 #           data.ts <- ts(as.vector(t(as.matrix(data))), start=min(uniq.year), frequency=12)
  #          data.ts <- zoo::zoo(data.ts, frequency=12)
   #       }
    #      data.ts <- zoo::zoo(data.ts)
          listData[[k]][[typeList.valid[[j]]]] <- data.ts
        }
      }
    # if only one grid, simplify the list
      if (length(listData) == 1){
        listData <- listData[[1]]
      }
    }
  }
  # add it to the data class
  LPJout <- new(Class="LPJData",
                runInfo=runInfo,
                dataTypes=listData)
  #LPJout@dataTypes <- listData
  return (LPJout)
}

