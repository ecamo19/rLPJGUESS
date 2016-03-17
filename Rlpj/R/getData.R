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
#' @param lon.extent a numeric vector containing the min and max values used
#'  for west-east extent(default -180 to 180)
#' @param lat.extent a numeric vector containing the max and min values used
#'  for north-south extent (default 90 to -90).
#' @param area.weighted a boolean indicating whether the gridcells should be
#' weighted by their size (regular grids only, default FALSE).
#' @param year.offset a integer indicating the value to be added to the 'Year'
#'  column in the LPJ-GUESS output.
#' @return the processed data returned in a S4 Class: LPJData Class
#' @seealso \url{https://cran.r-project.org/web/packages/zoo/zoo.pdf}
#' @export
#' @author Florian Hartig, Ramiro Silveyra Gonzalez, Maurizio Bagnara
#' @note Based on an older code of Joerg Steinkamp
#' @examples \dontrun{
#' LPJout <- getData( typeList = c("aaet",  "cflux","lai", "nflux"),
#'           "~/path/to/output/files", runInfo = list(parameter1 = 0.5, grid = 1))
#' }

getData <- function(typeList = NULL, outDir=NULL, runInfo=NULL, lon.extent=c(-180, 180),
                    lat.extent=c(-90, 90), area.weighted=FALSE, year.offset=0 ) {
  # checking provided parameters
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
  # Finding the files in path and checking if they exist
  typeList.valid <- NULL
  for (i in 1:length(typeList)){
    if (file.exists(file.path(outDir, paste(typeList[[i]], ".out", sep="")))){
        # check if they are empty
      if ( file.info( file.path(outDir, paste(typeList[[i]], ".out", sep="")) )[['size']] == 0){
        message( paste("The",  typeList[[i]], ".out is empty!\n", sep = " ") )
      }else{
        typeList.valid <- c(typeList.valid, typeList[[i]])
      }
    }else{
      message( paste("There is no ",  typeList[[i]], ".out\n", sep = "") )
      }
    }
  if (is.null(typeList.valid)){
    stop("There are not model outputs. Please check the guess.log files.")
  }
    # starting tclass!
  LPJout <- LPJData()
    # storing run info
  LPJout@runInfo <- runInfo
  # Creating a list to hold data
  listData <- vector(mode="list", length=length( typeList.valid))
  names(listData) <- typeList.valid
  # Adding Data to listData
  # looping over data types, reading files, processing data and adding it to the Data Class
  # list append data, probably will have to use the name() function to give it the right name
  # in the end, make the list of class LPJData
  # Add data to LPJout Data class
  for (j in 1:length(typeList.valid)) {
    # reading output
    data <- read.table(file.path(outDir, paste( typeList.valid[[j]], ".out", sep="")),header=T)
    # setting annual true or false
    annual <- TRUE
    if (colnames(data)[4] == "Jan"){
      annual <- FALSE
    }
    # choosing the spatial subset
    data <- subset(data, Lon>=min(lon.extent) & Lon<=max(lon.extent) & Lat <=max(lat.extent) & Lat>=min(lat.extent))
    data$Year <- data$Year + year.offset
    # create the area weight if desired
    data$area <- 1.
    if (area.weighted) {
      data$area <- NA
      uniq.lon <- sort(unique(data$Lon))
      uniq.lat <- sort(unique(data$Lat), decreasing = TRUE)
      uniq.lon <- seq(min(uniq.lon), max(uniq.lon),
                      min(uniq.lon[2:length(uniq.lon)] - uniq.lon[1:(length(uniq.lon)-1)]))
      uniq.lat <- seq(max(uniq.lat), min(uniq.lat),
                      max(uniq.lat[2:length(uniq.lat)] - uniq.lat[1:(length(uniq.lat)-1)]))
      area1d <- gridarea1d(uniq.lat, abs(uniq.lon[2]-uniq.lon[1]))*1.e-6
      for (i in 1:length(uniq.lat))
        data$area[data$Lat == uniq.lat[i]] = area1d[i]
    }
    uniq.year <- sort(unique(data$Year))
    cnames <- colnames(data)
    data.tmp <- NULL
    for (i in uniq.year){
      data.tmp <- rbind(data.tmp, data.frame(t(colMeans(data[data$Year == i, ]))))
      }
    # remove the unused columns
    data <- data.tmp[, !(cnames=="Lon" | cnames=="Lat" | cnames=="Year" | cnames=="area")]
    rm(data.tmp)
    #if annual remove columns with unique values
    if (annual) {
     # keep <- rep(TRUE, ncol(data))
      # remove columns with unique values
     # for (i in 1:ncol(data)){
      #    if (min(data[,i]) == max (data[,i])) keep[i] = FALSE
     # }
     # data <-  data[, keep]
      data.ts <- ts(data, start=min(uniq.year), frequency=1)
    }else {
      data.ts <- ts(as.vector(t(as.matrix(data))), start=min(uniq.year), frequency=12)
    }
    data.ts <- zoo::zoo(data.ts)
    listData[[typeList.valid[[j]]]] <- as.matrix(data.ts)
  }
  # add it to the data class
  LPJout@dataTypes <- listData
  return (LPJout)
}

#' @title grid cell area along a vector of latitudes
#' @description The function returns a vector of area in square meters of along
#' a vector of latitudes. These must not be of equal distance. However, for the
#' longitude will be equal along the given latitude vector. The latitude is assumed
#' to be the gridcell midpoint and the northern and southern edges are
#' calculated by as half of the distance to the next element in the latitude vector.#'
#' @param lat vetor of latitudes
#' @param dlon longitudinal extent
#' @param ellipse TRUE (polar and equatorial radius differ) or
#' FALSE (default, polar and equatorial radius are the same)
#' @keywords RLpj
#' @export
#' @return Returns the area in square meters along a vetor of latitudes by equal
#'  longitude distance. Vector of gridcell area is m^2
#' @note Based on an older code of Joerg Steinkamp
#' @examples \dontrun{
#' area1d <- gridarea1d(uniq.lat, abs(uniq.lon[2]-uniq.lon[1]))*1.e-6
#' }

.EarthRadius         <- 6371220.0
.EarthRadius.polar   <- 6356752.3142
.EarthRadius.equator <- 6378137.0

gridarea1d <- function (lat, dlon, scale=1.0, ellipse=FALSE) {
  nlat <- length(lat)
  area <- array(0.0, nlat)

  lat.border <- array(0.0, nlat+1)
  lat.border[1] = lat[1] - (lat[2] -lat[1])/2.
  for (i in 2:nlat) {
    lat.border[i] = lat[i] - (lat[i] - lat[i-1])/2.
  }
  lat.border[nlat+1] = lat[nlat] + (lat[nlat] - lat[nlat-1])/2.

  for (i in 1:nlat) {
    # this causes a negligible difference (510.068 compared to 510.1013 10^6 km^2
    # @ 0.5° resolution globally).
    if (ellipse){
      .EarthRadius <- .EarthRadius.equator * cos(lat[i]/180.0*pi)^2 + .EarthRadius.polar * sin(lat[i]/180*pi)^2
    }
    x <- cos(lat[i]/180.0*pi) * 2. * pi * .EarthRadius / (360.0/dlon)
    y <- 2 * pi * .EarthRadius * (abs(lat.border[i+1] - lat.border[i]) / 360.)
    area[i] <- x*y
  }
  return(area*scale)
}
