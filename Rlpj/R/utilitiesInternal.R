#' @title The function to fill the single run object
#'
#' @description This function TODO
#' @param singleObject TODO
#' @param runDir TODO
#' @param outDir TODO
#' @param parameterList TODO
#' @param ID TODO
#' @return TODO
fillSingleObject <- function(singleObject, runDir, outDir, parameterList, ID ){
  singleObject$runID <- ID
  singleObject$runDir <- runDir
  singleObject$outDir <- outDir
  singleObject$parameterList <- parameterList

  singleObject$template1 <- readLines(file.path(singleObject$mainDir, singleObject$template1Name))
  singleObject$template1 <- sub("path_to_output/", paste(singleObject$outDir, "/", sep =""), singleObject$template1)
  for ( j in 1:length(singleObject$typeList)) {
    singleObject$template1 <- sub(paste("! file", singleObject$typeList[j], sep="_"),
                                  paste("file",  singleObject$typeList[j], sep="_") , singleObject$template1)
  }
  # template 2: the cru or cf template
  singleObject$template2 <- readLines(file.path(singleObject$mainDir,singleObject$template2Name))
  singleObject$template2  <- sub("path_to_globalTemplate", paste(singleObject$runDir, "/", singleObject$template1Name, sep=""), singleObject$template2 )
  singleObject$template2  <- sub("path_to_gridlist",paste(singleObject$runDir,"/", singleObject$gridListName, sep=""),singleObject$template2 )
  for ( j in 1:length(singleObject$files.names)){
    singleObject$template2  <- gsub(singleObject$files.names[[j]][1],  singleObject$files.names[[j]][2], singleObject$template2 )
  }
  #
  return(singleObject)
}



#' @title The function to fill the single run object
#'
#' @description This function TODO
#' @param data TODO
#' @return TODO
convertTS <- function(data = NULL){
  # setting annual true or false
  annual <- TRUE
  if (colnames(data)[4] == "Jan"){
    annual <- FALSE
  }
  uniq.year <- sort(unique(data$Year))
  cnames <- colnames(data)
  data.tmp <- NULL
  for (i in 1:length(uniq.year)){
    data.tmp <- rbind(data.tmp, data.frame(t(colMeans(data[data$Year == uniq.year[i], ]))))
  }
  # remove the unused columns
  data <- data.tmp[, !(cnames=="Lon" | cnames=="Lat" | cnames=="Year")]
  rm(data.tmp)
  if (annual) {
    data.ts <- ts(data, start=min(uniq.year), frequency=1)
    data.ts <- zoo::zoo(data.ts)
  }else {
    data.ts <- ts(as.vector(t(as.matrix(data))), start=min(uniq.year), frequency=12)
    data.ts <- zoo::zoo(data.ts, frequency=12)
  }
  data.ts <- zoo::zoo(data.ts)
  return(data.ts)
}



#' @title A parameter list function
#' @description  This function checks the provided parameter list against the
#' complete parameters list. If any parameter is not provided, the function will
#' add it to the parameter list and return a complete parameter list with the values
#' provided as input and the default values for those parameters that were not provided.
#' @param scale a character string indicating whether the model runs global or for europe.
#' @param parameterList a named list holding the parameter or combination or parameters
#'  to be tested.
#' @return a named list holding the values of the template parameter.
#' @keywords Rlpj
#' @author Florian Hartig, Ramiro Silveyra Gonzalez, Maurizio Bagnara
#' @examples \dontrun{
#' parameterList <- list(run_emax = 4, run_lambda_max = 1)
#' parameterList.checked <- checkParameters(scale= "global", parameterList)
#' }
checkParameters <- function(scale = NULL, parameterList = NULL){
  # include check
  if ( scale != "global" & scale != "europe"){
    stop("checkParameters: Cannot recognize the template: neither global nor europe.")
  }
  # call the default template
  default <- parameterList.default[[scale]]
  # get names for doing the posterior chekc
  parameterNames <- names(default)
  # check now
  if( is.null(parameterList)){
    # get the defalut template
    parameterList <- default
  }else{
    # check the provided parameter list
    # if any paramater is not provided, then added it.
    for (i in 1:length(parameterNames)){
      if (is.null(parameterList[[parameterNames[i]]])){
        parameterList[[parameterNames[i]]] <- default[[parameterNames[i]]]
      }
    }
  }
  return(parameterList)
}




# @title grid cell area along a vector of latitudes
# @description The function returns a vector of area in square meters of along
# a vector of latitudes. These must not be of equal distance. However, for the
# longitude will be equal along the given latitude vector. The latitude is assumed
# to be the gridcell midpoint and the northern and southern edges are
# calculated by as half of the distance to the next element in the latitude vector.#'
# @param lat vetor of latitudes
# @param dlon longitudinal extent
# @param ellipse TRUE (polar and equatorial radius differ) or
# FALSE (default, polar and equatorial radius are the same)
# @keywords RLpj
# @export
# @return Returns the area in square meters along a vetor of latitudes by equal
#  longitude distance. Vector of gridcell area is m^2
# @note Based on an older code of Joerg Steinkamp
# @examples \dontrun{
# area1d <- gridarea1d(uniq.lat, abs(uniq.lon[2]-uniq.lon[1]))*1.e-6
# }

#.EarthRadius         <- 6371220.0
#.EarthRadius.polar   <- 6356752.3142
#.EarthRadius.equator <- 6378137.0

#gridarea1d <- function (lat, dlon, scale=1.0, ellipse=FALSE) {
#  nlat <- length(lat)
#  area <- array(0.0, nlat)#

#  lat.border <- array(0.0, nlat+1)
#  lat.border[1] = lat[1] - (lat[2] -lat[1])/2.
#  for (i in 2:nlat) {
#    lat.border[i] = lat[i] - (lat[i] - lat[i-1])/2.
#  }
#  lat.border[nlat+1] = lat[nlat] + (lat[nlat] - lat[nlat-1])/2.

#  for (i in 1:nlat) {
# this causes a negligible difference (510.068 compared to 510.1013 10^6 km^2
# @ 0.5° resolution globally).
#    if (ellipse){
#      .EarthRadius <- .EarthRadius.equator * cos(lat[i]/180.0*pi)^2 + .EarthRadius.polar * sin(lat[i]/180*pi)^2
#    }
#    x <- cos(lat[i]/180.0*pi) * 2. * pi * .EarthRadius / (360.0/dlon)
#    y <- 2 * pi * .EarthRadius * (abs(lat.border[i+1] - lat.border[i]) / 360.)
#    area[i] <- x*y
#  }
#  return(area*scale)
#}
