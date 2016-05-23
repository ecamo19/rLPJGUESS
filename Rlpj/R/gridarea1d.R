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
