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

