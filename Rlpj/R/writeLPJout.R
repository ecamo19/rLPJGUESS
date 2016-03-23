#' @title A write LPj outputs function
#' @description  This function will allow you write out the files contained in
#' runInfo.Rdata.
#' @param runInfo a list of matrices contained in the LPJ data object.
#' @param outDir a character string indicating the folder where the plots will be
#' saved.
#' @param typeList a character vector with the outputs to be written out.
#' @export
#' @keywords Rlpj
#' @author Florian Hartig, Ramiro Silveyra Gonzalez
#' @examples \dontrun{
#' plotData(dataList = list( aaet = aaet), typeList = c("aaet"),
#'  outDir = "/runDir/outDir", save.plots = FALSE)
#' }

writeLPJout <- function(runInfo = NULL, typeList = NULL, outDir= NULL){

  # checking input parameters
  if (is.null(runInfo) || !file.exists(runInfo)){
    stop("No valid runInfo has been provided")
  }
  if (is.null(outDir) || !file.exists(outDir)){
    stop("No validoutDir has been provided")
  }

  # Check what is available
  typeList.available <- names(runObject$output)
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

  # Write out the output data
  for (i in 1:length(typeList.valid)){
    df <- as.data.frame(runObject$output[[typeList.valid[i]]])
    cols <- colnames(df)
    df$Year <- as.numeric(rownames(df))
    df <-  df[,c("Year", cols)]
    write.table(df, file.path(outDir, paste(typeList.valid[i], ".out", sep="")),
                col.names = TRUE, row.names = FALSE, sep = " ")
  }
}
