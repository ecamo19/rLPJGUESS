# @title The function to run the LPJ in parallel
# @description This function tells each core which are its tasks and makes the
# core run the runLPJwrapper on them using sapply
# @param numcores a integer specifying number of cores of the cluster
# @param runParameters a list of lists, each list containing the following information:
# mainDir, template1, template2, gridList, runDir, outDir, mode, scale,
#  typeList, parameterList, runID and gridFilename
# @keywords Rlpj
# @author Ramiro Silveyra Gonzalez
# @note based on lapplys from M. T. Morgan (mtmorgan@fhcrc.org) (Parallel R)
# @examples \dontrun{#'
# result <- MPISapply(numcores = 6, runParameters = runParameters)
# }
MPISapply <- function(numcores, runParameters) {
  rank <- Rmpi::mpi.comm.rank()
  # master doesnt work the data
  if (rank > 0){
    mywork <- runParameters[seq(rank, length(runParameters), numcores)]
    result <- sapply(mywork, runLPJWrapper)
    return(result)
  }
}
