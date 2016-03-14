#' @title The function to close MPI connection on cluster
#' @description 
#' @seealso  \url{https://cran.r-project.org/web/packages/Rmpi/Rmpi.pdf}
#' @export
#' @keywords Rlpj
#' @author Florian Hartig, Ramiro Silveyra Gonzalez
#'
#' @examples \dontrun{
#' exit.mpi()
#'  }

exit.mpi <- function(){
  if (is.loaded("mpi_initialize")){
    if (Rmpi::mpi.comm.size(comm =1) > 1){
      cat("\nPlease use mpi.close.Rslaves() to close slaves")
      Rmpi::mpi.close.Rslaves(comm=1, dellog = FALSE)
    }
  }
  print("\nPlease use mpi.quit() to quit R")
  Rmpi::mpi.finalize() # Dont need to specify type
}
