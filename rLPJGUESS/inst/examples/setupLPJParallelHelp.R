\dontrun{
mainDir <- "/some/absolute/path/mainDir"
 list.files(mainDir)
 [1] "guess" or "guesscmd.exe"  # link to the model executable
 [2] "gridlist.txt"      # list of gridcells
 [3] "global.ins"        # template1 (optional)
 [4] "global_cru.ins"    # template2 (optional)

mySetup <- setupLPJParallel(numCores= 3, clusterType = "SOCK", mainDir=mainDir)
mySetup
      class              : LPJSetup
      cluster type       : SOCK
      number of cores    : 3
      output directories :
      /some/absolute/path/mainDir/runDirectory1
      /some/absolute/path/mainDir/runDirectory2
      /some/absolute/path/mainDir/runDirectory3

}
