testthat::context("Test basic")
set.seed(1)
library(rLPJGUESS)






checkSetupObject <- function(setupObject, numCores, mainDir, clusterType){
  # Test if setupLPJParallel returns the setup object as requested
       # Test the class
  expect_true(class(setupObject)=="LPJSetup")
      # Test the attributes
  expect_true(setupObject["clusterType"]==clusterType)
  expect_true(setupObject["numCores"]==numCores)
  expect_true(setupObject["mainDir"]==mainDir)

}



testthat::test_that("setupLPJParallel attributes", {

  numCores <- 3
  clusterType <- "SOCK"
  # mainDir <- normalizePath("./tests/LPJ-Example")
  mainDir <- mainDir <- "../LPJ-Example"
  list.files(mainDir)
  setupObject <- setupLPJParallel(numCores, clusterType, mainDir)

  # Call the function to test
  checkSetupObject(setupObject, numCores, mainDir, clusterType)

  # Delete the folders
  foldersCreated <- paste(file.path(mainDir, "runDirectory"), 1:numCores, sep = "")
  foldersAvailable <- list.dirs(mainDir)
  existingFolders <- foldersCreated[foldersCreated %in% foldersAvailable]
  unlink(existingFolders, recursive = TRUE)


})




checkFolderStructure <- function(setupObject, numCores, mainDir){
  # Test if setupLPJParallel has created as many run folders as cores requested
  foldersCreated <- paste(file.path(mainDir, "runDirectory"), 1:numCores, sep = "")
  foldersAvailable <- list.dirs(mainDir)
  existingFolders <- foldersCreated[foldersCreated %in% foldersAvailable]
  testthat::expect_true(length(existingFolders)==numCores)
  testthat::expect_true(identical(foldersCreated, setupObject["runDir"]))

  # Test if setupLPJParallel has created as many out folders as cores requested
  foldersCreated <- paste(file.path(mainDir, "runDirectory"), 1:numCores, sep = "")
  outFolders <-  paste("outDirectory", 1:numCores, sep = "")
  foldersCreated <- sapply(1:numCores, function(x){file.path(foldersCreated[x], outFolders[x])})
  foldersAvailable <- list.dirs(mainDir)
  existingFolders <- foldersCreated[foldersCreated %in% foldersAvailable]
  testthat::expect_true(length(existingFolders)==numCores)
  testthat::expect_true(identical(foldersCreated, setupObject["outDir"]))
}

testthat::test_that("setupLPJParallel creates directories and stores them", {

  numCores <- 3
  clusterType <- "SOCK"
  # mainDir <- normalizePath("./tests/LPJ-Example")
  mainDir <- mainDir <- "../LPJ-Example"
  list.files(mainDir)

  setupObject <- setupLPJParallel(numCores, clusterType, mainDir)

  # Call the function to test
  checkFolderStructure(setupObject, numCores, mainDir)

  # Delete the folders
  runFoldersCreated <- paste(file.path(mainDir, "runDirectory"), 1:numCores, sep = "")
  runFoldersAvailable <- list.dirs(mainDir)
  existingRunFolders <- runFoldersCreated[runFoldersCreated %in% runFoldersAvailable]
  unlink(existingRunFolders, recursive = TRUE)


})

