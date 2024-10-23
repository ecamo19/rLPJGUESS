testthat::context("Test basic")
set.seed(1)
library(rLPJGUESS)


testthat::test_that("test LPJdata", {

  data <- "../LPJ-Example/runDirectory/outDirectory"
  selectedTypes <-  c("cmass",  "lai")
  # dummy runInfo
  runInfo <- list(parameter1 = 0.5, grid = 1)

  LPJout <- getLPJData(data, typeList = selectedTypes,
                       runInfo = runInfo)
  # it retursn LPJData object
  testthat::expect_true(class(LPJout) == "LPJData")
  # it retuns only the selected outputs
  outputTypes <- names(LPJout["dataTypes"])
  testthat::expect_true(identical(selectedTypes, outputTypes))
  # runInfo is what we asked for
  desiredInfo <- names(runInfo)
  availableInfo <- names(LPJout["runInfo"])
  testthat::expect_true(identical(desiredInfo, availableInfo))

  # Data can be plotted
  plotLPJData(LPJout, typeList = c("cmass", "lai"))
})







