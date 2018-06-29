context("Test basic")
set.seed(1)
library(rLPJGUESS)


testthat::test_that("writeTemplate writes the desired value", {

  scale <- "global"
  parameter <- list()
  parameter$common_emax <- 1
  # Is different from default value
  testthat::expect_false(parameter$common_emax==getParameterList(scale, list = T)$common_emax)
  # put the template in the folder
  mainDir <- "../LPJ-Example"
  getTemplate(scale, mainDir)
  template <- readLines(file.path(mainDir, paste(scale, ".ins", sep="")))
  # Find the value in the template
  beforeParameter <- template[grepl("emax", template)]
  # write the template with the desired value
  writeTemplate(file.path(mainDir, paste(scale, ".ins", sep="")), parameter, mainDir, check = "serial")
  templateAfter <- readLines(file.path(mainDir, paste(scale, ".ins", sep="")))
  # Find the value in the template
  afterParameter <- templateAfter[grepl("emax", templateAfter)]
  # values should be different
  testthat::expect_false(identical(beforeParameter, afterParameter))
  # desired value should be present
  testthat::expect_true(grepl(parameter$common_emax, afterParameter))
  # delete template from the folder
  unlink(file.path(mainDir, paste(scale, ".ins", sep="")), recursive = TRUE)


})
