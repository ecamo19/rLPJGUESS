
Sys.setenv("R_TESTS" = "")

library(rLPJGUESS)
library(testthat)

test_check("rLPJGUESS")
