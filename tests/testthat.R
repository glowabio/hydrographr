# This file is part of the standard setup for testthat.
# It is recommended that you do not modify it.
#
# Where should you do additional test configuration?
# Learn more about the roles of various files in:
# * https://r-pkgs.org/testing-design.html#sec-tests-files-overview
# * https://testthat.r-lib.org/articles/special-files.html

# To run the tests:
#library(devtools)
#setwd("/.../.../.../hydrographr/")
#devtools::test()

# Or to run single test files:
# test_file("/home/.../hydrographr/tests/testthat/test-download_tiles_base.R")



library(testthat)
library(hydrographr)

test_check("hydrographr")
