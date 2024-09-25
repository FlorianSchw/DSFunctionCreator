# This file is part of the standard setup for testthat.
# It is recommended that you do not modify it.
#
# Where should you do additional test configuration?
# Learn more about the roles of various files in:
# * https://r-pkgs.org/testing-design.html#sec-tests-files-overview
# * https://testthat.r-lib.org/articles/special-files.html

#### Modified template builder based on the usethis::use_test functionality

library(testthat)
library(DSLite)
library(DSOpal)
library(DSI)
library(dsBase)
library(dsBaseClient)
library({{{ dsPackage }}})
library({{{ dsPackage }}}Client)

test_check("{{{ dsPackage }}}Client")
