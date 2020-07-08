library(testthat)
library(groundhogR)

if (R.version$status == "") {
  test_check("groundhogR")
}

