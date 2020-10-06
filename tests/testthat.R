library(testthat)
library(groundhog)

if (R.version$status == "") {
  test_check("groundhog")
}
