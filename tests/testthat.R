library(testthat)
library(groundhog)

if (isTRUE(R.version$status == "")) {
  test_check("groundhog")
}
