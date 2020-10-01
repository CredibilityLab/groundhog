library(testthat)
library(groundhog)

if (R.version$status == "" && identical(Sys.getenv("NOT_CRAN"), "true")) {
  test_check("groundhog")
}

