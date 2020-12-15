if (requireNamespace("testthat", quietly = TRUE)) {

  library(testthat)
  library(groundhog)
  "OK"

  if (isTRUE(R.version$status == "")) {
    test_check("groundhog")
  }

}
