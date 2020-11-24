test_that("groundhog.library()", {

  # We use contactdata for tests because it has no dependencies and
  # because there is no risk of conflicts with packages used in the
  # testing workflow (as opposed to e.g. magrittr)

  skip_on_cran()

  # Given the date we use, it will generate an error for earlier R versions
  skip_if_not(getRversion() >= "4.0.0")

  tb_path <- file.path(get.groundhog.folder(), "_tarballs")

  dir.create(tb_path, recursive = TRUE)

  file.copy(list.files("../../_tarballs/", full.names = TRUE),
            tb_path,
            recursive = TRUE)

  res <- expect_message(
    groundhog.library("contactdata", "2020-10-01"),
    ">groundhog says: successfully "
  )

  expect_true("contactdata_0.1" %in% res)

  # Test non-standard evaluation
  res <- expect_message(
    groundhog.library(contactdata, "2020-10-01"),
    "groundhog says: The package you requested"
  )

})
