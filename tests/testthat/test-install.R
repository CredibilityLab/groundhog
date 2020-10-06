test_that("groundhog.library()", {

  skip_on_cran()

  tb_path <- file.path(get.groundhog.folder(), "_tarballs")

  dir.create(tb_path, recursive = TRUE)

  file.copy(list.files("../../_tarballs/", full.names = TRUE),
            tb_path,
            recursive = TRUE)

  res <- expect_message(
    groundhog.library("magrittr", "2015-01-01"),
    "Successfully"
  )

  expect_true("magrittr_1.5" %in% res)

  # Test non-standard evaluation
  res <- expect_message(
    groundhog.library(magrittr, "2015-01-01"),
    "already loaded"
  )

})
