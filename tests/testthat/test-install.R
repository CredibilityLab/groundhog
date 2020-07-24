test_that("groundhog.library()", {

  tmp_gdir <- file.path(tempdir(), "groundhogR")
  dir.create(file.path(tmp_gdir, "_tarballs"), recursive = TRUE)

  file.copy(list.files("../../_tarballs/", full.names = TRUE),
            file.path(tmp_gdir, "_tarballs"),
            recursive = TRUE)

  Sys.setenv("GROUNDHOGR_FOLDER" = tmp_gdir)

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

  Sys.unsetenv("GROUNDHOGR_FOLDER")

})
