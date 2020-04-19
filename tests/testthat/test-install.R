test_that("groundhog.library()", {

  tmp_gdir <- file.path(tempdir(), "groundhogR")
  dir.create(tmp_gdir)

  file.copy("../../_tarballs/",
            file.path(tmp_gdir),
            recursive = TRUE)

  Sys.setenv("GROUNDHOGR_FOLDER" = tmp_gdir)

  res <- groundhog.library("stringr", "2020-01-01")

  expect_true("stringr_1.4.0" %in% res)

  Sys.unsetenv("GROUNDHOGR_FOLDER")

})
