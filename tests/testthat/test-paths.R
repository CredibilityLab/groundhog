test_that("get.groundhogr.folder()", {

  Sys.unsetenv("GROUNDHOGR_FOLDER")
  expect_identical(get.groundhogr.folder(), path.expand(file.path("~", "groundhogR")))

  Sys.setenv("GROUNDHOGR_FOLDER" = "~/groundhogR_test")
  expect_identical(get.groundhogr.folder(), path.expand(file.path("~", "groundhogR_test")))
  Sys.unsetenv("GROUNDHOGR_FOLDER")

})

test_that("set.groundhogr.folder", {

  set.groundhogr.folder("test")

  expect_identical(get.groundhogr.folder(), "test")

  set.groundhogr.folder("test2")

  expect_identical(get.groundhogr.folder(), "test2")

  # set.groundhogr.folder() doesn't overwrite other env vars
  tmp_renviron <- tempfile("renviron")

  Sys.setenv(R_ENVIRON = tmp_renviron)
  write("TEST = test_var", tmp_renviron)
  set.groundhogr.folder("test_preserver")

  expect_length(readLines(tmp_renviron), 2)

})

test_that("get.pkg_search_paths()", {

  expect_is(get.pkg_search_paths("magrittr", "1.0.1"), "character")

})

test_that("get.installed_path()", {

  expect_is(get.installed_path("magrittr", "1.5"), "character")

})
