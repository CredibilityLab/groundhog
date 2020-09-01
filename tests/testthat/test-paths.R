test_that("get.groundhog.folder()", {

  Sys.unsetenv("GROUNDHOG_FOLDER")
  expect_identical(get.groundhog.folder(), path.expand(file.path("~", "groundhog")))

  Sys.setenv("GROUNDHOG_FOLDER" = "~/groundhogR_test")
  expect_identical(get.groundhog.folder(), path.expand(file.path("~", "groundhogR_test")))
  Sys.unsetenv("GROUNDHOG_FOLDER")

})

test_that("set.groundhog.folder", {

  set.groundhog.folder("test")

  expect_identical(get.groundhog.folder(), "test")

  set.groundhog.folder("test2")

  expect_identical(get.groundhog.folder(), "test2")

  # set.groundhogr.folder() doesn't overwrite other env vars
  tmp_renviron <- tempfile("renviron")

  Sys.setenv(R_ENVIRON = tmp_renviron)
  write("TEST = test_var", tmp_renviron)
  set.groundhog.folder("test_preserver")

  expect_length(readLines(tmp_renviron), 2)

})

test_that("get.pkg_search_paths()", {

  expect_is(get.pkg_search_paths("magrittr", "1.0.1"), "character")

})

test_that("get.installed_path()", {

  expect_is(get.installed_path("magrittr", "1.5"), "character")

})
