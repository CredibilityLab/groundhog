test_that("get.groundhog.folder()", {

  Sys.unsetenv("GROUNDHOG_FOLDER")
  expect_identical(get.groundhog.folder(), path.expand(file.path("~", "groundhog")))

  test <- tempfile("groundhogR_test")
  Sys.setenv("GROUNDHOG_FOLDER" = test)
  expect_identical(get.groundhog.folder(), test)
  Sys.unsetenv("GROUNDHOG_FOLDER")

})

test_that("set.groundhog.folder", {

  test1 <- tempfile("test")
  set.groundhog.folder(test1)

  expect_identical(get.groundhog.folder(), test1)

  test2 <- tempfile("test2")
  set.groundhog.folder(test2)

  expect_identical(get.groundhog.folder(), test2)

  # set.groundhog.folder() doesn't overwrite other env vars
  write("TEST = test_var", Sys.getenv("R_ENVIRON"))
  set.groundhog.folder(tempfile("test_preserver"))

  expect_identical(Sys.getenv("TEST"), "test_var")

})

test_that("get.pkg_search_paths()", {

  p <- get.pkg_search_paths("magrittr", "1.0.1")

  expect_type(p, "character")
  expect_length(p, 1L)

})

test_that("get.installed_path()", {

  expect_is(get.installed_path("magrittr", "1.5"), "character")

})
