test_that("get.groundhogr.folder()", {

  expect_identical(get.groundhogr.folder(), path.expand(file.path("~", "groundhogR")))

  Sys.setenv("GROUNDHOGR_FOLDER" = "~/groundhogR_test")
  expect_identical(get.groundhogr.folder(), path.expand(file.path("~", "groundhogR_test")))
  Sys.unsetenv("GROUNDHOGR_FOLDER")

})

test_that("get.pkg_search_paths()", {

  expect_is(get.pkg_search_paths("magrittr", "1.0.1"), "character")

})

test_that("get.installed_path()", {

  expect_is(get.installed_path("magrittr", "1.5"), "character")

})
