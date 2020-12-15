test_that("load.cran.toc()", {

  load.cran.toc()

  cran.toc <- .pkgenv[["cran.toc"]]

  expect_is(cran.toc, "data.frame")
  expect_named(cran.toc, c("Package", "Version", "Imports", "Depends", "Suggests", "LinkingTo", "Published"))
  expect_is(cran.toc$Published, "Date")

  skip_if_not(getRversion() >= "3.2.0", {
    expect_false(anyNA(cran.toc$Published))
  })

  cran.times <- .pkgenv[["cran.times"]]

  expect_is(cran.times, "data.frame")
  expect_named(cran.times, c("pkg_vrs", "installation.time", "update.date"))
  expect_is(cran.times$update.date, "Date")

  skip_if_not(getRversion() >= "3.2.0", {
    expect_false(anyNA(cran.times$update.date))
  })

})

test_that("toc()", {

  
  test_pkg <- "dplyr"

  toc_pkg <- toc(test_pkg)

  expect_is(toc_pkg, "data.frame")
  expect_named(toc_pkg, c("Version", "Published"))
  expect_is(toc_pkg$Published, "Date")
  expect_identical(toc_pkg$Published, sort(toc_pkg$Published))

  toc_pkg_deps <- toc(test_pkg, dependencies = TRUE)

  expect_named(toc_pkg_deps, c("Version", "Published", "Imports", "Depends", "Suggests", "LinkingTo"))
  expect_identical(toc_pkg, toc_pkg_deps[, 1:2])

  # This error has been replaced by a exit() which doesn't play well with
  # testthat

  # expect_error(
  #   expect_message(toc(""), "no package")
  # )

})

test_that("cross.toc()", {

  test_pkgs <- c("lightr", "pavo")

  crosstoc_pkgs <- cross.toc(test_pkgs, date2 = "2020-01-01")

  expect_is(crosstoc_pkgs, "data.frame")
  expect_named(crosstoc_pkgs, c("Version", "Published", "Package"))
  expect_is(crosstoc_pkgs$Published, "Date")
  expect_identical(crosstoc_pkgs$Published, sort(crosstoc_pkgs$Published))
  expect_setequal(crosstoc_pkgs$Package, test_pkgs)

})

test_that("update_cran.toc_if.needed()", {

  # This error has been replaced by a exit() which doesn't play well with
  # testthat

  # expect_error(
  #   expect_message(update_cran.toc_if.needed("9999-12-31"),
  #                  "most recent")
  # )

  # This test is flimsy since it'll become false each time a new version of R
  # is released

  # expect_false(
  #   update_cran.toc_if.needed("1970-01-01")
  # )

})
