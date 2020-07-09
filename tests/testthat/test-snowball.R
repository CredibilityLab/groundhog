test_that("get.snowball()", {

  test_pkg <- "lightr"
  date <- "2020-01-01"

  sb <- expect_visible(get.snowball(test_pkg, date, current.deps = NULL))

  expect_is(sb, "data.frame")
  expect_named(
    sb,
    c("pkg", "vrs", "pkg_vrs", "installed", "from", "MRAN.date", "installation.time", "installation.path")
  )
  expect_is(sb$pkg, "character")
  expect_is(sb$vrs, "character")
  expect_is(sb$pkg_vrs, "character")
  expect_is(sb$installed, "logical")
  expect_true(all(sb$from %in% c("MRAN", "CRAN", "source")))
  expect_is(sb$MRAN.date, "Date")
  expect_true(min(sb$installation.time, na.rm = TRUE) > 0)
  expect_is(sb$installation.path, "character")

  sb_all <- get.snowball(test_pkg, date, include.suggests = TRUE, current.deps = NULL)

  expect_true(nrow(sb_all) >= nrow(sb))

  sb_source <- get.snowball(test_pkg, date, force.source = TRUE, current.deps = NULL)
  expect_true(all(sb_source$from == "source"))

})

test_that("check.snowball.conflict()", {

  test_pkg <- "lightr"
  date <- "2020-01-01"

  sb <- expect_visible(get.snowball(test_pkg, date, current.deps = NULL))

  expect_silent(check.snowball.conflict(sb, force.install = FALSE))

})

test_that("install.snowball()", {})
