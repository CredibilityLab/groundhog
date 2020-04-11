test_that("get.R.pkg.date()", {

  test_pkg <- "lightr_1.0"
  test_r <- "3.6.0"

  pkg_date <- expect_silent(get.R.pkg.date(test_pkg, test_r))

  expect_is(pkg_date, "Date")
  expect_true(pkg_date <= Sys.Date())

})

test_that("get.date.for.install.binary()", {

  test_pkg <- "lightr_1.0"

  bin_date <- expect_silent(get.date.for.install.binary(test_pkg))

  expect_is(bin_date, "Date")
  expect_true(bin_date <= Sys.Date())

})


test_that("get.rdate()", {

  expect_is(get.rdate(), "Date")

})

test_that("r.version.check()", {

  test_date <- "2020-01-01"

  expect_silent(
    rv <- r.version.check(test_date)
  )

  expect_is(rv, "list")
  expect_length(rv, 8)
  expect_named(rv,
               c("r.using.full", "r.using.major", "r.using.minor", "r.using.patch",
                  "r.need.full", "r.need.major", "r.need.minor", "r.need.patch"))

})

test_that("get.version()", {

  test_pkg <- "lightr"
  test_date <- "2020-01-01"

  expect_silent(
    vrs <- get.version(test_pkg, test_date)
  )
  expect_is(vrs, "character")

  test_before <- "2010-01-01"

  expect_error(
    expect_message(get.version(test_pkg, test_before), "not yet available")
  )

})
