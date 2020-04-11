test_that("get.installation.time()", {

  test_pkg <- "lightr"
  test_vrs <- "1.0"

  timing <- expect_visible(get.installation.time(test_pkg, test_vrs))

  expect_true(timing > 0)

})

test_that("estimate.seconds.left()", {

  test_pkg <- "lightr"
  date <- "2020-02-01"

  sb <- get.snowball(test_pkg, date)

  expect_silent(
    time_left <- estimate.seconds.left(1, Sys.time() - 180, sb)
  )

  expect_is(time_left, "numeric")

})
