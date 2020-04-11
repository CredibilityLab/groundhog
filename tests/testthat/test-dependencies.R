test_that("get.dependencies()", {

  test_pkg <- "lightr"
  date <- "2020-01-01"

  deps <- get.dependencies(test_pkg, date)

  expect_is(deps, "character")

  fulldeps <- get.dependencies(test_pkg, date, include.suggests = TRUE)

  expect_is(fulldeps, "character")

  expect_true(all(deps %in% fulldeps))
  expect_true(length(fulldeps) >= length(deps))

})

test_that("get.all.dependencies()", {

  test_pkg <- "lightr"
  date <- "2020-01-01"

  alldeps <- get.all.dependencies(test_pkg, date)

  expect_is(alldeps, "data.frame")
  expect_named(alldeps, c("pkg", "dep2"))

  fullalldeps <- get.all.dependencies(test_pkg, date, include.suggests = TRUE)

  expect_is(fullalldeps, "data.frame")
  expect_named(fullalldeps, c("pkg", "dep2"))

  expect_true(nrow(fullalldeps) >= nrow(alldeps))

})
