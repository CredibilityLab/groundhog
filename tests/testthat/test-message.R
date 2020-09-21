test_that("message1()", {

  expect_message(message1("test"), "test")

  expect_silent(suppressMessages(message1("test")))

})

test_that("message1()", {

  expect_message(message2("test"), "test")
  expect_message(message2(), "groundhog.library")

  expect_silent(suppressMessages(message2("test")))

})

test_that("msg.R.switch()", {

  expect_identical(
    expect_invisible(expect_message(msg.R.switch("2020-01-01"), "Instructions")),
    "3.6.3"
  )

})
