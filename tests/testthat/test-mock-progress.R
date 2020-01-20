test_that("ConsoleProgress mocks minimum features of shiny::Progress", {
  expect_silent(progress <- ConsoleProgress$new(min = 0, max = 2))
  expect_silent(progress$set(value = 1))
  expect_silent(progress$set(detail = 1))
  expect_message(progress$set(message = "foo"), "foo")
  expect_silent(progress$close())
})
