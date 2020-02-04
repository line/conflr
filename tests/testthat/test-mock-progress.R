# Copyright (C) 2019 LINE Corporation
#
# conflr is free software; you can redistribute it and/or modify it under the
# terms of the GNU General Public License as published by the Free Software
# Foundation, version 3.
#
# conflr is distributed in the hope that it will be useful, but WITHOUT ANY
# WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR
# A PARTICULAR PURPOSE. See <http://www.gnu.org/licenses/> for more details.

test_that("ConsoleProgress mocks minimum features of shiny::Progress", {
  expect_silent(progress <- ConsoleProgress$new(min = 0, max = 2))
  expect_silent(progress$set(value = 1))
  expect_silent(progress$set(detail = 1))
  expect_message(progress$set(message = "foo"), "foo")
  expect_silent(progress$close())
})
