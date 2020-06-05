# Copyright (C) 2019 LINE Corporation
#
# conflr is free software; you can redistribute it and/or modify it under the
# terms of the GNU General Public License as published by the Free Software
# Foundation, version 3.
#
# conflr is distributed in the hope that it will be useful, but WITHOUT ANY
# WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR
# A PARTICULAR PURPOSE. See <http://www.gnu.org/licenses/> for more details.

test_that("confl_post_page() works", {
  skip_on_cran()

  res <- structure(list(status_code = 200), class = "response")
  m <- mockery::mock(res)

  with_mock(
    "conflr::confl_verb" = m,
    "httr::content" = function(res) NULL,
    {
      confl_post_page("page", "space1", "title", "<p>foo</p>")
    }
  )

  args <- mockery::mock_args(m)[[1]]
  expect_equal(args$body, list(
    type = "page",
    title = "title",
    space = list(
      key = "space1"
    ),
    body = list(
      storage = list(
        value = "<p>foo</p>",
        representation = "storage"
      )
    )
  ))
})

test_that("confl_update_page() works", {
  skip_on_cran()

  res <- structure(list(status_code = 200), class = "response")
  m <- mockery::mock(res)
  info <- list(version = list(number = 11L), type = "page")
  m2 <- mockery::mock(info)

  with_mock(
    "conflr::confl_verb" = m,
    "conflr::confl_get_page" = m2,
    "httr::content" = function(res) NULL,
    {
      confl_update_page("1234", "title", "<p>foo</p>")
    }
  )
  args <- mockery::mock_args(m)[[1]]
  expect_equal(args$body, list(
    type = "page",
    title = "title",
    body = list(
      storage = list(
        value = "<p>foo</p>",
        representation = "storage"
      )
    ),
    version = list(
      number = 12L,
      minorEdit = FALSE
    )
  ))
})
