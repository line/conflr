# Copyright (C) 2019 LINE Corporation
#
# conflr is free software; you can redistribute it and/or modify it under the
# terms of the GNU General Public License as published by the Free Software
# Foundation, version 3.
#
# conflr is distributed in the hope that it will be useful, but WITHOUT ANY
# WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR
# A PARTICULAR PURPOSE. See <http://www.gnu.org/licenses/> for more details.

test_that("confl_contentbody_convert() works", {
  skip_on_cran()

  res <- structure(list(status_code = 200), class = "response")
  m <- mockery::mock(res)

  with_mock(
    "conflr::confl_verb" = m,
    "httr::content" = function(res) NULL,
    {
      confl_contentbody_convert("{cheese}", "wiki", "storage")
    }
  )

  args <- mockery::mock_args(m)[[1]]
  expect_equal(args[[2]], "/contentbody/convert/storage")
  expect_equal(
    args$body,
    list(
      value = "{cheese}",
      representation = "wiki"
    )
  )
})
