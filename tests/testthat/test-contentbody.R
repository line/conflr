test_that("confl_contentbody_convert() works", {
  skip_on_cran()

  res <- structure(list(status_code = 200), class = "response")
  m <- mockery::mock(res)

  with_mock(
    "conflr::confl_verb" = m,
    "httr::content" = function(res) NULL, {
      confl_contentbody_convert("{cheese}", "wiki", "storage")
    }
  )

  args <- mockery::mock_args(m)[[1]]
  expect_equal(args[[2]], "/contentbody/convert/storage")
  expect_equal(args$body,
     list(
       value = "{cheese}",
       representation = "wiki"
     )
  )
})
