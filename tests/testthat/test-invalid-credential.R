test_that("confluence_document() stops early", {
  tmp <- tempfile(fileext = ".Rmd")
  on.exit(unlink(tmp), add = TRUE)
  writeLines(c("---", "title: title1", "---\n", "test"), tmp, sep = "\n")

  knit_mock <- mockery::mock(NULL)

  withr::local_envvar(list(
    CONFLUENCE_URL = "base_url",
    CONFLUENCE_USERNAME = "username",
    CONFLUENCE_PASSWORD = "password"
  ))

  expect_error(
    with_mock(
      "httr::VERB" = function(...) abort("Unauthorized (HTTP 401)"),
      "knitr::knit" = knit_mock, {
        confl_create_post_from_Rmd(tmp, interactive = FALSE)
      }
    ),
    "Invalid credentials!", fixed = TRUE
  )

  # knit should not be called
  mockery::expect_called(knit_mock, 0)
})
