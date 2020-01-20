context("utils")

test_that('%|""|% works', {
  expect_equal("a" %|""|% "b", "a")
  expect_equal("" %|""|% "b", "b")
  expect_equal(NULL %|""|% "b", "b")
  expect_equal("" %|""|% "", "")
})

envvars <- list(
  CONFLUENCE_URL = "",
  CONFLUENCE_USERNAME = "user",
  CONFLUENCE_PASSWORD = "pass"
)

test_that("confl_verb() asks for credentials if it is not set", {
  skip_on_cran()

  # If the request succeeds, the provided credential is stored as an envvar
  res_success <- structure(list(status_code = 200), class = "response")
  mock_success <- mockery::mock(res_success, cycle = TRUE)
  mock_ask <- mockery::mock("foo")

  with_mock(
    "httr::VERB" = mock_success,
    "conflr::ask_confluence_url" = mock_ask,
    "conflr::ask_confluence_username" = mock_ask,
    "conflr::ask_confluence_password" = mock_ask,
    withr::with_envvar(
      envvars, {
        confl_verb("GET", "/")
        expect_equal(Sys.getenv("CONFLUENCE_URL"), "foo")
      }
    )
  )

  mockery::expect_called(mock_ask, 1)
  mockery::expect_call(mock_ask, n = 1, ask_confluence_url())


  # If the request fails, the provided credential is discarded

  res_failure <- structure(list(status_code = 500), class = "response")
  mock_failure <- mockery::mock(res_failure, cycle = TRUE)

  mock_ask2 <- mockery::mock("foo")

  with_mock(
    "httr::VERB" = mock_failure,
    "conflr::ask_confluence_url" = mock_ask2,
    "conflr::ask_confluence_username" = mock_ask2,
    "conflr::ask_confluence_password" = mock_ask2,
    withr::with_envvar(
      envvars, {
        expect_error(confl_verb("GET", "/"))
        # the url provided should not be stored
        expect_equal(Sys.getenv("CONFLUENCE_URL"), "")
      }
    )
  )

  mockery::expect_called(mock_ask2, 1)
  mockery::expect_call(mock_ask2, n = 1, ask_confluence_url())
})
