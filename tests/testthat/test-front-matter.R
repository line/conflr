should_not_be_called <- function(...) {
  stop(deparse(match.call()[[1]]), "() should not be called", call. = FALSE)
}

do_confl_create_post_from_Rmd <- function(mock, Rmd_file = "Rmd/front-matter.Rmd", ...) {
  with_mock(
    "conflr:::confl_upload" = mock,
    "conflr:::confl_get_current_user" = function(...) list(username = "user"),
    "conflr:::try_get_personal_space_key" = should_not_be_called, {
      confl_create_post_from_Rmd(Rmd_file, interactive = FALSE, ...)
    }
  )
}

test_that("confluence_settings can be set from front-matter", {
  confl_upload_mock <- mockery::mock(NULL)
  do_confl_create_post_from_Rmd(confl_upload_mock)

  cols_to_compare <- c("title", "space_key", "parent_id", "update", "use_original_size")
  expect_equal(
    mockery::mock_args(confl_upload_mock)[[1]][cols_to_compare],
    list(
      title = "title1",
      space_key = "space1",
      parent_id = 1234,
      update = TRUE,
      use_original_size = TRUE
    )
  )
  
  confl_upload_mock <- mockery::mock(NULL)
  do_confl_create_post_from_Rmd(confl_upload_mock,
    title = "title2", space_key = "space2", parent_id = 9999,
    update = FALSE, use_original_size = FALSE
  )

  cols_to_compare <- c("title", "space_key", "parent_id", "update", "use_original_size")
  expect_equal(
    mockery::mock_args(confl_upload_mock)[[1]][cols_to_compare],
    list(
      title = "title2",
      space_key = "space2",
      parent_id = 9999,
      update = FALSE,
      use_original_size = FALSE
    )
  )
})
