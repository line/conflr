test_that("confluence_settings can be set from front-matter", {
  m <- mockery::mock(NULL)

  with_mock(
    "conflr:::confl_upload" = m,
    "conflr:::confl_get_current_user" = function(...) list(username = "user"),
    "conflr:::try_get_personal_space_key" = function(...) "user", {
      confl_create_post_from_Rmd("Rmd/front-matter.Rmd", interactive = FALSE)
    }
  )

  cols_to_compare <- c("title", "spaceKey", "parent_id", "update", "use_original_size")
  expect_equal(
    mockery::mock_args(m)[[1]][cols_to_compare],
    list(
      title = "title1",
      spaceKey = "space1",
      parent_id = 1234,
      update = TRUE,
      use_original_size = FALSE
    )
  )
})
