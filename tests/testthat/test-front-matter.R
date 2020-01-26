expect_confluence_settings <- function(mock, ...) {
  expected <- list(...)
  cols_to_compare <- names(expected)
  expect_equal(
    mockery::mock_args(mock)[[1]][cols_to_compare],
    expected
  )
}

Rmd_with_all_defaults <-
'title: "title1"
confluence_settings:
  space_key: "space1"
  parent_id: 1234
  toc: TRUE
  toc_depth: 4
  update: TRUE
  use_original_size: TRUE'

test_that("confluence_settings can be set from front-matter", {

  # case: all settings are specified in the Rmd
  confl_upload_mock <- mockery::mock(NULL)
  do_confl_create_post_from_Rmd(confl_upload_mock, Rmd_with_all_defaults)

  expect_confluence_settings(
    confl_upload_mock,
    title = "title1",
    space_key = "space1",
    parent_id = 1234,
    toc = TRUE,
    toc_depth = 4,
    update = TRUE,
    use_original_size = TRUE
  )

  # case: args overwrite settings in the Rmd
  confl_upload_mock <- mockery::mock(NULL)
  do_confl_create_post_from_Rmd(confl_upload_mock, Rmd_with_all_defaults,
    title = "title2", space_key = "space2", parent_id = 9999,
    toc = FALSE, toc_depth = 2, update = FALSE, use_original_size = FALSE
  )

  expect_confluence_settings(
    confl_upload_mock,
    title = "title2",
    space_key = "space2",
    parent_id = 9999,
    toc = FALSE,
    toc_depth = 2,
    update = FALSE,
    use_original_size = FALSE
  )
})

Rmd_with_two_titles <-
'title: "title1"
confluence_settings:
  title: "title2"
  space_key: "space1"
  parent_id: 1234
  toc: TRUE
  toc_depth: 4
  update: TRUE
  use_original_size: TRUE'

test_that("confluence_settings$title is prior to title", {

  # case: confluence_settings$title is prior to title
  confl_upload_mock <- mockery::mock(NULL)
  do_confl_create_post_from_Rmd(confl_upload_mock, Rmd_with_two_titles)

  expect_confluence_settings(
    confl_upload_mock,
    title = "title2",
    space_key = "space1",
    parent_id = 1234,
    toc = TRUE,
    toc_depth = 4,
    update = TRUE,
    use_original_size = TRUE
  )

  # case: args overwrite settings in the Rmd
  confl_upload_mock <- mockery::mock(NULL)
  do_confl_create_post_from_Rmd(confl_upload_mock, Rmd_with_two_titles,
    title = "title3"
  )

  expect_confluence_settings(
    confl_upload_mock,
    title = "title3",
    space_key = "space1",
    parent_id = 1234,
    toc = TRUE,
    toc_depth = 4,
    update = TRUE,
    use_original_size = TRUE
  )
})

Rmd_with_some_settings <-
'title: "title1"
confluence_settings:
  space_key: "space1"'

test_that("confluence_settings can be specified partially", {

  # case: confluence_settings$title is prior to title
  confl_upload_mock <- mockery::mock(NULL)
  do_confl_create_post_from_Rmd(confl_upload_mock, Rmd_with_some_settings)

  expect_confluence_settings(
    confl_upload_mock,
    title = "title1",
    space_key = "space1"
  )
})

Rmd_without_space_key <- 'title: "title1"'

test_that("confluence_settings raise an error when any of mandatory parameters are missing", {

  # case: when space_key is neither in the front-matter or in the arguments, it fails
  confl_upload_mock <- mockery::mock(NULL)
  expect_error(
    do_confl_create_post_from_Rmd(confl_upload_mock, Rmd_without_space_key)
  )

  # case: when space_key is not in the front-matter but in the arguments, it works
  confl_upload_mock <- mockery::mock(NULL)
  do_confl_create_post_from_Rmd(confl_upload_mock, Rmd_without_space_key, space_key = "space2")
  expect_confluence_settings(
    confl_upload_mock,
    title = "title1",
    space_key = "space2"
  )
})
