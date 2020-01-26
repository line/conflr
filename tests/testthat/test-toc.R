test_that("TOC is added when set via argument", {
  tmp <- tempfile(fileext = ".Rmd")
  on.exit(unlink(tmp))

  writeLines(
'---
title: title1
confluence_settings:
  space_key: space1
---

# h1
## h2
', tmp)

  mock <- mockery::mock(NULL)
  with_mock(
    "conflr::confl_list_pages" = function(...) list(size = 1, results = list(list(id = 1))),
    "conflr::confl_list_attachments" = function(...) list(results = list()),
    "conflr::confl_update_page" = mock,
    "conflr::confl_get_current_user" = function(...) list(username = "user"),
    "conflr:::try_get_personal_space_key" = should_not_be_called, {
      confl_create_post_from_Rmd(tmp, interactive = FALSE, update = TRUE, toc = TRUE)
    }
  )

  expect_equal(
    mockery::mock_args(mock)[[1]]$body,
'<p>
  <ac:structured-macro ac:name="toc">
    <ac:parameter ac:name="maxLevel">7</ac:parameter>
  </ac:structured-macro>
</p>
<h1>h1</h1>
<h2>h2</h2>'
  )
})

test_that("TOC is added when set via front-matter", {
  tmp <- tempfile(fileext = ".Rmd")
  on.exit(unlink(tmp))

  writeLines(
'---
title: title1
confluence_settings:
  space_key: space1
  toc: true
  toc_depth: 3
---

# h1
## h2
', tmp)

  mock <- mockery::mock(NULL, cycle = TRUE)
  with_mock(
    "conflr::confl_list_pages" = function(...) list(size = 1, results = list(list(id = 1))),
    "conflr::confl_list_attachments" = function(...) list(results = list()),
    "conflr::confl_update_page" = mock,
    "conflr::confl_get_current_user" = function(...) list(username = "user"),
    "conflr:::try_get_personal_space_key" = should_not_be_called, {
      confl_create_post_from_Rmd(tmp, interactive = FALSE, update = TRUE)
      confl_create_post_from_Rmd(tmp, interactive = FALSE, update = TRUE, toc = FALSE)
    }
  )

  expect_equal(
    mockery::mock_args(mock)[[1]]$body,
'<p>
  <ac:structured-macro ac:name="toc">
    <ac:parameter ac:name="maxLevel">3</ac:parameter>
  </ac:structured-macro>
</p>
<h1>h1</h1>
<h2>h2</h2>'
  )

  expect_equal(
    mockery::mock_args(mock)[[2]]$body,
    '<h1>h1</h1>\n<h2>h2</h2>'
  )
})

