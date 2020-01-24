base64_img <- knitr::image_uri("plot1.png")

test_that("embed_images() works for current dir", {
  md_text <- "#\u30c6\u30b9\u30c8\n![](./plot1.png)\n"
  html_text <- commonmark::markdown_html(md_text)
  result <- embed_images(html_text, "./plot1.png", "./plot1.png")

  expect_equal(result, stringi::stri_replace_all_fixed(html_text, "./plot1.png", base64_img))
})

test_that("embed_images() works for multiple images", {
  md_text <- "#\u30c6\u30b9\u30c8\n![](./plot1.png)\n![](https://example.com/test.png)\n![](./plot1.png)\n"
  html_text <- commonmark::markdown_html(md_text)
  result <- embed_images(html_text, "./plot1.png", "./plot1.png")

  expect_equal(result, stringi::stri_replace_all_fixed(html_text, "./plot1.png", base64_img))
})

test_that("embed_images() works for non-ASCII dir", {
  # LATIN SMALL LETTER O WITH DIAERESIS
  tmp_dir <- file.path(tempdir(), "\u00f6")
  on.exit(unlink(tmp_dir, recursive = TRUE), add = TRUE)
  dir.create(tmp_dir)
  file.copy("plot1.png", file.path(tmp_dir, "plot1.png"))

  # check preview html
  md_text <- "# \u30c6\u30b9\u30c8\n![](%C3%B6/plot1.png)\n"
  html_text <- commonmark::markdown_html(md_text)
  result1 <- embed_images(html_text, "%C3%B6/plot1.png", file.path(tmp_dir, "plot1.png"))
  expect_equal(result1, stringi::stri_replace_all_fixed(html_text, "%C3%B6/plot1.png", base64_img))

  # integrated test
  Rmd_with_some_settings <-
'title: "title1"
confluence_settings:
  space_key: "space1"'
  confl_upload_mock <- mockery::mock(NULL)
  do_confl_create_post_from_Rmd(confl_upload_mock, Rmd_with_some_settings, body = md_text)
  result2 <- mockery::mock_args(confl_upload_mock)[[1]]$html_text
  expect_equal(result2, html_text)
  expect_match(
    translate_to_confl_macro(result2),
    '<ac:image ac:width="600"><ri:attachment ri:filename="plot1.png" /></ac:image>',
    fixed = TRUE
  )
})

