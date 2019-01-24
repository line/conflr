context("test-embed-images")

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

  md_text <- "#\u30c6\u30b9\u30c8\n![](%C3%B6/plot1.png)\n"
  html_text <- commonmark::markdown_html(md_text)
  result <- embed_images(html_text, "%C3%B6/plot1.png", file.path(tmp_dir, "plot1.png"))

  expect_equal(result, stringi::stri_replace_all_fixed(html_text, "%C3%B6/plot1.png", base64_img))
})
