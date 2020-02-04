# Copyright (C) 2019 LINE Corporation
#
# conflr is free software; you can redistribute it and/or modify it under the
# terms of the GNU General Public License as published by the Free Software
# Foundation, version 3.
#
# conflr is distributed in the hope that it will be useful, but WITHOUT ANY
# WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR
# A PARTICULAR PURPOSE. See <http://www.gnu.org/licenses/> for more details.

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

  md_text <- "# test\n![](%C3%B6/plot1.png)\n"
  html_text <- commonmark::markdown_html(md_text)
  expected <- stringi::stri_replace_all_fixed(html_text, "%C3%B6/plot1.png", base64_img)

  # unit test
  result1 <- embed_images(html_text, "%C3%B6/plot1.png", file.path(tmp_dir, "plot1.png"))
  expect_equal(result1, expected)

  # integrated test
  Rmd_with_some_settings <-
    'title: "title1"
output:
  conflr::confluence_document:
    space_key: "space1"'
  confl_upload_mock <- mockery::mock(NULL)
  do_confl_create_post_from_Rmd(confl_upload_mock, Rmd_with_some_settings, body = md_text)
  result2 <- mockery::mock_args(confl_upload_mock)[[1]]

  expect_equal(result2$html_text, html_text)
  expect_equal(result2$imgs, "%C3%B6/plot1.png")
  expect_equal(result2$imgs_realpath, "\u00f6/plot1.png")
})
