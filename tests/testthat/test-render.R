# Copyright (C) 2019 LINE Corporation
#
# conflr is free software; you can redistribute it and/or modify it under the
# terms of the GNU General Public License as published by the Free Software
# Foundation, version 3.
#
# conflr is distributed in the hope that it will be useful, but WITHOUT ANY
# WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR
# A PARTICULAR PURPOSE. See <http://www.gnu.org/licenses/> for more details.

test_that("rmarkdown::render() works when no space_key", {
  tmp <- tempfile(fileext = ".Rmd")
  on.exit(unlink(tmp))

  writeLines(
    "---
title: title1
output:
  conflr::confluence_document:
    toc: false
    space_key: space1
---

# h1
## h2
", tmp
  )

  with_mock(
    "conflr::confl_list_attachments" = function(...) list(results = list()),
    "conflr::confl_update_page" =  function(...) abort("", class = "success"),
    "conflr::confl_post_page" =  function(...) list(id = 1),
    "conflr::confl_get_current_user" = function(...) list(username = "user"),
    "conflr:::try_get_existing_page_id" = function(...) NULL,
    "conflr:::try_get_personal_space_key" = should_not_be_called,
    {
      expect_error(rmarkdown::render(tmp), class = "success")
    }
  )
})


test_that("rmarkdown::render() aborts when no space_key", {
  tmp <- tempfile(fileext = ".Rmd")
  on.exit(unlink(tmp))

  writeLines(
    "---
title: title1
output:
  conflr::confluence_document:
    toc: false
---

# h1
## h2
", tmp
  )

  with_mock(
    "conflr::confl_get_current_user" = function(...) list(username = "user"),
    "conflr:::try_get_existing_page_id" = function(...) NULL,
    "conflr:::try_get_personal_space_key" = should_not_be_called,
    {
      expect_error(rmarkdown::render(tmp), "Please provide `space_key`!")
    }
  )
})
