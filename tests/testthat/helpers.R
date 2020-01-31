should_not_be_called <- function(...) {
  stop(deparse(match.call()[[1]]), "() should not be called", call. = FALSE)
}

do_confl_create_post_from_Rmd <- function(mock, front_matter = NULL, ..., body = "test\n") {
  tmp <- tempfile(fileext = ".Rmd")
  on.exit(unlink(tmp), add = TRUE)

  writeLines(c("---", front_matter, "---\n", body), tmp, sep = "\n")
  with_mock(
    "conflr:::confl_upload" = mock,
    "conflr:::confl_get_current_user" = function(...) list(username = "user"),
    "conflr:::try_get_personal_space_key" = should_not_be_called,
    {
      confl_create_post_from_Rmd(tmp, interactive = FALSE, ...)
    }
  )
}
