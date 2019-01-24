skip_on_ci_or_cran <- function() {
  testthat::skip_on_cran()

  if (!identical(Sys.getenv("CI"), "true")) {
    return(invisible(TRUE))
  }
  testthat::skip("On CI")
}
