should_not_be_called <- function(...) {
  stop(deparse(match.call()[[1]]), "() should not be called", call. = FALSE)
}
