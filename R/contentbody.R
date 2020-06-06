# Copyright (C) 2019 LINE Corporation
#
# conflr is free software; you can redistribute it and/or modify it under the
# terms of the GNU General Public License as published by the Free Software
# Foundation, version 3.
#
# conflr is distributed in the hope that it will be useful, but WITHOUT ANY
# WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR
# A PARTICULAR PURPOSE. See <http://www.gnu.org/licenses/> for more details.


#' Converts between content body representations
#'
#' @name confl_contentbody
#' @param x
#'   The content body to convert.
#' @param from
#'   The format to convert from.
#' @param to
#'   The format to convert to.
#'
#' @return
#'   The API response as a list.
#'
#' @examples
#' \dontrun{
#' # Convert to a Math macro
#' confl_contentbody_convert("\\[1+1=2\\]")
#'
#' # Convert to an Expand macro
#' confl_contentbody_convert("\{expand\}detail is here \{expand\}")
#' }
#'
#' @seealso <https://docs.atlassian.com/ConfluenceServer/rest/latest/>
#'
#' @export
confl_contentbody_convert <- function(x,
                                      from = c("wiki", "storage", "editor", "view", "export_view", "styled_view"),
                                      to = c("storage", "editor", "view", "export_view", "styled_view")) {
  if (length(x) != 1) {
    abort("`x` must be length 1")
  }

  from <- arg_match(from)
  to <- arg_match(to)

  res <- confl_verb("POST", glue("/contentbody/convert/{to}"),
    body = list(value = x, representation = from), encode = "json"
  )
  httr::content(res)$value
}
