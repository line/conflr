# Copyright (C) 2019 LINE Corporation
#
# conflr is free software; you can redistribute it and/or modify it under the
# terms of the GNU General Public License as published by the Free Software
# Foundation, version 3.
#
# conflr is distributed in the hope that it will be useful, but WITHOUT ANY
# WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR
# A PARTICULAR PURPOSE. See <http://www.gnu.org/licenses/> for more details.


#' REST Wrapper for the SpaceService
#'
#' @name confl_space
#' @param spaceKey
#'   The space key to find content under.
#' @param type
#'   Filter the list of spaces returned by type (`global`, `personal`).
#' @param status
#'   Filter the list of spaces returned by status (`current`, `archived`).
#' @param label
#'   Filter the list of spaces returned by label.
#' @param favourite
#'   Filter the list of spaces returned by favourites.
#' @inheritParams confl_content
#' @export
confl_list_spaces <- function(spaceKey = NULL,
                              type = c("global", "personal"),
                              status = c("current", "archived"),
                              label = NULL,
                              favourite = NULL,
                              expand = NULL,
                              start = NULL,
                              limit = 25) {
  type <- match.arg(type)
  status <- match.arg(status)
  query <- list(type = type, status = status, label = label, favourite = favourite,
                expand = expand, start = start, limit = limit)
  res <- confl_verb("GET", "/space", query = purrr::compact(query))
  httr::content(res)
}

#' @name confl_space
#' @export
confl_get_space <- function(spaceKey, expand = NULL) {
  query <- list(expand = expand)
  res <- confl_verb("GET", glue::glue("/space/{spaceKey}"), query = purrr::compact(query))
  httr::content(res)
}
