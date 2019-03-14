# Copyright (C) 2019 LINE Corporation
#
# conflr is free software; you can redistribute it and/or modify it under the
# terms of the GNU General Public License as published by the Free Software
# Foundation, version 3.
#
# conflr is distributed in the hope that it will be useful, but WITHOUT ANY
# WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR
# A PARTICULAR PURPOSE. See <http://www.gnu.org/licenses/> for more details.


#' REST Wrapper for the ContentService
#'
#' @name confl_content
#' @param type
#'   The content type to return. Default value: `page`. Valid values: `page`, `blogpost`.
#' @param limit
#'   The limit of the number of items to return, this may be restricted by fixed system limits.
#' @param start
#'   The start point of the collection to return.
#' @param spaceKey
#'   The space key to find content under.
#' @param title
#'   The title of the page to find. Required for `page` type.
#' @param expand
#'   A comma separated list of properties to expand. To refer the nested
#'   contents, use periods. (e.g. `body.storage,history`).
#'
#' @seealso <https://docs.atlassian.com/ConfluenceServer/rest/6.10.1/>
#'
#' @export
confl_list_pages <- function(type = c("page", "blogpost", "comment", "attachment"),
                             limit = 10,
                             start = 0,
                             spaceKey = NULL,
                             title = NULL,
                             expand = NULL) {
  type <- match.arg(type)
  query <- list(type = type, limit = limit, start = start, spaceKey = spaceKey, title = title, expand = expand)
  res <- confl_verb("GET", "/content/", query = purrr::compact(query))
  httr::content(res)
}


#' @rdname confl_content
#'
#' @param id
#'   ID of the content.
#' @export
confl_get_page <- function(id, expand = "body.storage") {
  id <- as.character(id)
  res <- confl_verb("GET", glue::glue("/content/{id}"), query = list(expand = expand))
  httr::content(res)
}

#' @rdname confl_content
#' @param body
#'   The HTML source of the page.
#' @param ancestors
#'   The page ID of the parent pages.
#' @param image_size_default
#'   The default width of images in pixel. If `NULL`, images are displayed in their original sizes.
#' @export
confl_post_page <- function(type = c("page", "blogpost"),
                            spaceKey,
                            title,
                            body,
                            ancestors = NULL,
                            image_size_default = 600) {
  type <- match.arg(type)

  body <- translate_to_confl_macro(body, image_size_default = image_size_default)

  req_body = list(
    type = type,
    title = title,
    space = list(key = spaceKey),
    body = list(storage = list(value = body, representation = "storage"))
  )

  if (!is.null(ancestors) && !identical(ancestors, "")) {
    ancestors <- stringi::stri_trim_both(ancestors)
    req_body$ancestors <- purrr::map(ancestors, ~list(id = .))
  }
  res <- confl_verb("POST", "/content/",
                    body = req_body, encode = "json")
  httr::content(res)
}

#' @rdname confl_content
#' @export
confl_update_page <- function(id,
                              title,
                              body,
                              image_size_default = 600) {
  id <- as.character(id)
  page_info <- confl_get_page(id, expand = "version")

  body <- translate_to_confl_macro(body, image_size_default = image_size_default)

  res <- confl_verb("PUT", glue::glue("/content/{id}"),
                    body = list(
                      type = page_info$type,
                      title = title,
                      body = list(storage = list(value = body, representation = "storage")),
                      version = list(number = page_info$version$number + 1L)
                    ),
                    encode = "json")
  httr::content(res)
}


#' @rdname confl_content
#' @export
confl_delete_page <- function(id) {
  id <- as.character(id)
  res <- confl_verb("DELETE", glue::glue("/content/{id}"))
  httr::content(res)
}
