# Copyright (C) 2019 LINE Corporation
#
# conflr is free software; you can redistribute it and/or modify it under the
# terms of the GNU General Public License as published by the Free Software
# Foundation, version 3.
#
# conflr is distributed in the hope that it will be useful, but WITHOUT ANY
# WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR
# A PARTICULAR PURPOSE. See <http://www.gnu.org/licenses/> for more details.


#' CRUD operations for Attachments on Content
#'
#' @name confl_attachment
#' @param id
#'   The ID of a page that attachments belong to.
#' @param filename
#'   Filter parameter to return only the Attachment with the matching file name. Optional.
#' @param mediaType
#'   Filter parameter to return only Attachments with a matching Media-Type. Optional.
#' @inheritParams confl_content
#' @export
confl_list_attachments <- function(id,
                                   filename = NULL,
                                   mediaType = NULL,
                                   start = 0,
                                   limit = 50,
                                   expand = NULL) {
  id <- as.character(id)
  query <- list(limit = limit, start = start, filename = filename, mediaType = mediaType, expand = expand)
  res <- confl_verb("GET", glue::glue("/content/{id}/child/attachment"),
    query = purrr::compact(query)
  )
  httr::content(res)
}

#' @rdname confl_attachment
#' @param path Path to a file to upload.
#' @export
confl_post_attachment <- function(id, path) {
  id <- as.character(id)
  res <- confl_verb("POST", glue::glue("/content/{id}/child/attachment"),
    body = list(file = httr::upload_file(path)),
    httr::add_headers(`X-Atlassian-Token` = "nocheck")
  )
  httr::content(res)
}

#' @rdname confl_attachment
#' @param attachmentId The ID of an attachment.
#' @param ... Other arguments passed to 'query'.
#' @export
confl_update_attachment_metadata <- function(id, attachmentId, ...) {
   id <- as.character(id)
   res <- confl_verb("PUT", glue::glue("/content/{id}/child/attachment/{attachmentId}"),
                     query = list(...)
   )
   httr::content(res)
}

#' @rdname confl_attachment
#' @export
confl_update_attachment_data <- function(id, attachmentId, path, ...) {
   id <- as.character(id)
   res <- confl_verb("POST", glue::glue("/content/{id}/child/attachment/{attachmentId}/data"),
                     body = list(file = httr::upload_file(path)),
                     httr::add_headers(`X-Atlassian-Token` = "nocheck")
   )
   httr::content(res)
}
