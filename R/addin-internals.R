# Copyright (C) 2019 LINE Corporation
#
# conflr is free software; you can redistribute it and/or modify it under the
# terms of the GNU General Public License as published by the Free Software
# Foundation, version 3.
#
# conflr is distributed in the hope that it will be useful, but WITHOUT ANY
# WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR
# A PARTICULAR PURPOSE. See <http://www.gnu.org/licenses/> for more details.

confl_upload <- function(title, spaceKey, type, ancestors, html_text,
                         imgs, imgs_realpath,
                         update = NULL, use_original_size = FALSE,
                         interactive = NULL, session = NULL) {
  if (is.null(interactive)) {
    interactive <- interactive()
  }

  # check if there is an existing page
  existing_pages <- confl_list_pages(title = title, spaceKey = spaceKey)

  if (existing_pages$size == 0) {
    # if the page doesn't exist, create a blank page
    blank_page <- confl_post_page(
      type = type,
      spaceKey = spaceKey,
      title = title,
      body = "",
      ancestors = ancestors
    )
    id <- blank_page$id
  } else {
    # Confirm if it's OK to update the existing page
    #
    # 1) interactive,     update is NULL : ask (default)
    # 2) interactive,     update is TRUE : proceed
    # 3) interactive,     update is FALSE: abort
    # 4) non-interactive, update is NULL : abort (default)
    # 5) non-interacitve, update is TRUE : proceed
    # 6) non-interactive, update is FALSE: abort
    if (interactive && is.null(update)) {
      update <- confirm_upload()
    }

    if (!isTRUE(update)) {
      stop("Page already exists. Re-run with `update = TRUE` to overwrite.",
           call. = FALSE)
    }

    id <- existing_pages$results[[1]]$id
  }

  progress <- new_progress(session, min = 0, max = 2)
  on.exit(progress$close())

  # Step 1) Upload Images
  progress$set(message = "Checking the existing images...")

  # Check if the images already exist
  imgs_exist <- confl_list_attachments(id)
  imgs_exist_ids <- purrr::map_chr(imgs_exist$results, "id")
  names(imgs_exist_ids) <- purrr::map_chr(imgs_exist$results, "title")

  progress$set(message = "Uploading the images...")
  num_imgs <- length(imgs)
  for (i in seq_along(imgs)) {
    progress$set(detail = imgs[i])

    # attempt to avoid rate limits
    Sys.sleep(0.2)

    img_id <- imgs_exist_ids[basename(imgs[i])]
    if (is.na(img_id)) {
      confl_post_attachment(id, imgs_realpath[i])
    } else {
      confl_update_attachment_data(id, img_id, imgs_realpath[i])
    }

    progress$set(value = i / num_imgs)
  }

  # Step 2) Upload the document
  progress$set(message = "Uploading the document...")

  image_size_default <- if (!use_original_size) 600 else NULL
  result <- confl_update_page(
    id = id,
    title = title,
    body = html_text,
    image_size_default = image_size_default
  )

  progress$set(value = 2, message = "Done!")
  Sys.sleep(2)

  results_url <- paste0(result$`_links`$base, result$`_links`$webui)

  # TOOD: use interactive here?
  if (!is.null(session)) {
    shiny::stopApp()
    browseURL(paste0(result$`_links`$base, result$`_links`$webui))
  } else {
    message(paste0("Results at: ", results_url))
  }

  results_url
}

confirm_upload <- function() {
  ans <- rstudioapi::showQuestion(
    "Update?",
    glue::glue(
      "There is already an existing page named '{title}'.\n",
      "Are you sure to overwrite it?"
    ),
    ok = "OK", cancel = "cancel"
  )
  if (ans) {
    TRUE
  } else {
    stop("Cancel to upload.", call. = FALSE)
  }
}
