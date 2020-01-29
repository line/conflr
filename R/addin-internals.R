# Copyright (C) 2019 LINE Corporation
#
# conflr is free software; you can redistribute it and/or modify it under the
# terms of the GNU General Public License as published by the Free Software
# Foundation, version 3.
#
# conflr is distributed in the hope that it will be useful, but WITHOUT ANY
# WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR
# A PARTICULAR PURPOSE. See <http://www.gnu.org/licenses/> for more details.

confl_upload <- function(title, space_key, type, parent_id, html_text,
                         imgs, imgs_realpath,
                         toc = FALSE, toc_depth = 7,
                         supported_syntax_highlighting = getOption("conflr_supported_syntax_highlighting"),
                         update = NULL, use_original_size = FALSE,
                         interactive = NULL, session = NULL) {
  if (is.null(interactive)) {
    interactive <- interactive()
  }

  # check if there is an existing page
  existing_pages <- confl_list_pages(title = title, spaceKey = space_key)

  if (existing_pages$size == 0) {
    # if the page doesn't exist, create a blank page
    blank_page <- confl_post_page(
      type = type,
      spaceKey = space_key,
      title = title,
      body = "",
      ancestors = parent_id
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
      update <- confirm_upload(title)
    }

    if (!isTRUE(update)) {
      abort("Page already exists. Re-run with `update = TRUE` to overwrite.")
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

  html_text <- translate_to_confl_macro(
    html_text,
    image_size_default = if (!use_original_size) 600 else NULL,
    supported_syntax_highlighting = supported_syntax_highlighting
  )

  # Restore <ac:...> and <ri:...> tags before actually posting to Confluence
  html_text <- restore_confluence_namespaces(html_text)

    toc_tag <- paste(
  if (toc) {
      '<p>',
      '  <ac:structured-macro ac:name="toc">',
      glue::glue('    <ac:parameter ac:name="maxLevel">{toc_depth}</ac:parameter>'),
      '  </ac:structured-macro>',
      '</p>',
      sep = "\n"
    )
    html_text <- paste(toc_tag, html_text, sep = "\n")
  }

  result <- confl_update_page(
    id = id,
    title = title,
    body = html_text
  )
  results_url <- paste0(result$`_links`$base, result$`_links`$webui)

  progress$set(value = 2, message = "Done!")
  Sys.sleep(2)

  if (!is.null(session)) {
    shiny::stopApp()
  }

  if (interactive) {
    browseURL(paste0(result$`_links`$base, result$`_links`$webui))
  } else {
    message(paste0("Results at: ", results_url))
  }

  results_url
}

confirm_upload <- function(title) {
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
    abort("Cancel to upload.")
  }
}
