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
                         id = NULL,
                         toc = FALSE, toc_depth = 7,
                         supported_syntax_highlighting = getOption("conflr_supported_syntax_highlighting"),
                         update = FALSE,
                         use_original_size = FALSE,
                         session = NULL) {
  # 1) id is NULL,                     update is TRUE : proceed
  # 2) id is NULL,                     update is FALSE: proceed
  # 3) id is NULL but the page exists, update is TRUE : proceed
  # 4) id is NULL but the page exists, update is FALSE: abort
  # 5) id is not NULL,                 update is TRUE : proceed
  # 6) id is not NULL,                 update is FALSE: abort

  # If id is NULL, check if there's the same title of the page
  if (is.null(id)) {
    # TODO: we check if there's an existing page twice; here and
    # confl_update_interactively(). Can we remove the duplication?
    existing_pages <- confl_list_pages(title = title, spaceKey = space_key)
    id <- existing_pages$results[[1]]$id
  }

  # If there's an existing page and the user don't want to update, abort
  if (!is.null(id)) {
    if (!isTRUE(update)) {
      abort("Page already exists. Re-run with `update = TRUE` to overwrite.")
    }
  } else {
    # if the page doesn't exist yet, create a blank page
    blank_page <- confl_post_page(
      type = type,
      spaceKey = space_key,
      title = title,
      body = "",
      ancestors = parent_id
    )
    id <- blank_page$id
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

  if (toc) {
    toc_tag <- paste(
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

  # Even on non-interactive sessions, jump to the URL if knitting is done on RStudio
  if (interactive() || (identical(Sys.getenv("RSTUDIO"), "1") && is.null(Sys.getenv("TESTTHAT")))) {
    browseURL(paste0(result$`_links`$base, result$`_links`$webui))
  } else {
    message(paste0("Results at: ", results_url))
  }

  results_url
}
