# Copyright (C) 2019 LINE Corporation
#
# conflr is free software; you can redistribute it and/or modify it under the
# terms of the GNU General Public License as published by the Free Software
# Foundation, version 3.
#
# conflr is distributed in the hope that it will be useful, but WITHOUT ANY
# WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR
# A PARTICULAR PURPOSE. See <http://www.gnu.org/licenses/> for more details.

confl_console_upload <- function(md_file, title, tags, space_key, type,
                                 parent_id, update = FALSE, use_original_size) {

  # conflr doesn't insert a title in the content automatically
  md_text <- read_utf8(md_file)
  html_text <- commonmark::markdown_html(md_text)

  md_dir <- dirname(md_file)
  imgs <- extract_image_paths(html_text)
  imgs <- curl::curl_unescape(imgs)
  # imgs might be absolute, relative to md_dir, or relative to the current dir.
  imgs_realpath <- ifelse(file.exists(imgs), imgs, file.path(md_dir, imgs))

  html_text_for_preview <- embed_images(html_text, imgs, imgs_realpath)

  # Extracted Shiny Code
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
    if (!update) {
      stop("Page already exists. Re-run with `update = TRUE` to overwrite.",
           call. = FALSE)
    }

    id <- existing_pages$results[[1]]$id
  }

  # Step 1) Upload Images
  message("Checking the existing images...")

  # Check if the images already exist
  imgs_exist <- confl_list_attachments(id)
  imgs_exist_ids <- purrr::map_chr(imgs_exist$results, "id")
  names(imgs_exist_ids) <- purrr::map_chr(imgs_exist$results, "title")

  message("Uploading the images...")
  num_imgs <- length(imgs)
  for (i in seq_along(imgs)) {

    # attempt to avoid rate limits
    Sys.sleep(0.2)

    img_id <- imgs_exist_ids[basename(imgs[i])]
    if (is.na(img_id)) {
      confl_post_attachment(id, imgs_realpath[i])
    } else {
      confl_update_attachment_data(id, img_id, imgs_realpath[i])
    }
  }

  # Step 2) Upload the document
  message("Uploading the document...")

  image_size_default <- if (!use_original_size) 600 else NULL
  result <- confl_update_page(
    id = id,
    title = title,
    body = html_text,
    image_size_default = image_size_default
  )

  message("Done!")
  Sys.sleep(2)

  results_url <- paste0(result$`_links`$base, result$`_links`$webui)
  message(paste0("Results at: ", results_url))
  results_url
}
