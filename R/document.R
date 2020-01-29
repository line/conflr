# Copyright (C) 2019 LINE Corporation
#
# conflr is free software; you can redistribute it and/or modify it under the
# terms of the GNU General Public License as published by the Free Software
# Foundation, version 3.
#
# conflr is distributed in the hope that it will be useful, but WITHOUT ANY
# WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR
# A PARTICULAR PURPOSE. See <http://www.gnu.org/licenses/> for more details.

#' Publish R Markdown Document to 'Confluence'
#'
#' Knit and post a given R Markdown file to 'Confluence'.
#'
#' @param interactive If `FALSE`, shiny interface is not launched.
#' @param title Title of the post.
#' @param type If provided, this overwrites the YAML front matter type
#' @param space_key The space key to find content under.
#' @param parent_id The page ID of the parent pages.
#' @param toc If `TRUE`, add TOC.
#' @param toc_depth The depth of the TOC. Ignored when `toc` is `FALSE`.
#' @param update If `TRUE`, overwrite the existing page (if it exists).
#' @param use_original_size
#'   If `TRUE`, use the original image sizes.
#' @param supported_syntax_highlighting
#'   A named character vector of supported syntax highlighting other than default (e.g. `c(r = "r")`).
#'
#' @details
#' `title`, `type`, `space_key`, `parent_id`, `toc`, `toc_depth`, `update`, and
#' `use_original_size` can be specified as `confluence_settings` item in the
#' front-matter of the Rmd file to knit. The arguments of
#' `confl_create_post_from_Rmd()` overwrite these settings if provided.
#'
#' @rdname confluence_document
#'
#' @export
confluence_document <- function(interactive = FALSE,
                                title = NULL,
                                # Use snake case for user-facing functions and use the actual API parameter name
                                # in camel case for simple binding functions.
                                space_key = NULL,
                                type = NULL,
                                parent_id = NULL,
                                toc = NULL,
                                toc_depth = NULL,
                                supported_syntax_highlighting = getOption("conflr_supported_syntax_highlighting"),
                                update = NULL,
                                use_original_size = NULL) {
  # This will be refered in post_processor()
  confluence_settings_from_args <- list(
    title = title,
    space_key = space_key,
    type = type,
    parent_id = parent_id,
    toc = toc,
    toc_depth = toc_depth,
    supported_syntax_highlighting = supported_syntax_highlighting,
    update = update,
    use_original_size = use_original_size
  )

  format <- rmarkdown::md_document(
    variant = "commonmark",
    pandoc_args = "--wrap=none",
    md_extensions = "-tex_math_single_backslash-tex_math_dollars-raw_tex",
    preserve_yaml = FALSE
  )

  username <- NULL

  format$pre_knit <- function(input_file) {
    # confirm the username and password are valid (and username will be useful later).
    tryCatch(
      username <<- confl_get_current_user()$username,
      error = function(e) {
        if (stringi::stri_detect_fixed(as.character(e), "Unauthorized (HTTP 401)")) {
          abort("Invalid credentials!")
        } else {
          cnd_signal(e)
        }
      }
    )
  }
  
  format$post_processor <- function(front_matter, input_file, output_file, clean, verbose) {
    # For backward-compatibility
    if (has_name(front_matter, "confluence_settings")) {
      warn(paste0("Set options via `confluence_settings` front-matter is deprecated.\n",
                  "Please use `confluence_document` instead."))

      confluence_settings <- front_matter$confluence_settings

      confluence_settings <- purrr::list_modify(
        confluence_settings,
        !!!purrr::compact(confluence_settings_from_args)
      )
    } else {
      confluence_settings <- confluence_settings_from_args
    }

    # title can be specified as a seperate item on front matter
    confluence_settings$title <- confluence_settings$title %||% front_matter$title

    md_text <- read_utf8(output_file)

    # Replace <ac:...> and <ri:...> because they are not recognized as proper tags
    # by commonmark and accordingly get escaped. We need to replace the namespace
    # to bypass the unwanted conversions. The tags will be restored later in
    # confl_upload().
    md_text <- mark_confluence_namespaces(md_text)

    html_text <- commonmark::markdown_html(md_text)

    imgs <- extract_image_paths(html_text)
    imgs_realpath <- curl::curl_unescape(imgs)

    # upload ------------------------------------------------------------------

    if (interactive) {
      # On some Confluence, the key of a personal space can be guessed from the username
      if (is.null(confluence_settings$space_key)) {
        confluence_settings$space_key <- try_get_personal_space_key(username)
      }

      exec(
        confl_upload_interactively,
        !!! confluence_settings,
        html_text = html_text,
        imgs = imgs,
        imgs_realpath = imgs_realpath
      )
    } else {
      exec(
        confl_upload,
        !!! confluence_settings,
        html_text = html_text,
        imgs = imgs,
        imgs_realpath = imgs_realpath,
        interactive = interactive
      )
    }

    output_file
  }

  format
}
