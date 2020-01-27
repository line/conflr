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
confluence_document <- function(...,
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
  format <- rmarkdown::md_document(
    variant = "commonmark",
    pandoc_args = "--wrap=none",
    md_extensions = "-tex_math_single_backslash-tex_math_dollars-raw_tex",
    preserve_yaml = FALSE
  )

  format$post_processor <- function() {

  }
  format
}
