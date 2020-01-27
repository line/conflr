# Copyright (C) 2019 LINE Corporation
#
# conflr is free software; you can redistribute it and/or modify it under the
# terms of the GNU General Public License as published by the Free Software
# Foundation, version 3.
#
# conflr is distributed in the hope that it will be useful, but WITHOUT ANY
# WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR
# A PARTICULAR PURPOSE. See <http://www.gnu.org/licenses/> for more details.

#' Convert to a Confluence document
#' 
#' Format for converting from R Markdown to Confluence documents
#'
#' @export
confluence_document <- function(..., toc = FALSE) {
  format <- rmarkdown::md_document(
    variant = "commonmark",
    pandoc_args = "--wrap=none",
    md_extensions = "-tex_math_single_backslash-tex_math_dollars-raw_tex",
    preserve_yaml = FALSE
  )
}
