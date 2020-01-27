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

  format$pre_knit <- function(input) {
    # conflr doesn't insert a title in the content automatically
    md_text <- read_utf8(input)
    
    # Replace <ac:...> and <ri:...> because they are not recognized as proper tags
    # by commonmark and accordingly get escaped. We need to replace the namespace
    # to bypass the unwanted conversions. The tags will be restored later in
    # confl_upload().
    md_text <- mark_confluence_namespaces(md_text)
    
    if (toc) {
      toc_tag <- paste(
        '<p>',
        '  <ac:structured-macro ac:name="toc">',
        glue::glue('    <ac:parameter ac:name="maxLevel">{toc_depth}</ac:parameter>'),
        '  </ac:structured-macro>',
        '</p>',
        sep = "\n"
      )
      
      # html_text is already replaced <ac:...> and <ri:...>
      toc_tag <- mark_confluence_namespaces(toc_tag)
      md_text <- paste(toc_tag, md_text, sep = "\n")
      write_utf8(input, md_text)
    }
  }
}
