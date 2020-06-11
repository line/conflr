# Copyright (C) 2019 LINE Corporation
#
# conflr is free software; you can redistribute it and/or modify it under the
# terms of the GNU General Public License as published by the Free Software
# Foundation, version 3.
#
# conflr is distributed in the hope that it will be useful, but WITHOUT ANY
# WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR
# A PARTICULAR PURPOSE. See <http://www.gnu.org/licenses/> for more details.

# {.tabset} notation will be brown away on conversion to commonmark as it is a Pandoc-only syntax.
wrap_tabsets <- function(x) {
  stringi::stri_replace_all_regex(x,
    "(^#+.*?)\\{[^{}]*.tabset[^{}]*\\}",
    "$1\n\n`<tabset-start/>`{=html}",
    multiline = TRUE
  )
}

mark_tabsets <- function(html_doc) {
  # If there are no tabset-start tag, do nothing
  if (length(xml2::xml_find_all(html_doc, "//body//tabset-start")) == 0) {
    return(NULL)
  }

  xpath <- c("//body//tabset-start", glue("//body//h{i}", i = 1:9))
  tags <- xml2::xml_find_all(html_doc, glue_collapse(xpath, sep = "|"))

  pos_tabset_start <- which(xml2::xml_name(tags) == "tabset-start")

  second_char <- substr(xml2::xml_name(tags), 2, 2)
  second_char[pos_tabset_start] <- NA
  h_levels <- as.integer(second_char)

  i <- 1
  while(i <= length(pos_tabset_start)) {
    start <- pos_tabset_start[i]

    h <- h_levels[start - 1]
    # The corresponding end is the nearest position among the ones that are larger than the start.
    pos_tabset_end_candidates <- which(h_levels <= h)
    end <- pos_tabset_end_candidates[pos_tabset_end_candidates > start]

    if (length(end) == 0) {
      # If there's no corresponding end, it means the end of the document is the end of the tabset.
      end <- length(tags) + 1
      xml2::xml_add_child(xml2::xml_find_first(html_doc, "//body"), xml2::as_xml_document("<tabset-end/>"), .where = "after")
    } else {
      end <- min(end)
      xml2::xml_add_sibling(tags[[end]], xml2::as_xml_document("<tabset-end/>"), .where = "before")
    }

    # Note: (start + 2) > (end - 1) when there's only one tab, so we cannot use (start + 2):(end - 1) here
    xml2::xml_set_name(tags[(start + 1):(end - 1)], "tabset-tab")
    xml2::xml_set_name(tags[(start + 1)], "tabset-tab-first")

    # If there's some <tabset-start> tag inside the tabset, remove them.
    invalid_tabsets <- pos_tabset_start[start < pos_tabset_start & pos_tabset_start < end]
    if (length(invalid_tabsets) > 0) {
      i <- i + length(invalid_tabsets)
      purrr::walk(tags[invalid_tabsets], xml2::xml_remove)
    }

    i <- i + 1
  }

  NULL
}


replace_tabsets <- function(x) {
  x <- replace_tabsets_start(x)
  x <- replace_tabsets_tabs_first(x)
  x <- replace_tabsets_tabs(x)
  x <- replace_tabsets_end(x)
  x
}


replace_tabsets_start <- function(x) {
  stringi::stri_replace_all_regex(
    x,
    # TODO: (<h\\d>)(.*?)(</h\\d>) doesn't work (c.f. https://stackoverflow.com/a/40556433)
    "(<h\\d>)([^<>]+)(</h\\d>)\\s*<tabset-start/>",
    '$1$2$3
<ac:structured-macro ac:name="deck">
<ac:parameter ac:name="id">$2</ac:parameter>
<ac:rich-text-body>',
    multiline = TRUE,
    dotall = TRUE
  )
}

replace_tabsets_tabs_first <- function(x) {
  stringi::stri_replace_all_regex(
    x,
    "<tabset-tab-first>\\s*(.*?)\\s*</tabset-tab-first>",
    '<ac:structured-macro ac:name="card">
<ac:parameter ac:name="label">$1</ac:parameter>
<ac:rich-text-body>',
    multiline = TRUE,
    dotall = TRUE
  )
}

replace_tabsets_tabs <- function(x) {
  stringi::stri_replace_all_regex(
    x,
    "<tabset-tab>\\s*(.*?)\\s*</tabset-tab>",
    '</ac:rich-text-body>
</ac:structured-macro>

<ac:structured-macro ac:name="card">
<ac:parameter ac:name="label">$1</ac:parameter>
<ac:rich-text-body>',
    multiline = TRUE,
    dotall = TRUE
  )
}

replace_tabsets_end <- function(x) {
  stringi::stri_replace_all_regex(
    x,
    "<tabset-end/>",
    '</ac:rich-text-body>
</ac:structured-macro>

</ac:rich-text-body>
</ac:structured-macro>',
    multiline = TRUE,
    dotall = TRUE
  )
}
