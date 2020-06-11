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
  tabset_level <- determine_tabset_level(html_doc)
  if (is.null(tabset_level) || is.na(tabset_level)) {
    return(NULL)
  }

  tabset_h_tag_name <- glue("h{tabset_level}")
  # tabs are the headers of one level lower
  tab_h_tag_name <- glue("h{tabset_level + 1}")

  tags <- xml2::xml_find_all(
    html_doc,
    glue("//body//tabset-start|//body//{tabset_h_tag_name}|//body//{tab_h_tag_name}")
  )

  pos_tabset_start <- which(xml2::xml_name(tags) == "tabset-start")

  # If the header of the same level, it's right after the end of the tabset
  # But, an end must have its corresponding start, and we don't determine it here.
  pos_tabset_end_candidates <- which(xml2::xml_name(tags) == tabset_h_tag_name)

  for (start in pos_tabset_start) {
    # The corresponding end is the nearest position among the ones that are larger than the start.
    end <- pos_tabset_end_candidates[pos_tabset_end_candidates > start]

    if (length(end) == 0) {
      # If there's no corresponding end, it means the end of the document is the end of the tabset.
      end <- length(tags) + 1
      xml2::xml_add_child(xml2::xml_find_first(html_doc, "//body"), xml2::as_xml_document("<tabset-end/>"), .where = "after")
    } else {
      end <- min(end)
      xml2::xml_add_sibling(tags[[end]], xml2::as_xml_document("<tabset-end/>"), .where = "before")
    }

    xml2::xml_set_name(tags[(start + 1)], "tabset-tab-first")
    xml2::xml_set_name(tags[(start + 2):(end - 1)], "tabset-tab")
  }

  NULL
}

determine_tabset_level <- function(html_doc) {
  xpath <- glue("//body//tabset-start/preceding-sibling::h{i}[1]", i = 1:5)
  result <- xml2::xml_find_all(html_doc, glue::glue_collapse(xpath, "|"))
  if (length(result) == 0) {
    return(NULL)
  }

  h_tag_names <- sort(unique(xml2::xml_name(result)))

  if (length(h_tag_names) > 1) {
    warn("Multiple level of headers are used for tabset")
  }

  return(as.integer(substr(h_tag_names[1], 2, 2)))
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
