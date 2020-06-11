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
    "`<tabset-start>`{=html}\n\n$1\n\n`</tabset-start>`{=html}",
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

  h_tags <- xml2::xml_find_all(html_doc, glue("//body//{tabset_h_tag_name}|//body//{tab_h_tag_name}"))

  h_tags_len <- length(h_tags)
  # If the parent is <tabset-start>, it's the head of a tabset
  parent_tag_names <- purrr::map_chr(h_tags, ~ xml2::xml_name(xml2::xml_parent(.)))
  pos_tabset_start <- which(parent_tag_names == "tabset-start")

  # If the header of the same level, it's right after the end of the tabset
  # But, an end must have its corresponding start, and we don't determine it here.
  pos_tabset_end_candidates <- which(xml2::xml_name(h_tags) == tabset_h_tag_name)

  for (start in pos_tabset_start) {
    # Flatten the tabset-start tags so that we can do some regex-fu a bit easier on the next step
    #
    # Before:
    # <tabset-start>
    #   <h1>title</h1>
    # </tabset-start>
    #
    # After:
    # <h1>title</h1>
    # <tabset-start/>
    tabset_start_tag <- xml2::xml_parent(h_tags[[start]])
    # need to insert tabset-start tag before replacing the actual tabset,
    # othewise h_tags[[start]] doesn't exist in the doc since it's replaced it's copy.
    xml2::xml_add_sibling(h_tags[[start]], xml2::as_xml_document("<tabset-start/>"), .where = "after")
    # extract the tags inside <tabset-start>...</tabset-start>
    purrr::walk(xml2::xml_children(tabset_start_tag),
                ~xml2::xml_add_sibling(tabset_start_tag, .x, .where = "before"))
    xml2::xml_remove(tabset_start_tag)

    # The coresponding end is the nearest position among the ones that are larger than the start.
    end <- min(pos_tabset_end_candidates[pos_tabset_end_candidates > start])

    if (is.na(end)) {
      # If there's no corresponding end, it means the end of the document is the end of the tabset.
      end <- h_tags_len + 1
      xml2::xml_add_sibling(html_doc, xml2::as_xml_document("<tabset-end/>"), .where = "before")
    } else {
      xml2::xml_add_sibling(h_tags[[end]], xml2::as_xml_document("<tabset-end/>"), .where = "before")
    }

    xml2::xml_set_name(h_tags[(start + 1)], "tabset-tab-first")
    xml2::xml_set_name(h_tags[(start + 2):(end - 1)], "tabset-tab")
  }

  NULL
}

determine_tabset_level <- function(html_doc) {
  result <- xml2::xml_find_all(html_doc, "//body//tabset-start/*[self::h1 or self::h2 or self::h3 or self::h4 or self::h5]")
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
    "(<h\\d>)(.*?)(</h\\d>)\\s*<tabset-start/>",
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
