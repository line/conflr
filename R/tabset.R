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
    "`<tabset>`{=html}\n\n$1\n\n`</tabset>`{=html}",
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
  # If the parent is <tabset>, it's the head of a tabset
  parent_tag_names <- purrr::map_chr(h_tags, ~ xml2::xml_name(xml2::xml_parent(.)))
  idx_tabset <- parent_tag_names == "tabset"

  # If the parent is not <tabset> but is the header of the same level, it's not tabset
  idx_no_tabset <- !idx_tabset & (xml2::xml_name(h_tags) == tabset_h_tag_name)

  idx_tab <- xml2::xml_name(h_tags) == tab_h_tag_name

  tabset_ids <- cumsum(idx_tabset)
  tabset_pos <- which(idx_tabset)
  no_tabset_pos <- which(idx_no_tabset)

  # headers before the first tabset headers are not tabs
  tabset_ids[tabset_ids == 0] <- NA

  # headers after non-tabset header and before the next tabset headers are not tabs
  for (pos in no_tabset_pos) {
    end_pos <- tabset_pos[tabset_pos > pos]
    if (length(end_pos) > 0) {
      end_pos <- end_pos - 1
    } else {
      end_pos <- h_tags_len
    }
    tabset_ids[pos:end_pos] <- NA
  }

  tabset_ids
}

determine_tabset_level <- function(html_doc) {
  result <- xml2::xml_find_all(html_doc, "//body//tabset/*[self::h1 or self::h2 or self::h3 or self::h4 or self::h5]")
  if (length(result) == 0) {
    return(NULL)
  }

  h_tag_names <- sort(unique(xml2::xml_name(result)))

  if (length(h_tag_names) > 1) {
    warn("Multiple level of headers are used for tabset")
  }

  return(as.integer(substr(h_tag_names[1], 2, 2)))
}
