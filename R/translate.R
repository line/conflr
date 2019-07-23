# Copyright (C) 2019 LINE Corporation
#
# conflr is free software; you can redistribute it and/or modify it under the
# terms of the GNU General Public License as published by the Free Software
# Foundation, version 3.
#
# conflr is distributed in the hope that it will be useful, but WITHOUT ANY
# WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR
# A PARTICULAR PURPOSE. See <http://www.gnu.org/licenses/> for more details.


translate_to_confl_macro <- function(html_text, image_size_default = 600) {
  # remove UTF-8 characters that are invalid XML
  html_text <- gsub("[^\u0009\u000a\u000d\u0020-\uD7FF\uE000-\uFFFD]", "", html_text)

  html_text <- paste0("<body>", html_text, "</body>")
  html_doc <- xml2::read_xml(html_text, options = c("RECOVER", "NOERROR", "NOBLANKS"))

  # Mark $ and $$ in plain <p> so that we can replace them safely later.
  text_nodes <- xml2::xml_find_all(html_doc, "//p/text()")
  for (node in text_nodes) {
    text <- xml2::xml_text(node)
    text <- mark_math(text)
    text <- mark_inline_math(text)
    xml2::xml_text(node) <- text
  }

  # Conflucence doesn't accept <br />, so just remove it.
  xml2::xml_remove(xml2::xml_find_all(html_doc, "//br"))

  # convert back to character
  html_contents <- xml2::xml_find_all(html_doc, "//body/*")
  html_text <- paste(as.character(html_contents), collapse = "\n")

  # replace syntax with macros
  html_text <- replace_code_chunk(html_text)
  html_text <- replace_inline_math(html_text)
  html_text <- replace_math(html_text)
  html_text <- replace_image(html_text, image_size_default = image_size_default)
  # unescape texts inside CDATA
  html_text <- restore_cdata(html_text)

  html_text
}

unescape_html <- function(x) {
  stringi::stri_replace_all_regex(
    x,
    c("&amp;", "&lt;", "&gt;", "&#39;", "&quot;"),
    c("&", "<", ">", "'", "\""),
    vectorize_all = FALSE
  )
}

restore_cdata <- function(x) {
  locs <- stringi::stri_locate_all_regex(
    x,
    "(?<=<ac:plain-text-body><\\!\\[CDATA\\[).*?(?=\\]\\]></ac:plain-text-body>)",
    dotall = TRUE,
    omit_no_match = TRUE
  )[[1]]

  for (loc in rev(split(locs, row(locs)))) {
    stringi::stri_sub(x, loc[1], loc[2]) <- unescape_html(stringi::stri_sub(x, loc[1], loc[2]))
  }
  x
}

replace_code_chunk <- function(x) {
  stringi::stri_replace_all_regex(
    x,
    "<pre>\\s*<code[^>]*>(.*?)</code>\\s*</pre>",
    '<ac:structured-macro ac:name="code">
  <ac:plain-text-body><![CDATA[$1]]></ac:plain-text-body>
</ac:structured-macro>',
    dotall = TRUE
  )
}


mark_inline_math <- function(x) {
  # replace the left dallar (e.g. $\frac{1}{3}$)
  #                               ^
  # matches: "$foo", " $foo", "\n$foo"
  # doesn't match: "\\$foo", "foo$foo"
  x <- stringi::stri_replace_all_regex(x, "(^|(?<=\\s))\\$(?=\\S)", "%1%D%O%L%L%A%R%", multiline = TRUE)

  # replace the right dallar (e.g. $\frac{1}{3}$)
  #                                            ^
  # matches: "foo$", "foo$ ", "foo$\n"
  # doesn't match: "foo\\$", "foo$foo"
  x <- stringi::stri_replace_all_regex(x, "(?<=[^\\s\\\\])\\$($|(?=\\s))", "%1%D%O%L%L%A%R%", multiline = TRUE)
  x
}

replace_inline_math <- function(x) {
  x <- stringi::stri_replace_all_regex(
    x,
    "%1%D%O%L%L%A%R%(.*?)%1%D%O%L%L%A%R%",
    '<ac:structured-macro ac:name="mathinline">
  <ac:parameter ac:name="body">$1</ac:parameter>
</ac:structured-macro>',
    dotall = TRUE
  )
  # restore dollars that doesn't have pairs
  x <- stringi::stri_replace_all_fixed(x, "%1%D%O%L%L%A%R%", "$")
  x
}

mark_math <- function(x) stringi::stri_replace_all_regex(x, "\\$\\$", "%2%D%O%L%L%A%R%")

replace_math <- function(x) {
  # matches:
  #   1. %2%D%O%L%L%A%R%
  #      ...
  #      %2%D%O%L%L%A%R%
  #
  #   2. <p>%2%D%O%L%L%A%R%...%2%D%O%L%L%A%R%</p>
  x <- stringi::stri_replace_all_regex(
    x,
    "(^|(?<=<p>))\\s*%2%D%O%L%L%A%R%(.*?)%2%D%O%L%L%A%R%\\s*($|(?=</p>))",
    '<ac:structured-macro ac:name="mathblock">
  <ac:plain-text-body><![CDATA[$2]]></ac:plain-text-body>
</ac:structured-macro>',
    multiline = TRUE,
    dotall = TRUE
  )
  # restore dollars that doesn't have pairs
  x <- stringi::stri_replace_all_fixed(x, "%2%D%O%L%L%A%R%", "$$")
  x
}

replace_image <- function(x, image_size_default = 600) {
  locs <- stringi::stri_locate_all_regex(
    x,
    "<img[^>]*/>",
    dotall = TRUE,
    omit_no_match = TRUE
  )[[1]]

  for (loc in rev(split(locs, row(locs)))) {
    # extract attributes
    img_tag <- stringi::stri_sub(x, loc[1], loc[2])
    img_attrs <- as.list(xml2::xml_attrs(xml2::read_xml(img_tag)))

    src <- img_attrs$src
    if (is.null(src)) {
      warning(img_tag, " doesn't contain src attribute.", call. = FALSE)
      next()
    }

    # construct height and width params (e.g. ac:height="400" ac:width="300")
    hw <- list(
      width = img_attrs$width %||% image_size_default,
      height = img_attrs$height
    )
    hw <- purrr::compact(hw)

    # glue_collapse returns character(0) for character(0), so this if branch is needed.
    if (length(hw) > 0) {
      hw_params <- glue::glue('ac:{names(hw)}="{hw}"')
      hw_params <- glue::glue_collapse(hw_params, sep = " ")
    } else {
      hw_params <- ""
    }

    stringi::stri_sub(x, loc[1], loc[2]) <- glue::glue(
      '<ac:image {hw_params}><ri:attachment ri:filename="{basename(src)}" /></ac:image>'
    )
  }
  x
}
