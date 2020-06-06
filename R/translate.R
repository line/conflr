# Copyright (C) 2019 LINE Corporation
#
# conflr is free software; you can redistribute it and/or modify it under the
# terms of the GNU General Public License as published by the Free Software
# Foundation, version 3.
#
# conflr is distributed in the hope that it will be useful, but WITHOUT ANY
# WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR
# A PARTICULAR PURPOSE. See <http://www.gnu.org/licenses/> for more details.

# picking some popular languages from https://confluence.atlassian.com/doc/code-block-macro-139390.html
supported_syntax_highlighting_default <- c(
  sql = "sql",
  cpp = "cpp",
  python = "py",
  html = "xml",
  css = "css",
  bash = "bash",
  yaml = "yaml"
)

normalise_supported_syntax_highlighting <- function(x) {
  # supported_syntax_highlighting can be
  # 1) a named character vector
  # 2) an unnamed character vector
  # 3) a named list of characters
  # 4) an unnamed list of characters
  # 5) NULL

  if (is.null(x)) {
    return(NULL)
  }

  # if supported_syntax_highlighting is a list, flatten it (TODO: use vctrs?)
  if (is.list(x)) {
    x <- unlist(x, recursive = FALSE)
  }

  # if supported_syntax_highlighting is provided as unnamed form, name it
  if (is.character(x) && is.null(names(x))) {
    names(x) <- x
  }

  x
}

translate_to_confl_macro <- function(html_text,
                                     image_size_default = 600,
                                     supported_syntax_highlighting = NULL,
                                     code_folding = "none") {
  supported_syntax_highlighting <- normalise_supported_syntax_highlighting(supported_syntax_highlighting)
  supported_syntax_highlighting <- c(supported_syntax_highlighting, supported_syntax_highlighting_default)

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
  html_text <- replace_code_chunk(html_text,
    supported_syntax_highlighting = supported_syntax_highlighting,
    code_folding = code_folding
  )
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

get_corresponding_lang <- function(x, supported_syntax_highlighting = character(0)) {
  if (isTRUE(is.na(x)) || identical(x, "")) {
    return("none")
  }

  x <- supported_syntax_highlighting[x]

  if (is.na(x)) {
    "none"
  } else {
    unname(x)
  }
}

replace_code_chunk <- function(x,
                               supported_syntax_highlighting = character(0),
                               code_folding = "none") {
  locs <- stringi::stri_locate_all_regex(
    x,
    "<pre>\\s*<code[^>]*>(.*?)</code>\\s*</pre>",
    dotall = TRUE,
    omit_no_match = TRUE
  )[[1]]

  for (loc in rev(split(locs, row(locs)))) {
    pre_tag <- stringi::stri_sub(x, loc[1], loc[2])
    code_tag <- xml2::xml_find_first(xml2::read_xml(pre_tag), "/pre/code")

    # code
    code_text <- xml2::xml_text(code_tag)

    # language attribute
    class <- xml2::xml_attr(code_tag, "class")
    if (!is.na(class)) {
      lang <- stringi::stri_extract_first_regex(class, "(?<=language-)(.*)")
    } else {
      # TODO: where did I face this attribute...?
      lang <- xml2::xml_attr(code_tag, "language")
    }

    lang <- get_corresponding_lang(lang, supported_syntax_highlighting)
    lang_param <- glue('  <ac:parameter ac:name="language">{lang}</ac:parameter>')

    # collapse
    if (identical(code_folding, "hide") &&
      # do not collapse when the code block is of the result, which probably
      # doesn't have language-* class.
      isTRUE(startsWith(class, "language-"))) {
      collapse_param <- '\n  <ac:parameter ac:name="collapse">true</ac:parameter>'
    } else {
      collapse_param <- ""
    }

    stringi::stri_sub(x, loc[1], loc[2]) <- glue(
      '<ac:structured-macro ac:name="code">
{lang_param}{collapse_param}
  <ac:plain-text-body><![CDATA[{code_text}]]></ac:plain-text-body>
</ac:structured-macro>'
    )
  }
  x
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
      warn(glue("{img_tag} doesn't contain src attribute."))
      next()
    }

    # skip external files
    if (grepl("^https?://", src)) {
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
      hw_params <- glue('ac:{names(hw)}="{hw}"')
      hw_params <- glue_collapse(hw_params, sep = " ")
    } else {
      hw_params <- ""
    }

    stringi::stri_sub(x, loc[1], loc[2]) <- glue(
      '<ac:image {hw_params}><ri:attachment ri:filename="{basename(src)}" /></ac:image>'
    )
  }
  x
}

mark_confluence_namespaces <- function(x) {
  locs <- stringi::stri_locate_all_regex(
    x,
    "</?(ac|ri):[^<>]*?>",
    omit_no_match = TRUE
  )[[1]]

  for (loc in rev(split(locs, row(locs)))) {
    orig <- stringi::stri_sub(x, loc[1], loc[2])
    marked <- stringi::stri_replace_all_regex(orig,
      pattern = "(?<=<|\\s|^)(/?)(ac|ri):",
      replacement = "$1confl-$2-"
    )
    stringi::stri_sub(x, loc[1], loc[2]) <- marked
  }

  x
}

restore_confluence_namespaces <- function(x) {
  locs <- stringi::stri_locate_all_regex(
    x,
    "</?confl-(ac|ri)-[^<>]*?>",
    omit_no_match = TRUE
  )[[1]]

  for (loc in rev(split(locs, row(locs)))) {
    orig <- stringi::stri_sub(x, loc[1], loc[2])
    restored <- stringi::stri_replace_all_regex(orig,
      pattern = "(?<=<|\\s|^)(/?)confl-(ac|ri)-",
      replacement = "$1$2:"
    )
    stringi::stri_sub(x, loc[1], loc[2]) <- restored
  }

  x
}
