# Copyright (C) 2019-2020 LINE Corporation
#
# conflr is free software; you can redistribute it and/or modify it under the
# terms of the GNU General Public License as published by the Free Software
# Foundation, version 3.
#
# conflr is distributed in the hope that it will be useful, but WITHOUT ANY
# WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR
# A PARTICULAR PURPOSE. See <http://www.gnu.org/licenses/> for more details.

#' General Confluence macro builder for internal use
#' @param type inline or block code style to be used for the HTML content
#' @param name Confluence macro name
#' @param parameters named list of optional macro parameters
#' @param body optional \code{confl-ac-rich-text-body} content
#' @return HTML
#' @keywords internal
conf_macro_generator <- function(type = c('inline', 'block'),
                                 name, parameters = NULL, body = NULL) {

  type <- match.arg(type)

  open <- switch(type, inline = '`', block = '\n```{=html}\n')
  close <- switch(type, inline = '`{=html}', block = '\n```\n')
  spacer <- switch(type, inline = '', block = '\n')

  macro <- glue('{open}<ac:structured-macro ac:name="{name}">{spacer}')

  if (!is.null(parameters)) {
    for (parameter in names(parameters)) {
      macro <- paste0(
        macro,
        glue('<ac:parameter ac:name="{parameter}">'),
        parameters[[parameter]],
        '</ac:parameter>{spacer}')
    }
  }

  if (!is.null(body)) {
    macro <- paste0(
      macro,
      '<ac:rich-text-body>',
      spacer, body, spacer,
      '</ac:rich-text-body>',
      spacer)
  }

  macro <- paste0(macro, '</ac:structured-macro>')
  macro <- paste0(macro, close)

  macro

}

#' Generate Confluence macro for dynamic Table of Contents
#' @param levels max number of levels to show
#' @return HTML as string
#' @export
#' @references \url{https://confluence.atlassian.com/doc/table-of-contents-macro-182682099.html}
#' @examples
#' confl_macro_toc(2)
confl_macro_toc <- function(levels) {
  conf_macro_generator(type = 'block', name = 'toc', parameters = list(levels = levels))
}


#' Generate Confluence macro referencing a Jira ticket
#' @param key Jira ticket id, eg CONFLR-XXXX
#' @return HTML as string
#' @export
#' @examples
#' confl_macro_jira('CONFLR-42')
confl_macro_jira <- function(key) {
  conf_macro_generator(type = 'inline', name = 'jira', parameters = list(key = key))
}


#' Generate Confluence macro for an expand block
#' @param title defines the text that appears next to the expand/collapse icon
#' @param body this HTML content will be visible when someone clicks the macro title
#' @return HTML as string
#' @export
#' @references \url{https://confluence.atlassian.com/doc/expand-macro-223222352.html}
#' @note \code{content} needs to be HTML, so look at \code{commonmark::markdown_html}, \code{pander::pander} and eg \code{xtable} for doing the conversion before passing to \code{confluence_expand}
#' @examples \dontrun{
#' confl_macro_expand(
#'   'Example block',
#'   commonmark::markdown_html(pander::pander_return(list(a = list(b = 4), c = 2))))
#' }
confl_macro_expand <- function(title, body) {
  conf_macro_generator(type = 'block', name = 'expand',
                       parameters = list(title = title),
                       body = body)
}


#' Generate Confluence macro for an excerpt block
#' @param body HTML content of the excerpt
#' @param hidden if the \code{body} should be shown on the actual page
#' @return HTML as string
#' @export
#' @references \url{https://confluence.atlassian.com/doc/excerpt-macro-148062.html}
confl_macro_excerpt <- function(body, hidden = TRUE) {
  conf_macro_generator(type = 'block', name = 'excerpt',
                       parameters = list(hidden = tolower(hidden)),
                       body = body)
}


#' Generate Confluence macro for a Info, Tip, Note, or Warning block
#' @param body HTML content of the block
#' @return HTML as string
#' @usage
#' confl_macro_info(body)
#'
#' confl_macro_tip(body)
#'
#' confl_macro_note(body)
#'
#' confl_macro_warning(body)
#' @export
#' @references \url{https://confluence.atlassian.com/doc/info-tip-note-and-warning-macros-51872369.html}
#' @aliases confl_macro_info confl_macro_tip confl_macro_note confl_macro_warning
confl_macro_info <- function(body) {
  conf_macro_generator(type = 'block', name = 'info', body = body)
}
#' @export
confl_macro_tip <- function(body) {
  conf_macro_generator(type = 'block', name = 'tip', body = body)
}
#' @export
confl_macro_note <- function(body) {
  conf_macro_generator(type = 'block', name = 'note', body = body)
}
#' @export
confl_macro_warning <- function(body) {
  conf_macro_generator(type = 'block', name = 'warning', body = body)
}
