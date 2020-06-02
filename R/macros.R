#' Generate Confluence macro referencing a Jira ticket
#' @param id Jira ticket id, eg CONFLR-XXXX
#' @return HTML as string
#' @export
#' @importFrom glue glue
#' @examples
#' confl_macro_jira('CONFLR-42')
confl_macro_jira <- function(id) {
  glue(
    '`<ac:structured-macro ac:name="jira">',
    '<ac:parameter ac:name="key">{id}</ac:parameter>',
    '</ac:structured-macro>`{{=html}}')
}


#' Generate Confluence macro for an expand block
#' @param title defines the text that appears next to the expand/collapse icon
#' @param content this HTML content will be visible when someone clicks the macro title
#' @return HTML as string
#' @export
#' @importFrom glue glue
#' @references \url{https://confluence.atlassian.com/doc/expand-macro-223222352.html}
#' @note \code{content} needs to be HTML, so look at \code{commonmark::markdown_html}, \code{pander::pander} and eg \code{xtable} for doing the conversion before passing to \code{confluence_expand}
#' @examples \dontrun{
#' confluence_expand(
#'   'Example block',
#'   commonmark::markdown_html(pander::pander_return(list(a = list(b = 4), c = 2))))
#' }
confluence_expand <- function(title, content) {
  glue(
    '\n',
    '```{{=html}}\n',
    '<confl-ac-structured-macro confl-ac-name="expand">\n',
    '  <confl-ac-parameter confl-ac-name="title">',
    title,
    '</confl-ac-parameter>\n',
    '  <confl-ac-rich-text-body>\n',
    content,
    '\n  </confl-ac-rich-text-body>\n',
    '</confl-ac-structured-macro>\n```'
  )
}
