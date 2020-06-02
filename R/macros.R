#' General Confluence macro builder for internal use
#' @param type inline or block code style to be used for the HTML content
#' @param name Confluence macro name
#' @param parameters named list of optional macro parameters
#' @param body optional \code{confl-ac-rich-text-body} content
#' @return HTML
#' @keywords internal
conf_macro_generator <- function(type = c('inline', 'block'),
                                 name, parameters, body) {
    type <- match.arg(type)

    macro <- switch(
        type,
        inline = '`',
        block = '\n```{{=html}}\n')

    macro <- paste0(
        macro,
        glue('<ac:structured-macro ac:name="{name}">'))

    if (!missing(parameters)) {
        for (parameter in names(parameters)) {
            macro <- paste0(
                macro,
                glue('<ac:parameter ac:name="{parameter}">',
                     parameters[[parameter]],
                     '</ac:parameter>'))
        }
    }

    macro <- paste0(macro, '</ac:structured-macro>')

    macro <- paste0(macro, switch(
        type,
        inline = '`{=html}',
        block = '\n```\n'))

    macro

}

#' Generate Confluence macro for dynamic Table of Contents
#' @param levels max number of levels to show
#' @return HTML as string
#' @export
#' @importFrom glue glue
#' @examples
#' confl_macro_toc(2)
confl_macro_toc <- function(levels) {
  conf_macro_generator(type = 'inline', name = 'toc', parameters = list(levels = levels))
}


#' Generate Confluence macro referencing a Jira ticket
#' @param key Jira ticket id, eg CONFLR-XXXX
#' @return HTML as string
#' @export
#' @importFrom glue glue
#' @examples
#' confl_macro_jira('CONFLR-42')
confl_macro_jira <- function(key) {
  conf_macro_generator(type = 'inline', name = 'jira', parameters = list(key = id))
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
    conf_macro_generator(type = 'block', name = 'expand',
                         parameters = list(title = title))
}
