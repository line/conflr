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
