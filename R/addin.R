# Copyright (C) 2019 LINE Corporation
#
# conflr is free software; you can redistribute it and/or modify it under the
# terms of the GNU General Public License as published by the Free Software
# Foundation, version 3.
#
# conflr is distributed in the hope that it will be useful, but WITHOUT ANY
# WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR
# A PARTICULAR PURPOSE. See <http://www.gnu.org/licenses/> for more details.

#' @param Rmd_file Path to an .Rmd file.
#' @param params If provided, a list of named parameters that override custom
#'   params in the YAML front-matter.
#' @param ... Arguments passed to `confluence_documents()`.
#'
#' @rdname confluence_document
#'
#' @examples
#' \dontrun{
#' # Convert an R Markdown document into a 'Confluence' page interactively
#' confl_create_post_from_Rmd("path/to/file.Rmd")
#'
#' # You can override most of the parameters of confluence_document()
#' confl_create_post_from_Rmd("path/to/file.Rmd", space = "space1", toc = TRUE)
#' }
#'
#' @export
confl_create_post_from_Rmd <- function(Rmd_file, interactive = NULL, params = NULL, ...) {

  # FIXME: uncomment this when r-lib/ellipsis#32 is fixed
  # ellipsis::check_dots_used()

  if (is.null(interactive)) {
    interactive <- interactive()
  }

  # Sanity checks -----------------------------------------------------------

  if (!tolower(tools::file_ext(Rmd_file)) %in% c("rmd", "rmarkdown")) {
    abort(glue::glue("{basename(Rmd_file)} is not .Rmd file!"))
  }

  # Combine options from arguments and front matter -------------------------

  # Get options on the front matter
  front_matter <- rmarkdown::yaml_front_matter(Rmd_file)
  if (is.list(front_matter$output) &&
    has_name(front_matter$output, "conflr::confluence_document")) {
    output_options <- front_matter$output$`conflr::confluence_document`
  } else {
    output_options <- list()
  }

  # Override the options by those via arguments (title will be handled in post_processor())
  output_options <- purrr::list_modify(output_options, ..., interactive = interactive)

  output_format <- exec(confluence_document, !!!output_options)

  # Knit --------------------------------------------------------------------

  rmarkdown::render(
    input = Rmd_file,
    output_format = output_format,
    encoding = "UTF-8",
    params = params,
    # TODO: I'm not fully sure the global env is always the right place to knit, but this is needed to avoid
    #       an error related to data.table (#29). If this doesn't work, I need to add this code (c.f. https://github.com/Rdatatable/data.table/blob/5ceda0f383f91b7503d4a236ee4e7438724340be/R/cedta.R#L13):
    #   assignInNamespace("cedta.pkgEvalsUserCode", c(data.table:::cedta.pkgEvalsUserCode, "conflr"), "data.table")
    env = globalenv()
  )
}

confl_create_post_from_Rmd_addin <- function() {
  if (!rstudioapi::isAvailable()) {
    abort("This function must be called on RStudio!")
  }

  Rmd_file <- rstudioapi::getSourceEditorContext()$path
  if (identical(Rmd_file, "")) {
    # Probably "UntitledX"
    abort("Please save the .Rmd file first!")
  }

  confl_create_post_from_Rmd(Rmd_file, interactive = TRUE)
}

confl_upload_interactively <- function(title, html_text, imgs, imgs_realpath,
                                       space_key = NULL,
                                       parent_id = NULL,
                                       type = "page",
                                       toc = FALSE, toc_depth = 7,
                                       code_folding = "none",
                                       supported_syntax_highlighting = getOption("conflr_supported_syntax_highlighting"),
                                       use_original_size = FALSE) {

  # Shiny UI -----------------------------------------------------------
  ui <- confl_addin_ui(
    title = title,
    html_text = html_text,
    imgs = imgs,
    imgs_realpath = imgs_realpath,
    space_key = space_key,
    parent_id = parent_id,
    type = type,
    toc = toc,
    toc_depth = toc_depth,
    code_folding = code_folding,
    use_original_size = use_original_size
  )


  # Shiny Server -------------------------------------------------------
  server <- function(input, output, session) {
    id <- NULL
    done <- shiny::reactiveVal(FALSE)

    shiny::observeEvent(input$confirm, {
      # if the space key is empty, tell the user to provide it and exit early
      if (identical(input$space_key, "")) {
        shiny::showModal(
          shiny::modalDialog("Please provide a space key!",
            footer = shiny::modalButton("OK")
          )
        )
        return()
      }

      id <<- try_get_existing_page_id(title = title, space_key = input$space_key)

      # If there is already an existing page, confirm user
      if (!is.null(id)) {
        shiny::showModal(shiny::modalDialog(
          glue::glue(
            "There is already an existing page named '{title}'.\n",
            "Are you sure to overwrite it?"
          ),
          footer = shiny::tagList(
            shiny::modalButton("Cancel"),
            shiny::actionButton("trigger_done", "OK")
          )
        ))
      } else {
        done(TRUE)
      }
    })

    shiny::observeEvent(input$trigger_done, {
      shiny::removeModal()
      done(TRUE)
    })

    shiny::observe({
      shiny::req(done())

      confl_upload(
        title = title,
        html_text = html_text,
        imgs = imgs,
        imgs_realpath = imgs_realpath,
        id = id,
        space_key = input$space_key,
        parent_id = input$parent_id,
        type = input$type,
        session = session,
        toc = input$toc,
        toc_depth = input$toc_depth,
        code_folding = if (input$code_folding) "hide" else "none",
        supported_syntax_highlighting = supported_syntax_highlighting,
        use_original_size = input$use_original_size,
        # Already confirmed
        update = TRUE
      )

      unset_password_if_special_envvar_is_set()
    })
  }

  viewer <- shiny::dialogViewer("Preview", width = 1000, height = 800)
  shiny::runGadget(ui, server, viewer = viewer)
}

# if the user doesn't want to store the password as envvar, clear it.
unset_password_if_special_envvar_is_set <- function() {
  if (isTRUE(getOption("conflr_addin_clear_password_after_success"))) {
    message("unsetting CONFLUENCE_PASSWORD...")
    Sys.unsetenv("CONFLUENCE_PASSWORD")
  }
}

read_utf8 <- function(x) {
  paste(readLines(x, encoding = "UTF-8"), collapse = "\n")
}

extract_image_paths <- function(html_text) {
  html_doc <- xml2::read_html(html_text)
  img_nodes <- xml2::xml_find_all(html_doc, ".//img")
  img_paths <- xml2::xml_attr(img_nodes, "src")
  # exclude external images
  img_paths[!is.na(img_paths) & !grepl("^https?://", img_paths)]
}

try_get_personal_space_key <- function(username) {
  # check if the space really exists
  tryCatch(
    confl_get_space(spaceKey = paste0("~", username))$key,
    error = function(e) {
      # Do not show even warnings because it's likely to happen as the keys of personal spaces are often numeric (#30).
      NULL
    }
  )
}

wrap_with_column <- function(..., width = 2) {
  shiny::column(width = width, ...)
}

# NOTE: conflr_supported_syntax_highlighting cannot be set via GUI because
#       it's not a feature frequently used and is a bit difficult to input
#       via Shiny interface.
confl_addin_ui <- function(title, html_text, imgs, imgs_realpath,
                           space_key = NULL, parent_id = NULL, type = "page",
                           toc = FALSE, toc_depth = 7,
                           code_folding = "none",
                           use_original_size = FALSE) {
  # title bar
  title_bar_button <- miniUI::miniTitleBarButton("confirm", "Publish", primary = TRUE)
  title_bar <- miniUI::gadgetTitleBar("Preview", right = title_bar_button)

  # type (page or blogpost)
  type_input <- shiny::selectInput(inputId = "type", label = "Type", choices = c("page", "blogpost"), selected = type)

  # space_key
  space_key_input <- shiny::textInput(inputId = "space_key", label = "Space Key", value = space_key)

  # parent page ID
  parent_id_input <- shiny::textInput(inputId = "parent_id", label = "Parent page ID", value = parent_id)

  # use the original size or not
  use_original_size_input <- shiny::checkboxInput(inputId = "use_original_size", label = "Use original image sizes", value = use_original_size)

  # add TOC or not
  toc_input <- shiny::checkboxInput(inputId = "toc", label = "TOC", value = toc)
  toc_depth_input <- shiny::numericInput(
    inputId = "toc_depth",
    label = "TOC depth",
    value = toc_depth,
    width = "4em"
  )
  code_folding_input <- shiny::checkboxInput(
    inputId = "code_folding",
    label = "Fold code blocks",
    value = identical(code_folding, "hide")
  )

  # Preview
  html_text_for_preview <- embed_images(html_text, imgs, imgs_realpath)
  preview_html <- shiny::HTML(html_text_for_preview)

  miniUI::miniPage(
    title_bar,
    miniUI::miniContentPanel(
      shiny::fluidRow(
        wrap_with_column(type_input),
        wrap_with_column(space_key_input),
        wrap_with_column(parent_id_input),
        wrap_with_column(
          use_original_size_input,
          code_folding_input
        ),
        wrap_with_column(
          toc_input,
          toc_depth_input
        )
      ),
      shiny::hr(),
      shiny::h1(title, align = "center"),
      shiny::div(preview_html)
    )
  )
}
