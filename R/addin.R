# Copyright (C) 2019 LINE Corporation
#
# conflr is free software; you can redistribute it and/or modify it under the
# terms of the GNU General Public License as published by the Free Software
# Foundation, version 3.
#
# conflr is distributed in the hope that it will be useful, but WITHOUT ANY
# WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR
# A PARTICULAR PURPOSE. See <http://www.gnu.org/licenses/> for more details.

#' Publish R Markdown Document to 'Confluence'
#'
#' Knit and post a given R Markdown file to 'Confluence'.
#'
#' @param Rmd_file Path to an .Rmd file.
#' @param interactive If `FALSE`, shiny interface is not launched.
#' @param params If provided, a list of named parameters that override custom
#'   params in the YAML front-matter.
#' @param ... Ignored.
#' @param title Title of the post.
#' @param type If provided, this overwrites the YAML front matter type
#' @param space_key The space key to find content under.
#' @param parent_id The page ID of the parent pages.
#' @param toc If `TRUE`, add TOC.
#' @param toc_depth The depth of the TOC. Ignored when `toc` is `FALSE`.
#' @param update If `TRUE`, overwrite the existing page (if it exists).
#' @param image_size_default
#'   The default width of images in pixel. If `NULL`, images are displayed in their original sizes.
#' @param supported_syntax_highlighting
#'   A named character vector of supported syntax highlighting other than default (e.g. `c(r = "r")`).
#'
#' @inheritParams confl_content
#'
#' @details
#' `title`, `type`, `space_key`, `parent_id`, `toc`, `toc_depth`, `update`, and
#' `use_original_size` can be specified as `confluence_settings` item in the
#' front-matter of the Rmd file to knit. The arguments of
#' `confl_create_post_from_Rmd()` overwrite these settings if provided.
#'
#' @export
confl_create_post_from_Rmd <- function(
  Rmd_file,
  interactive = NULL,
  params = NULL,
  ...,
  title = NULL,
  # Use snake case for user-facing functions and use the actual API parameter name
  # in camel case for simple binding functions.
  space_key = NULL,
  type = NULL,
  parent_id = NULL,
  toc = NULL,
  toc_depth = NULL,
  supported_syntax_highlighting = getOption("conflr_supported_syntax_highlighting"),
  update = NULL,
  use_original_size = NULL) {

  ellipsis::check_dots_used()

  # sanity checks -----------------------------------------------------------

  if (is.null(interactive)) {
    interactive <- interactive()
  }

  if (!tolower(tools::file_ext(Rmd_file)) %in% c("rmd", "rmarkdown")) {
    abort(glue::glue("{basename(Rmd_file)} is not .Rmd file!"))
  }

  # confirm the username and password are valid (and username will be useful later).
  tryCatch(
    username <- confl_get_current_user()$username,
    error = function(e) {
      if (stringi::stri_detect_fixed(as.character(e), "Unauthorized (HTTP 401)")) {
        abort("Invalid credentials!")
      } else {
        cnd_signal(e)
      }
    }
  )

  # knit --------------------------------------------------------------------

  knitr::opts_chunk$set(
    collapse = TRUE,
    comment = "#>"
  )
  md_file <- rmarkdown::render(
    input = Rmd_file,
    output_format = rmarkdown::md_document(
      variant = "commonmark",
      pandoc_args = "--wrap=none",
      md_extensions = "-tex_math_single_backslash-tex_math_dollars-raw_tex"
    ),
    encoding = "UTF-8",
    params = params,
    # TODO: I'm not fully sure the global env is always the right place to knit, but this is needed to avoid
    #       an error related to data.table (#29). If this doesn't work, I need to add this code (c.f. https://github.com/Rdatatable/data.table/blob/5ceda0f383f91b7503d4a236ee4e7438724340be/R/cedta.R#L13):
    #   assignInNamespace("cedta.pkgEvalsUserCode", c(data.table:::cedta.pkgEvalsUserCode, "conflr"), "data.table")
    env = globalenv()
  )

  # combine settings --------------------------------------------------------------------

  # Get confluence_settings
  front_matter <- rmarkdown::yaml_front_matter(Rmd_file, "UTF-8")
  confluence_settings <- front_matter$confluence_settings %||% list()

  # title can be specified as a seperate item on front matter
  # override title if it's specified as the argument of confl_create_post_from_Rmd
  confluence_settings$title <- confluence_settings$title %||% front_matter$title

  # 1. Use confluence_settings on the front matter if it's available
  # 2. Override the option if it's specified as the argument of confl_create_post_from_Rmd
  confluence_settings_from_args <- list(
    title = title,
    space_key = space_key,
    type = type,
    parent_id = parent_id,
    toc = toc,
    toc_depth = toc_depth,
    supported_syntax_highlighting = supported_syntax_highlighting,
    update = update,
    use_original_size = use_original_size
  )
  confluence_settings <- purrr::list_modify(
    confluence_settings,
    !!!purrr::compact(confluence_settings_from_args)
  )

  # On some Confluence, the key of a personal space can be guessed from the username
  if (is.null(confluence_settings$space_key)) {
    confluence_settings$space_key <- try_get_personal_space_key(username)
  }

  # conflr doesn't insert a title in the content automatically
  md_text <- read_utf8(md_file)

  # Replace <ac:...> and <ri:...> because they are not recognized as proper tags
  # by commonmark and accordingly get escaped. We need to replace the namespace
  # to bypass the unwanted conversions. The tags will be restored later in
  # confl_upload().
  md_text <- mark_confluence_namespaces(md_text)

  html_text <- commonmark::markdown_html(md_text)

  md_dir <- dirname(md_file)
  imgs <- extract_image_paths(html_text)
  imgs_unescaped <- curl::curl_unescape(imgs)

  # imgs might be absolute, relative to md_dir, or relative to the current dir.
  imgs_realpath <- ifelse(file.exists(imgs_unescaped), imgs, file.path(md_dir, imgs_unescaped))

  # upload ------------------------------------------------------------------

  if (interactive) {
    exec(
      confl_upload_interactively,
      !!! confluence_settings,
      html_text = html_text,
      imgs = imgs,
      imgs_realpath = imgs_realpath
    )

    # if the user doesn't want to store the password as envvar, clear it.
    if (isTRUE(getOption("conflr_addin_clear_password_after_success"))) {
      message("unsetting CONFLUENCE_PASSWORD...")
      Sys.unsetenv("CONFLUENCE_PASSWORD")
    }
  } else {
    exec(
      confl_upload,
      !!! confluence_settings,
      html_text = html_text,
      imgs = imgs,
      imgs_realpath = imgs_realpath,
      interactive = interactive
    )
  }
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

confl_upload_interactively <- function(title, space_key, type, parent_id, html_text,
                                       imgs, imgs_realpath,
                                       toc = FALSE, toc_depth = 7,
                                       supported_syntax_highlighting = getOption("conflr_supported_syntax_highlighting"),
                                       use_original_size = FALSE) {

  # Shiny UI -----------------------------------------------------------
  ui <- confl_addin_ui(
    title = title,
    space_key = space_key,
    type = eval(formals(confl_post_page)$type),
    parent_id = parent_id,
    html_text = html_text,
    imgs = imgs,
    imgs_realpath = imgs_realpath,
    toc = toc,
    toc_depth = toc_depth,
    use_original_size = use_original_size
  )

  # Shiny Server -------------------------------------------------------
  server <- function(input, output, session) {
    shiny::observeEvent(input$done, {

      # TODO: this warning cannot be shown to users. Consider using shinyFeedback
      shiny::validate(
        shiny::need(input$space_key != "", "Please provide a space key")
      )

      confl_upload(
        title = title,
        space_key = input$space_key,
        type = input$type,
        parent_id = input$parent_id,
        session = session,
        html_text = html_text,
        imgs = imgs,
        imgs_realpath = imgs_realpath,
        toc = input$toc,
        toc_depth = input$toc_depth,
        supported_syntax_highlighting = supported_syntax_highlighting,
        use_original_size = input$use_original_size
      )
    })
  }

  viewer <- shiny::dialogViewer("Preview", width = 1000, height = 800)
  shiny::runGadget(ui, server, viewer = viewer)
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
    space <- confl_get_space(spaceKey = paste0("~", username)),
    error = function(e) {
      # Do not show even warnings because it's likely to happen as the keys of personal spaces are often numeric (#30).
      return(NULL)
    }
  )

  space$key
}

wrap_with_column <- function(..., width = 2) {
  shiny::column(width = width, ...)
}

# NOTE: conflr_supported_syntax_highlighting cannot be set via GUI because
#       it's not a feature frequently used and is a bit difficult to input
#       via Shiny interface.
confl_addin_ui <- function(title, space_key, type, parent_id, html_text,
                           imgs, imgs_realpath,
                           toc = FALSE, toc_depth = 7,
                           use_original_size = FALSE) {
  # title bar
  title_bar_button <- miniUI::miniTitleBarButton("done", "Publish", primary = TRUE)
  title_bar <- miniUI::gadgetTitleBar("Preview", right = title_bar_button)

  # type (page or blogpost)
  type_input <- shiny::selectInput(inputId = "type", label = "Type", choices = type)

  # space_key
  space_key_input <- shiny::textInput(inputId = "space_key", label = "Space Key", value = space_key)

  # parent page ID
  parent_id_input <- shiny::textInput(inputId = "parent_id", label = "Parent page ID", value = parent_id)

  # use the original size or not
  use_original_size_input <- shiny::checkboxInput(inputId = "use_original_size", label = "Use original image sizes", value = use_original_size)

  # add TOC or not
  toc_input <- shiny::checkboxInput(inputId = "toc", label = "TOC", value = toc)
  toc_depth_input <- shiny::numericInput(inputId = "toc_depth", label = "TOC depth", value = toc_depth)

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
        wrap_with_column(use_original_size_input, toc_input, toc_depth_input, width = 4)
      ),
      shiny::hr(),
      shiny::h1(title, align = "center"),
      shiny::div(preview_html)
    )
  )
}
