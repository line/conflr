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
#' @param Rmd_file path to a .Rmd file. If `NULL`, use the active document.
#' @param interactive If `FALSE` shiny interface is not launched.
#' @param title If provided this overwrites the YAML front matter title.
#' @param params If provided, a list of named parameters that override custom
#'   params in the YAML front-matter.
#' @param ... Addtional arguments passed to `confl_console_upload()`.
#'
#' @export
confl_create_post_from_Rmd <- function(Rmd_file = NULL, interactive = NULL,
                                       title = NULL, params = NULL, ...) {

  if (is.null(interactive)) {
    interactive <- interactive()
  }

  if (is.null(Rmd_file) && rstudioapi::isAvailable()) {
    Rmd_file <- rstudioapi::getSourceEditorContext()$path
    if (identical(Rmd_file, "")) {
      # Probably "UntitledX"
      stop("Please save the .Rmd file first!", call. = FALSE)
    }
  }

  if (tolower(tools::file_ext(Rmd_file)) != "rmd") {
    stop(glue::glue("{basename(Rmd_file)} is not .Rmd file!"), call. = FALSE)
  }

  # confirm the username and password are valid.
  tryCatch(
    confl_get_current_user(),
    error = function(e) {
      if (stringi::stri_detect_fixed(as.character(e), "Unauthorized (HTTP 401)")) {
        stop("Invalid credentials!", call. = FALSE)
      } else {
        stop(e, call. = FALSE)
      }
    }
  )

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

  # set confl setting
  front_matter <- rmarkdown::yaml_front_matter(Rmd_file, "UTF-8")

  # 1. Use confluence_settings on the front matter if it's available
  # 2. Override the option if it's specified as the argument of confl_create_post_from_Rmd
  confluence_settings <- purrr::list_modify(front_matter$confluence_settings %||% list(), ...)

  # title is specified as a seperate item on front matter
  # override title if it's specified as the argument of confl_create_post_from_Rmd
  confluence_settings$title <- title %||% front_matter$title

  if (!interactive) {
    # TODO: these arguments should be logical, so we need to check and fill it.
    #       But, this should be done inside confl_addin_upload()...
    if (is.null(confluence_settings$update)) {
      confluence_settings$update <- FALSE
    }
    if (is.null(confluence_settings$use_original_size)) {
      confluence_settings$use_original_size <- FALSE
    }
  }

  if (interactive) {
    confl_addin_upload(
      md_file = md_file,
      title = confluence_settings$title,
      tags = confluence_settings$tags,
      spaceKey = confluence_settings$spaceKey,
      parent_id = confluence_settings$parent_id
    )

    # if the user doesn't want to store the password as envvar, clear it.
    if (isTRUE(getOption("conflr_addin_clear_password_after_success"))) {
      message("unsetting CONFLUENCE_PASSWORD...")
      Sys.unsetenv("CONFLUENCE_PASSWORD")
    }
  } else {
    confl_console_upload(
      md_file = md_file,
      title = confluence_settings$title,
      tags = confluence_settings$tags,
      spaceKey = confluence_settings$spaceKey,
      type = confluence_settings$type,
      parent_id = confluence_settings$parent_id,
      update = confluence_settings$update,
      use_original_size = confluence_settings$use_original_size
    )
  }
}

confl_addin_upload <- function(md_file, title, tags, spaceKey = NULL, parent_id = NULL) {
  # conflr doesn't insert a title in the content automatically
  md_text <- read_utf8(md_file)
  html_text <- commonmark::markdown_html(md_text)

  md_dir <- dirname(md_file)
  imgs <- extract_image_paths(html_text)
  imgs <- curl::curl_unescape(imgs)

  # imgs might be absolute, relative to md_dir, or relative to the current dir.
  imgs_realpath <- ifelse(file.exists(imgs), imgs, file.path(md_dir, imgs))

  # Shiny UI -----------------------------------------------------------
  ui <- conflr_addin_ui(
    title = title,
    spaceKey = spaceKey,
    type = eval(formals(confl_post_page)$type),
    parent_id = parent_id,
    html_text = html_text,
    imgs = imgs,
    imgs_realpath = imgs_realpath
  )

  # Shiny Server -------------------------------------------------------
  server <- function(input, output, session) {
    shiny::observeEvent(input$done, {
      confl_upload(
        title = title,
        spaceKey = input$spaceKey,
        type = input$type,
        ancestors = input$ancestors,
        session = session,
        html_text = html_text,
        imgs = imgs,
        imgs_realpath = imgs_realpath,
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

try_get_personal_space_key <- function(verbose = FALSE) {
  tryCatch({
    # get the current username
    username <- confl_get_current_user()$username
    if (is.null(username)) {
      warning("Failed to get username", call. = FALSE)
      return(NULL)
    }

    spaceKey <- paste0("~", username)
    # check if the space really exists
    space <- confl_get_space(spaceKey = spaceKey)
    space$key
  },
  error = function(e) {
    if (verbose) {
      # by default, do not show the error, because the keys of personal spaces are often numeric (#30).
      warning(e, call. = FALSE)
    }
    return(NULL)
  }
  )
}

wrap_with_column <- function(..., width = 2) {
  shiny::column(width = width, ...)
}

conflr_addin_ui <- function(title, spaceKey, type, parent_id, html_text, imgs, imgs_realpath) {
  # title bar
  title_bar_button <- miniUI::miniTitleBarButton("done", "Publish", primary = TRUE)
  title_bar <- miniUI::gadgetTitleBar("Preview", right = title_bar_button)

  # type (page or blogpost)
  type_input <- shiny::selectInput(inputId = "type", label = "Type", choices = type)

  # spaceKey
  spaceKey <- spaceKey %||% try_get_personal_space_key()
  spaceKey_input <- shiny::textInput(inputId = "spaceKey", label = "Space Key", value = spaceKey)

  # parent page ID
  ancestors_input <- shiny::textInput(inputId = "ancestors", label = "Parent page ID", value = parent_id)

  # use the original size or not
  use_original_size_input <- shiny::checkboxInput(inputId = "use_original_size", label = "Use original image sizes", value = FALSE)

  # Preview
  html_text_for_preview <- embed_images(html_text, imgs, imgs_realpath)
  preview_html <- shiny::HTML(html_text_for_preview)

  miniUI::miniPage(
    title_bar,
    miniUI::miniContentPanel(
      shiny::fluidRow(
        wrap_with_column(type_input),
        wrap_with_column(spaceKey_input),
        wrap_with_column(ancestors_input),
        wrap_with_column(use_original_size_input, width = 4)
      ),
      shiny::hr(),
      shiny::h1(title, align = "center"),
      shiny::div(preview_html)
    )
  )
}
