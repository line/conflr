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
#'
#' @export
confl_create_post_from_Rmd <- function(Rmd_file = NULL) {
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
      pandoc_args = "--wrap=none",
      md_extensions = "-tex_math_single_backslash-tex_math_dollars-raw_tex"
    ),
    encoding = "UTF-8"
  )

  front_matter <- rmarkdown::yaml_front_matter(Rmd_file, "UTF-8")

  confl_addin_upload(
    md_file = md_file,
    title = front_matter$title,
    tags = front_matter$tags
  )
}

confl_addin_upload <- function(md_file, title, tags) {
  # conflr doesn't insert a title in the content automatically
  md_text <- read_utf8(md_file)
  html_text <- commonmark::markdown_html(md_text)

  md_dir <- dirname(md_file)
  imgs <- extract_image_paths(html_text)
  imgs <- curl::curl_unescape(imgs)
  # imgs might be absolute, relative to md_dir, or relative to the current dir.
  imgs_realpath <- ifelse(file.exists(imgs), imgs, file.path(md_dir, imgs))

  html_text_for_preview <- embed_images(html_text, imgs, imgs_realpath)

  # Shiny UI -----------------------------------------------------------
  ui <- miniUI::miniPage(
    miniUI::gadgetTitleBar("Preview",
      right = miniUI::miniTitleBarButton("done", "Publish", primary = TRUE)
    ),
    miniUI::miniContentPanel(
      shiny::fluidRow(
        shiny::column(
          width = 2,
          shiny::selectInput(
            inputId = "type", label = "type",
            choices = eval(formals(confl_post_page)$type),
          )
        ),
        shiny::column(
          width = 2,
          shiny::textInput(
            inputId = "spaceKey", label = "Space Key",
            value = try_get_personal_space_key()
          )
        ),
        shiny::column(
          width = 2,
          shiny::textInput(
            inputId = "ancestors", label = "Parent page ID",
            value = NULL
          )
        )
      ),
      shiny::hr(),
      shiny::h1(title, align = "center"),
      shiny::div(
        shiny::HTML(
          html_text_for_preview
        )
      )
    )
  )

  # Shiny Server -------------------------------------------------------
  server <- function(input, output, session) {
    shiny::observeEvent(input$done, {

      # check if there is an existing page
      existing_pages <- confl_list_pages(title = title, spaceKey = input$spaceKey)
      if (existing_pages$size == 0) {
        # if the page doesn't exist, create a blank page
        blank_page <- confl_post_page(
          type = input$type,
          spaceKey = input$spaceKey,
          title = title,
          body = "",
          ancestors = input$ancestors
        )
        id <- blank_page$id
      } else {
        ans <- rstudioapi::showQuestion(
          "Update?",
          glue::glue("There is already an existing page named '{title}'.
                      Are you sure to overwrite it?"),
          ok = "OK", cancel = "cancel"
        )
        if (!ans) stop("Cancel to upload.", call. = FALSE)

        id <- existing_pages$results[[1]]$id
      }

      progress <- shiny::Progress$new(session, min = 0, max = 2)
      on.exit(progress$close())

      # Step 1) Upload Images
      progress$set(message = "Checking the existing images...")

      # Check if the images already exist
      imgs_exist <- confl_list_attachments(id)
      imgs_exist_ids <- purrr::map_chr(imgs_exist$results, "id")
      names(imgs_exist_ids) <- purrr::map_chr(imgs_exist$results, "title")

      progress$set(message = "Uploading the images...")
      num_imgs <- length(imgs)
      for (i in seq_along(imgs)) {
        progress$set(detail = imgs[i])

        # attempt to avoid rate limits
        Sys.sleep(0.2)

        img_id <- imgs_exist_ids[basename(imgs[i])]
        if (is.na(img_id)) {
          confl_post_attachment(id, imgs_realpath[i])
        } else {
          confl_update_attachment_data(id, img_id, imgs_realpath[i])
        }

        progress$set(value = i / num_imgs)
      }

      # Step 2) Upload the document
      progress$set(message = "Uploading the document...")

      result <- confl_update_page(
        id = id,
        title = title,
        body = html_text
      )

      progress$set(value = 2, message = "Done!")
      Sys.sleep(2)

      invisible(shiny::stopApp())
      browseURL(paste0(result$`_links`$base, result$`_links`$webui))
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
  img_paths[!is.na(img_paths) & !grepl(pattern = "^http", img_paths)]
}

try_get_personal_space_key <- function() {
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
    warning(e)
    return(NULL)
  })
}
