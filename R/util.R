# Copyright (C) 2019 LINE Corporation
#
# conflr is free software; you can redistribute it and/or modify it under the
# terms of the GNU General Public License as published by the Free Software
# Foundation, version 3.
#
# conflr is distributed in the hope that it will be useful, but WITHOUT ANY
# WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR
# A PARTICULAR PURPOSE. See <http://www.gnu.org/licenses/> for more details.


# Util
`%|""|%` <- function(lhs, rhs) {
  if (is.null(lhs) || identical(lhs, "")) rhs else lhs
}

ask_confluence_url <- function() rstudioapi::showPrompt("URL", "Base URL of Confluence API: ", default = "https://")
ask_confluence_username <- function() rstudioapi::showPrompt("Username", "Username for Confluence: ", default = "")
ask_confluence_password <- function() rstudioapi::askForPassword("Password for Confluence: ")

confl_verb <- function(verb, path, ...) {
  base_url <- Sys.getenv("CONFLUENCE_URL") %|""|% ask_confluence_url()
  # remove trailing /
  base_url <- stringi::stri_replace_last_regex(base_url, "/$", "")

  username <- Sys.getenv("CONFLUENCE_USERNAME") %|""|% ask_confluence_username()
  password <- Sys.getenv("CONFLUENCE_PASSWORD") %|""|% ask_confluence_password()

  res <- httr::VERB(
    verb = verb,
    url  = glue::glue("{base_url}{path}"),
    httr::authenticate(username, password),
    ...
  )

  if (httr::status_code(res) >= 300) {
    stop(httr::http_condition(res, type = "error"),
         httr::content(res))
  }

  # If the request succeeds, cache the credentials.
  Sys.setenv(CONFLUENCE_URL = base_url)
  Sys.setenv(CONFLUENCE_USERNAME = username)
  Sys.setenv(CONFLUENCE_PASSWORD = password)

  res
}

embed_images <- function(html_text, imgs, imgs_realpath) {
  for (i in seq_along(imgs)) {
    locs <- stringi::stri_locate_all_regex(html_text, imgs[[i]])[[1]]
    for (loc in rev(split(locs, row(locs)))) {
      stringi::stri_sub(html_text, loc[1], loc[2]) <- knitr::image_uri(imgs_realpath[[i]])
    }
  }

  html_text
}
