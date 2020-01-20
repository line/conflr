# Copyright (C) 2019 LINE Corporation
#
# conflr is free software; you can redistribute it and/or modify it under the
# terms of the GNU General Public License as published by the Free Software
# Foundation, version 3.
#
# conflr is distributed in the hope that it will be useful, but WITHOUT ANY
# WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR
# A PARTICULAR PURPOSE. See <http://www.gnu.org/licenses/> for more details.

# A mock of shiny::Progress() for console usages
ConsoleProgress <- R6::R6Class(
  'ConsoleProgress',
  public = list(
    initialize = function(...) {
      # All arguments are ignored
    },
    set = function(value = NULL, message = NULL, detail = NULL) {
      # value is ignored

      # If message is set, show the message
      if (!is.null(message)) {
        message(message)
      }
    },
    close = function() {
      # Do nothing
    }
  ),

  private = list()
)

new_progress <- function(session = NULL, min = 0, max = 1) {
  if (!is.null(session)) {
    shiny::Progress$new(session, min = min, max = max)
  } else {
    ConsoleProgress$new()
  }
}
