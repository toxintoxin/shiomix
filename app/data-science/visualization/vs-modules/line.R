lineUI <- function(id) {
  ns <- NS(id)
  tagList(
    "line widgets"
  )
}

lineServer <- function(id) {
  moduleServer(id, function(input, output, session) {

  })
}