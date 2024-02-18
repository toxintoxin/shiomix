pieUI <- function(type) {
  ns <- NS(type)
  tagList(
    "pie widgets"
  )
}

pieServer <- function(data_ready) {
  moduleServer(NULL, function(input, output, session) {

  })
}