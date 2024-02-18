boxUI <- function(type) {
  ns <- NS(type)
  tagList(
    "box widgets"
  )
}

boxServer <- function(data_ready) {
  moduleServer(NULL, function(input, output, session) {

  })
}