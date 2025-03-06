boxUI <- function(id) {
  ns <- NS(id)
  tagList(
    "box widgets"
  )
}

boxServer <- function(id) {
  moduleServer(id, function(input, output, session) {

  })
}