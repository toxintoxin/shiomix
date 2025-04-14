box_ui <- function(id) {
  ns <- NS(id)
  tagList(
    "box widgets"
  )
}

box_server <- function(id) {
  moduleServer(id, function(input, output, session) {

  })
}