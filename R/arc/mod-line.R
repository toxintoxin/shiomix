line_ui <- function(id) {
  ns <- NS(id)
  tagList(
    "line widgets"
  )
}

line_server <- function(id) {
  moduleServer(id, function(input, output, session) {

  })
}