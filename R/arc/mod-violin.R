violin_ui <- function(id) {
  ns <- NS(id)
  tagList(
    "violin widgets"
  )
}

violin_server <- function(id) {
  moduleServer(id, function(input, output, session) {

  })
}