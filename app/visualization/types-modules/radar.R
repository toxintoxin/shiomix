radarUI <- function(type) {
  ns <- NS(type)
  tagList(
    "radar widgets"
  )
}

radarServer <- function(data_ready) {
  moduleServer(NULL, function(input, output, session) {

  })
}