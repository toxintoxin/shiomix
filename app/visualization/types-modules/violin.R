violinUI <- function(type) {
  ns <- NS(type)
  tagList(
    "violin widgets"
  )
}

violinServer <- function(data_ready) {
  moduleServer(NULL, function(input, output, session) {

  })
}