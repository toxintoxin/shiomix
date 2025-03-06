violinUI <- function(id) {
  ns <- NS(id)
  tagList(
    "violin widgets"
  )
}

violinServer <- function(id) {
  moduleServer(id, function(input, output, session) {

  })
}