heatmapUI <- function(type) {
  ns <- NS(type)
  tagList(
    "heatmap widgets"
  )
}

heatmapServer <- function(data_ready) {
  moduleServer(NULL, function(input, output, session) {

  })
}