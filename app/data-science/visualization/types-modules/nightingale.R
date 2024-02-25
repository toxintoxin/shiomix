nightingaleUI <- function(id) {
  ns <- NS(id)
  tagList(
    selectInput(ns("x"), "select x", choices = ""),
    selectInput(ns("y"), "select y", choices = ""),
    selectInput(ns("fill"), "select fill", choices = "")
  )
}

nightingaleServer <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    # df <- data_ready
    # updateSelectInput(session, "x", choices = colnames(df))
  })
}