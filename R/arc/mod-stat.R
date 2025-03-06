statUI <- function(id) {
  ns <- NS(id)
  # layout_sidebar(height = "700px", border = FALSE, class = "p-0",
  #   sidebar = sidebar(width = "200px",
  #     radioButtons(ns("nav"), label = "Statistical types",
  #       choiceNames = c("ttest"),
  #       choiceValues = c("ttest")
  #     )
  #   ),
  #   navset_hidden(
  #     id = ns("container"),
  #     nav_panel_hidden("ttest", ttestUI(ns("ttest")))
  #   )
  # )
}

statServer <- function(id) {
  moduleServer(id, function(input, output, session) {

    ns <- session$ns

    # observe({
    #   nav_select("container", input$nav)
    # })

    # lapply(stat_types, function(stat_type) {
    #   get(paste0(stat_type, "Server"))(stat_type)
    # })

  })
}