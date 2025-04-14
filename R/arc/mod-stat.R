stat_ui <- function(id) {
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
  #     nav_panel_hidden("ttest", ttest_ui(ns("ttest")))
  #   )
  # )
}

stat_server <- function(id) {
  moduleServer(id, function(input, output, session) {

    ns <- session$ns

    # observe({
    #   nav_select("container", input$nav)
    # })

    # lapply(stat_types, function(stat_type) {
    #   get(paste0(stat_type, "_server"))(stat_type)
    # })

  })
}