#' The application server-side
#'
#' @param input,output,session Internal parameters for `{shiny}`.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_server <- function(input, output, session) {

  # increase the upload limit to 2GB
  options(shiny.maxRequestSize = 2000 * 1024^2)

  sidebar_link_ids <- c(

    "ttest",
    "pie", "bar", "volcano", "nightingale",

    "data_preprocessing",
    # minimal test
    "minimal_test",
    "formula_handle",
    "tf",

    "md5_check"

  )

  # shinyjs::onclick("homepage", {
  #   # 执行与 observeEvent 类似的逻辑
  #   freezeReactiveValue(input, "tabs")
  #   updateTabsetPanel(session, inputId = "tabs", selected = "homepage")
  # })

  # # add observers to switch to the clicked link's tab:
  # lapply(sidebar_link_ids, \(id) {
  #   observeEvent(input[[id]], {
  #     freezeReactiveValue(input, "tabs")
  #     updateTabsetPanel(session = session, inputId = "tabs", selected = id)
  #   })
  # })

  observe(nav_select("container", input$radio))

  tfServer("tf")
  ppServer("pp")



  minimal_test_server("mini")


}
