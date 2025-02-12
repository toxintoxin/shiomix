server <- function(input, output, session) {
  sidebar_link_ids <- c(
    "ttest",
    "pie", "bar", "volcano", "nightingale",

    "data_preprocessing",

    "formula_handle",
    "tf",

    "md5_check"
  )

  shinyjs::onclick("homepage", {
    # 执行与 observeEvent 类似的逻辑
    freezeReactiveValue(input, "tabs")
    updateTabsetPanel(session, inputId = "tabs", selected = "homepage")
  })

  # add observers to switch to the clicked link's tab:
  lapply(sidebar_link_ids, \(id) {
    observeEvent(input[[id]], {
      freezeReactiveValue(input, "tabs")
      updateTabsetPanel(session = session, inputId = "tabs", selected = id)
    })
  })



  # data science
  # aggrServer("aggr")
#   statServer("stat")
#   lapply(stat_modules, function(stat_module) {
#     get(paste0(stat_module, "Server"))(stat_module)
#   })
#   vsServer("vs")
#   lapply(vs_modules, function(vs_module) {
#     get(paste0(vs_module, "Server"))(vs_module)
#   })

  # omics
  ppServer("pp")

#   # mass tools
#   # enviPatServer("enviPat")
#   lapply(enviPat_modules, function(enviPat_module) {
#     get(paste0(enviPat_module, "Server"))(enviPat_module)
#   })
#   tfServer("tf")

#   # toolkits
#   md5Server("md5")

#   observe({
#     nav_select(id = "nav_main", selected = input$nav)
#   })
}