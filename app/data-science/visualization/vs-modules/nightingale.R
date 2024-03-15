nightingaleUI <- function(id) {
  ns <- NS(id)
  card(
    card_header(
      class = "d-flex justify-content-between",
      "南丁格尔玫瑰图",
      actionButton(ns("restart"), "Restart (still building)")
    ),
    layout_sidebar(border_radius = FALSE, class = "p-0",
      sidebar = sidebar(width = "900px", open = "always",
        navset_pill_list(widths = c(2, 10),
          nav_panel("Upload",
            excelInput(ns("data"), header = "Data"),
            actionButton(ns("data_handle"), label = "Handle")
          ),
          nav_panel("Main",
            selectInput(ns("x"), label = "x轴", choices = c(Choose = "")),
            selectInput(ns("y"), label = "y轴", choices = c(Choose = ""))
          ),
          nav_panel("Labels", gglabsUI(ns(NULL))),
          nav_panel("Theme", ggthemeUI(ns(NULL)))
        )
      ),
      layout_sidebar(border = FALSE,
        sidebar = sidebar(width = "300px", position = "right",
          includeMarkdown(paste0("data-science/visualization/types-readme/", id, ".md"))
        ),
        vsUniversalUI(ns(NULL))
      )
    )
  )
}

nightingaleServer <- function(id) {
  moduleServer(id, function(input, output, session) {

    ns <- session$ns

    rv <- reactiveValues()

    data_ls <- excelServer("data")

    observe({
      rv$data_original <- data_ls$data()
      rv$name <- data_ls$name()
    })

    observeEvent(input$data_handle, {
      rv$data <- rv$data_original
      rv$data$log2FC <- log2(rv$data$FC)
    })


    # df <- data_ready
    # updateSelectInput(session, "x", choices = colnames(df))
  })
}