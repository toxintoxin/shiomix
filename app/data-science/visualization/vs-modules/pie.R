pieUI <- function(id) {
  ns <- NS(id)
  card(
    card_header(
      class = "d-flex justify-content-between",
      "饼图",
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
            selectInput(ns("fill"), label = "x轴", choices = c(Choose = "")),
            selectInput(ns("direction"), label = "方向", choices = c("顺时针", "逆时针")),
            selectInput(ns("order"), label = "排序", choices = c("Original", "Desc", "Asc")),
            radioButtons(ns("aes_fill"), label = "颜色方案", choices = c("预设", "自定义"), inline = TRUE),
            uiOutput(ns("aes_fill_plan"))
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

pieServer <- function(id) {
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
      updateSelectInput(session, "fill", choices = c(Choose = "", colnames(rv$data)))
    })

    output$aes_fill_plan <- renderUI({
      if (input$aes_fill == "预设") {
        selectInput(ns("aes_fill_palette"), label = NULL, choices = c("Default"))
      } else if (input$aes_fill == "自定义") {
        map(rv$data$var, function(label) {
          colorPickr(inputId = ns(paste0("aes_fill_", label)), label = label, selected = "#000000", width = "100px")
        })
      }
    })

    observeEvent(input$apply, {

      df_ordered <- if (input$order == "Original") {
        rv$data %>% mutate(var = as_factor(var))
      } else if (input$order == "Desc") {
        rv$data %>% mutate(var = fct_reorder(var, value, .desc = TRUE))
      } else if (input$order == "Asc") {
        rv$data %>% mutate(var = fct_reorder(var, value, .desc = FALSE))
      }

      named_color_values <- sapply(rv$data$var, function(label) {
        input_value <- input[[paste0("aes_fill_", label)]]
      })

      p <- df_ordered %>%
        ggplot() +
        geom_bar(aes(x = "", y = value, fill = var), stat = "identity") +
        coord_polar("y", direction = if (input$direction == "顺时针") {-1} else if (input$direction == "逆时针") {1}) +
        scale_fill_manual(values = named_color_values)

      rv$plot_init <- p
      rv$plot_labeled <- gglabsServer(NULL, rv$plot_init)
      rv$plot_final <- ggthemeServer(NULL, rv$plot_labeled)
    })
    vsUniversalServer(NULL, id, rv)

  })
}