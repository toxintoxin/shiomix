pieUI <- function(id) {
  ns <- NS(id)
  card(
    card_header(
      class = "d-flex justify-content-between",
      "饼图",
      actionButton(ns("restart"), "Restart (still building)")
    ),
    layout_sidebar(border_radius = FALSE, class = "p-0",
      sidebar = sidebar(width = "700px", open = "always",
        navset_pill_list(widths = c(2, 10),
          nav_panel("Upload",
            excelInput(ns("data"), header = "Data"),
            actionButton(ns("data_handle"), label = "Handle")
          ),
          nav_panel("Main",
            selectInput(ns("fill"), label = "x轴", choices = c(Choose = "")),
            radioButtons(ns("label_view"), label = "值显示为", choices = c("Actual", "Percent"), inline = TRUE),
            radioButtons(ns("label_position"), label = "值显示在", choices = c("None", "Inside", "Outside"), inline = TRUE),
            selectInput(ns("direction"), label = "方向", choices = c("顺时针", "逆时针")),
            selectInput(ns("order"), label = "排序", choices = c("Original", "Desc", "Asc")),
            radioButtons(ns("scale_fill_manual_plan"), label = "颜色方案", choices = c("预设", "手选", "Code"), inline = TRUE),
            uiOutput(ns("scale_fill_manual_params"))
          ),
          nav_panel("Labels", gglabsUI(ns(NULL))),
          nav_panel("Theme", ggthemeUI(ns(NULL)))
        )
      ),
      layout_sidebar(border = FALSE, style = "background: #eeeddd",
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
      rv$data_arranged <- rv$data
    })

    output$scale_fill_manual_params <- renderUI({
      if (input$scale_fill_manual_plan == "预设") {
        selectInput(ns("scale_fill_manual_palette"), label = NULL, choices = c("Default"))
      } else if (input$scale_fill_manual_plan == "手选") {
        map(rv$data_arranged$var, function(label) {
          colorPickr(inputId = ns(paste0("scale_fill_manual_sp_", label)), label = label, selected = "#000000", width = "100px")
        })
      } else if (input$scale_fill_manual_plan == "Code") {
        textInput(ns("scale_fill_manual_value"), label = "HEX, 用逗号分隔")
      }
    })

    observeEvent(input$apply, {

      df <- if (input$label_view == "Actual") {
        rv$data %>%
          mutate(label = value)
      } else if (input$label_view == "Percent") {
        rv$data %>%
          mutate(label = scales::percent(value/sum(value), accuracy = 0.01))
      }

      df <- df %>% mutate(
        csum = rev(cumsum(rev(value))),
        pos = value/2 + lead(csum, 1),
        pos = if_else(is.na(pos), value/2, pos)
      )

      df <- if (input$order == "Original") {
        df
      } else if (input$order == "Asc") {
        df %>% arrange(value)
      } else if (input$order == "Desc") {
        df %>% arrange(desc(value))
      }

      df <- df %>% mutate(var = as_factor(var))

      p <- df %>%
        ggplot(aes(x = "", y = value, fill = var)) +
        geom_col()

      if (input$label_position == "Inside") {
        p <- p +
          geom_text(aes(label = label), position = position_stack(vjust = 0.5))
      } else if (input$label_position == "Outside") {
        p <- p +
          geom_label_repel(aes(y = pos, label = label), size = 4.5, nudge_x = 1, show.legend = FALSE)
      }

      if (input$scale_fill_manual_plan == "手选") {
        named_color_values <- sapply(rv$data$var, function(label) {
          input_value <- input[[paste0("scale_fill_manual_sp_", label)]]
        })
        p <- p + scale_fill_manual(values = named_color_values)
      } else if (input$scale_fill_manual_plan == "Code") {
        color_hex <- input$scale_fill_manual_value %>%
          str_replace_all(pattern = " ", replacement = "") %>%
          strsplit(split = ",") %>%
          unlist()
        p <- p + scale_fill_manual(values = color_hex)
      }

      p <- p + coord_polar(theta = "y", direction = if (input$direction == "顺时针") {-1} else if (input$direction == "逆时针") {1})

      rv$plot_init <- p
      rv$plot_labeled <- gglabsServer(NULL, rv$plot_init)
      rv$plot_final <- ggthemeServer(NULL, rv$plot_labeled)
    })
    vsUniversalServer(NULL, id, rv)

  })
}