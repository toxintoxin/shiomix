pca_ui <- function(id) {
  ns <- NS(id)
  card(
    card_header(
      class = "d-flex justify-content-between",
      "PCA主成分分析",
      actionButton(ns("restart"), "Restart (still building)")
    ),
    layout_sidebar(border_radius = FALSE, class = "p-0",
      sidebar = sidebar(width = "700px", open = "always",
        navset_pill_list(widths = c(2, 10),
          nav_panel("Upload",
            excelInput(ns("data"), header = "Data")
          ),
          nav_panel("Main",
            radioButtons(ns("point_shape"), label = NULL, choices = c("圆点", "黑色边框圆点", "各具形状"), inline = TRUE),
            numericInput(ns("point_size"), "大小", value = 2, min = 0.5, max = 4),
            div(style = "display: flex;",
              div(style = "width: 26%; height: 85px;", input_switch(ns("ellipse"), "置信椭圆")),
              conditionalPanel(
                ns = ns,
                condition = "input.ellipse", style = "display: flex; width: 74%; gap: 2.7%;",
                div("置信椭圆参数")
              )
            ),
            div(style = "display: flex;",
              input_switch(ns("loadings"), "展示各变量贡献", value = FALSE),
              conditionalPanel(
                ns = ns,
                condition = "input.loadings", style = "display: flex; width: 74%; gap: 2.7%;",
                numericInput(ns("loadings_scale"), "缩放", min = 1, max = 10000000, value = 1)
              )
            )
          ),
          # nav_panel("Labels", gglabs_ui(ns(NULL))),
          # nav_panel("Theme", ggtheme_ui(ns(NULL)))
        )
      ),
      # layout_sidebar(border = FALSE, style = "background: #eeeddd",
      #   sidebar = sidebar(width = "300px", position = "right",
      #     includeMarkdown(paste0("data-science/visualization/types-readme/", id, ".md"))
      #   ),
      #   vsUniversal_ui(ns(NULL))
      # )
    )
  )
}

pca_server <- function(id) {
  moduleServer(id, function(input, output, session) {

    ns <- session$ns

    rv <- reactiveValues()

    data_ls <- excel_server("data")

    observe({
      rv$data_original <- data_ls$data()
      rv$name <- data_ls$name()
    })

    plot_interactive <- function() {

      rv$data_ready <- rv$data_original %>%
        column_to_rownames("var") %>%
        t() %>%
        as.data.frame() %>%
        rownames_to_column("sample")
        # mutate(group = str_extract(sample, ".*(?=_.*)"))

      pca_result <- rv$data_ready %>%
        prcomp(scale. = FALSE)

      percentage <- scales::percent(pca_result$sdev^2 / sum(pca_result$sdev^2), accuracy = 0.01)

      pc_labels <- paste0("PC", seq_along(pca_result$sdev), " (", percentage, ")")

      scree_data <- data.frame(
        PC = 1:length(pca_result$sdev),
        VarianceExplained = pca_result$sdev^2 / sum(pca_result$sdev^2)
      )

      scree_plot <- ggplot(scree_data, aes(x = PC, y = VarianceExplained)) +
        geom_bar(stat = "identity") +
        labs(title = "Scree Plot", x = "Principal Component", y = "Proportion of Variance Explained")

      individual_coord <- pca_result$x

      p <- individual_coord %>%
        ggplot(aes(x = PC1, y = PC2, colour = group))

      percentage <- round(pca_result$sdev^2 / sum(pca_result$sdev^2) * 100, 2)
      percentage <- paste0(colnames(pca_result$x), " (", percentage, "%", ")") %>% as.list()

      pcaLoadings <- as.data.frame(pca_result$rotation) %>% rownames_to_column("var")

      p <- p + geom_point()

      if (input$point_shape == "圆点") {
        p <- p + geom_point(size = input$point_size)
      } else if (input$point_shape == "黑色边框圆点") {
        p <- p + geom_point(size = input$point_size, shape = 21, stroke = 1, colour = "black", aes(fill = group))
      } else if (input$point_shape == "各具形状") {
        p <- p + geom_point(size = input$point_size, aes(shape = group))
      }
      if (input$ellipse == TRUE) {
        p <- p + stat_ellipse(geom = "polygon", level = 0.95, alpha = 0.2, linewidth = 0.2, colour = "black", aes(fill = group), show.legend = FALSE)
      }
      if (input$loadings == TRUE) {
        p <- p + geom_segment(data = pcaLoadings, aes(x = 0, y = 0, xend = (PC1*input$loadings_scale), yend = (PC2*input$loadings_scale)), arrow = arrow(length = unit(5, "mm")), colour = "black") +
          annotate("text", x = (pcaLoadings$PC1*input$loadings_scale), y = (pcaLoadings$PC2*input$loadings_scale), label = pcaLoadings$var)
      }
      return(p)

    }

    observeEvent(input$apply, {
      rv$plot_init <- plot_interactive()
      rv$plot_labeled <- gglabs_server(NULL, rv$plot_init)
      rv$plot_final <- ggtheme_server(NULL, rv$plot_labeled)
    })
    # vsUniversal_server(NULL, id, rv)

  })
}