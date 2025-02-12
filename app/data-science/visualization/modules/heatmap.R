heatmapUI <- function(id) {
  ns <- NS(id)
  card(
    card_header(
      class = "d-flex justify-content-between",
      "热图",
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
            input_switch(ns("cluster_rows"), label = "行聚类", value = TRUE),
            input_switch(ns("cluster_cols"), label = "列聚类", value = TRUE),
            input_switch(ns("show_rownames"), label = "显示行名", value = TRUE),
            input_switch(ns("show_colnames"), label = "显示列名", value = TRUE),
            conditionalPanel(
              ns = ns,
              condition = "input.show_colnames",
              selectInput(ns("angle_col"), label = "列名角度", choices = c("270", "0", "45", "90", "315"))
            ),
            selectInput(ns("clustering_method"), label = "聚类方法", choices = c("ward.D", "complete"))
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

heatmapServer <- function(id) {
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
      rv$data_ready <- rv$data %>% rename("var" = 1) %>% column_to_rownames("var")
    })

    plot_interactive <- function() {

      annotation_row <- data.frame(row.names = rownames(rv$data_ready), sp = str_extract(rownames(rv$data_ready), "^[^ ]+"))
      annotation_col <- data.frame(row.names = colnames(rv$data_ready), Group = str_extract(colnames(rv$data_ready), ".*(?=_.*)"))

      mat <- rv$data_ready %>% data.matrix()

      p <- pheatmap(
        mat,
        color = colorRampPalette(c("navy", "white", "firebrick3"))(50),  # 图例颜色，数字是等级
        scale = "row",
        cluster_rows = input$cluster_rows,
        cluster_cols = input$cluster_cols,
        clustering_distance_rows = "euclidean",
        clustering_distance_cols = "euclidean",
        clustering_method = input$clustering_method,  # MetaboAnalyst里默认的是ward.D，本来pheatmap默认的是complete
        annotation_row = annotation_row,
        annotation_col = annotation_col,
        # annotation_colors = list(
        #   Group = c(
        #     "DPP4_neg" = "#298c8c",
        #     "DPP4_pos" = "#f1a226"
        #   )
        # ),
        annotation_names_row = FALSE,
        annotation_names_col = TRUE,
        show_rownames = input$show_rownames,
        show_colnames = input$show_colnames,
        angle_col = input$angle_col
      )

      return(p)
    }

    observeEvent(input$apply, {
      rv$plot_init <- plot_interactive()
      rv$plot_labeled <- rv$plot_init
      rv$plot_final <- rv$plot_labeled
    })
    vsUniversalServer(NULL, id, rv)

  })
}