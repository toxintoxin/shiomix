pcaUI <- function(type) {
  ns <- NS(type)
  tagList(
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
  )
}

pcaServer <- function(data_ready) {
  moduleServer(NULL, function(input, output, session) {
    reactive({
      pca <- data_ready %>%
        select(c("var", "sample", "value")) %>%
        pivot_wider(names_from = "var", values_from = "value") %>%
        select(-1) %>%
        prcomp()

      pcaValues <- data_ready %>% select(-c("var", "value")) %>% distinct() %>% cbind(pca$x)

      percentage <- round(pca$sdev / sum(pca$sdev)*100, 2)
      percentage <- paste0(colnames(pca$x), " (", percentage, "%", ")") %>% as.list()
      pctg <- setNames(percentage, colnames(pca$x))

      pcaLoadings <- as.data.frame(pca$rotation) %>% tibble::rownames_to_column("var")


      p <- pcaValues %>%
        ggplot(aes(x = PC1, y = PC2, colour = group))

      p <- p + geom_point()

      if (input$point_shape == "圆点") {
        p <- p + geom_point(size = input$point_size)
      } else if (input$point_shape == "黑色边框圆点") {
        p <- p + geom_point(size = input$point_size, shape = 21, stroke = 1, colour = "black", aes(fill = group))
      } else if (input$point_shape == "各具形状") {
        p <- p + geom_point(size = input$point_size, aes(shape = group))
      }
      if (input$ellipse == TRUE) {
        p <- p + stat_ellipse(level = 0.95, show.legend = F, colour = "black", linewidth = 0.2, geom = "polygon", aes(fill = group), alpha = 0.2)
      }
      if (input$loadings == TRUE) {
        p <- p + geom_segment(data = pcaLoadings, aes(x = 0, y = 0, xend = (PC1*input$loadings_scale), yend = (PC2*input$loadings_scale)), arrow = arrow(length = unit(5, "mm")), colour = "black") +
          annotate("text", x = (pcaLoadings$PC1*input$loadings_scale), y = (pcaLoadings$PC2*input$loadings_scale), label = pcaLoadings$var)
      }
      return(p)
    })
  })
}