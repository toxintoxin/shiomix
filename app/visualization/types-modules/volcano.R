volcanoUI <- function(type) {
  ns <- NS(type)
  tagList(
    sliderInput(ns("pthreshold"), "pthreshold", min = 0.01, max = 0.05, value = 0.05, step = 0.01),
    sliderInput(ns("fcthreshold"), "fcthreshold", min = 1, max = 8, value = 2, step = 1),
    materialSwitch(
      inputId = ns("assist_lines"),
      label = "辅助线",
      value = FALSE,
      status = "success"
    ),
    conditionalPanel(
      ns = ns,
      condition = "input.assist_lines",
      div(
        pickerInput(
          ns("hline_linetype"),
          "hline类型",
          choices = c("solid", "dashed", "dotted", "dotdash", "longdash", "twodash")
        ),
        colorPickr(ns("hline_colour"), "hline颜色", selected = "#000000")
      ),
      div(
        pickerInput(
          ns("vline_linetype"),
          "vline类型",
          choices = c("solid", "dashed", "dotted", "dotdash", "longdash", "twodash")
        ),
        colorPickr(ns("vline_colour"), "vline颜色", selected = "#000000")
      )
    ),
    div(
      colorPickr(ns("point_up"), "UP颜色", selected = "#F8766D"),
      colorPickr(ns("point_down"), "DOWN颜色", selected = "#00BA38"),
      colorPickr(ns("point_ns"), "NS颜色", selected = "#BEBEBE")
    ),
    div(style = "display:inline-block",
      sliderInput(ns("point_size"), "点大小", min = 1, max = 5, value = 2, step = 0.5),
      sliderInput(ns("point_alpha"), "点透明度", min = 0.1, max = 1, value = 1, step = 0.1)
    )
  )
}

volcanoServer <- function(data_ready) {
  moduleServer(NULL, function(input, output, session) {
    reactive({
      pthreshold <- input$pthreshold
      fcthreshold <- input$fcthreshold

      # 应该先丢弃整行一样的，如果前面数据处理方式不对，可能出现这种情况
      data <- data_ready  # ttest

      data$log2FC <- log2(data$FC)
      data$change <- factor(ifelse(data$p.ajusted<pthreshold & abs(data$log2FC)>log2(fcthreshold),
                                  ifelse(data$log2FC>log2(fcthreshold),"Up","Down"),"None"), levels = c("Up", "None", "Down"))

      p <- data %>%
        ggplot(aes(x = log2FC, y = -log10(p.ajusted), colour = change)) +
        geom_point(alpha = input$point_alpha, size = input$point_size) +
        scale_colour_manual(values = c(input$point_up, input$point_ns, input$point_down))


      if (input$assist_lines) {
        p <- p +
          geom_hline(yintercept = -log10(pthreshold), linetype = input$hline_linetype, colour = input$hline_colour) +
          geom_vline(xintercept = c(-fcthreshold, fcthreshold), linetype = input$vline_linetype, colour = input$vline_colour)
      }

      return(p)
    })
  })
}