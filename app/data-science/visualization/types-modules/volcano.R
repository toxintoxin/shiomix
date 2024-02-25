volcanoUI <- function(id) {
  ns <- NS(id)
  navset_pill_list(
    nav_panel("Upload",
      excelInput(ns("data"), header = "Data"),
      actionButton(ns("data_handle"), "Handle")
    ),
    nav_panel("p1_generate",
      sliderInput(ns("pthreshold"), "pthreshold", min = 0.01, max = 0.05, value = 0.05, step = 0.01),
      sliderInput(ns("fcthreshold"), "fcthreshold", min = 1, max = 8, value = 2, step = 1),
      input_switch(ns("assist_lines"), label = "辅助线", value = FALSE),
      conditionalPanel(
        ns = ns,
        condition = "input.assist_lines",
        div(
          selectInput(
            ns("hline_linetype"),
            "hline类型",
            choices = c("solid", "dashed", "dotted", "dotdash", "longdash", "twodash")
          ),
          colorPickr(ns("hline_colour"), "hline颜色", selected = "#000000")
        ),
        div(
          selectInput(
            ns("vline_linetype"),
            "vline类型",
            choices = c("solid", "dashed", "dotted", "dotdash", "longdash", "twodash")
          ),
          colorPickr(ns("vline_colour"), "vline颜色", selected = "#000000")
        )
      )
    ),
    nav_panel("p2_generate",
      div(
        colorPickr(ns("point_up"), "UP颜色", selected = "#F8766D"),
        colorPickr(ns("point_down"), "DOWN颜色", selected = "#00BA38"),
        colorPickr(ns("point_ns"), "NS颜色", selected = "#BEBEBE")
      )
    ),
    nav_panel("pq_generate",
      div(style = "display:inline-block",
        sliderInput(ns("point_size"), "点大小", min = 1, max = 5, value = 2, step = 0.5),
        sliderInput(ns("point_alpha"), "点透明度", min = 0.1, max = 1, value = 1, step = 0.1)
      )
    ),
    nav_panel("Labels",
      gglabsUI(ns(NULL))
    ),
    nav_panel("Theme",
      ggthemeUI(ns(NULL))
    )
  )
}

volcanoServer <- function(id) {
  moduleServer(id, function(input, output, session) {

    ns <- session$ns

    rv <- reactiveValues()

    rv$plot_labeled <- gglabsServer(NULL, rv$plot_init)
    rv$plot_final <- ggthemeServer(NULL, rv$plot_labeled)
    vsUniversalServer(NULL, rv$plot_final, rv$name, id)

    data_ls <- excelServer("data")

    observe({
      rv$data_original <- data_ls$data()
      rv$name <- data_ls$name()
    })

    observeEvent(input$data_handle, {
      rv$data <- rv$data_original
      rv$data$log2FC <- log2(rv$data$FC)
    })

    rv$plot_init <- reactive({
      pthreshold <- input$pthreshold
      fcthreshold <- input$fcthreshold
      rv$data$change <- factor(ifelse(rv$data$p.ajusted<pthreshold & abs(rv$data$log2FC)>log2(fcthreshold),
                              ifelse(rv$data$log2FC>log2(fcthreshold),"Up","Down"),"None"), levels = c("Up", "None", "Down"))
      p <- rv$data %>%
        ggplot(aes(x = log2FC, y = -log10(p.ajusted), colour = change)) +
        geom_point(alpha = input$point_alpha, size = input$point_size) +
        scale_colour_manual(values = c(input$point_up, input$point_ns, input$point_down))

      if (input$assist_lines) {
        p <- p +
          geom_hline(yintercept = -log10(pthreshold), linetype = input$hline_linetype, colour = input$hline_colour) +
          geom_vline(xintercept = c(-log2(fcthreshold), log2(fcthreshold)), linetype = input$vline_linetype, colour = input$vline_colour)
      }

      return(p)
    })





  })
}