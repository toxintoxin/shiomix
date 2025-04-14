#' @import shiny

volcano_ui <- function(id) {
  ns <- NS(id)
  card(
    card_header(
      class = "d-flex justify-content-between",
      "火山图",
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
            sliderInput(ns("pthreshold"),  label = "pthreshold", min = 0.01, max = 0.05, value = 0.05, step = 0.01),
            sliderInput(ns("fcthreshold"),  label = "fcthreshold", min = 1, max = 8, value = 2, step = 1),
            input_switch(ns("assist_lines"), label = "辅助线", value = FALSE),
            conditionalPanel(
              ns = ns,
              condition = "input.assist_lines",
              layout_columns(col_widths = c(6, 6),
                selectInput(ns("hline_linetype"), label = "hline类型", choices = c("solid", "dashed", "dotted", "dotdash", "longdash", "twodash")),
                colorPickr(ns("hline_colour"),  label = "hline颜色", selected = "#000000")
              ),
              layout_columns(col_widths = c(6, 6),
                selectInput(ns("vline_linetype"), label = "vline类型", choices = c("solid", "dashed", "dotted", "dotdash", "longdash", "twodash")),
                colorPickr(ns("vline_colour"), "vline颜色", selected = "#000000")
              )
            ),
            layout_columns(col_widths = c(4, 4, 4),
              colorPickr(ns("point_up"), label = "UP颜色", selected = "#F8766D"),
              colorPickr(ns("point_down"), label = "DOWN颜色", selected = "#00BA38"),
              colorPickr(ns("point_ns"), label = "NS颜色", selected = "#BEBEBE")
            ),
            layout_columns(col_widths = c(6, 6),
              sliderInput(ns("point_size"), label = "点大小", min = 1, max = 5, value = 2, step = 0.5),
              sliderInput(ns("point_alpha"), label = "点透明度", min = 0.1, max = 1, value = 1, step = 0.1)
            )
          ),
          nav_panel("Labels", gglabs_ui(ns(NULL))),
          nav_panel("Theme", ggtheme_ui(ns(NULL)))
        )
      ),
      layout_sidebar(border = FALSE, style = "background: #eeeddd",
        sidebar = sidebar(width = "300px", position = "right",
          includeMarkdown(paste0("data-science/visualization/types-readme/", id, ".md"))
        ),
        vsUniversal_ui(ns(NULL))
      )
    )
  )
}

volcano_server <- function(id) {
  moduleServer(id, function(input, output, session) {

    ns <- session$ns

    rv <- reactiveValues()

    data_ls <- excel_server("data")

    observe({
      rv$data_original <- data_ls$data()
      rv$name <- data_ls$name()
    })

    observeEvent(input$data_handle, {
      rv$data <- rv$data_original
      rv$data$log2FC <- log2(rv$data$FC)
    })

    plot_interactive <- function() {

      pthreshold <- input$pthreshold
      fcthreshold <- input$fcthreshold

      rv$data <- rv$data %>%
        mutate(
          change = case_when(
            p.value < pthreshold & abs(log2FC) > log2(fcthreshold) ~ ifelse(log2FC > log2(fcthreshold), "Up", "Down"),
            TRUE ~ "None"
          )
        )

      rv$data$change <- factor(rv$data$change, levels = c("Up", "None", "Down"))

      rv$data$label <- ifelse(rv$data$change %in% c("Up", "Down"), rv$data$var, NA)

      p <- rv$data %>%
        ggplot(aes(x = log2FC, y = -log10(p.value), colour = change)) +
        geom_point(alpha = input$point_alpha, size = input$point_size) +
        scale_colour_manual(values = c("Up" = input$point_up, "None" = input$point_ns, "Down" = input$point_down))

      if (input$assist_lines) {
        p <- p +
          geom_hline(yintercept = -log10(pthreshold), linetype = input$hline_linetype, colour = input$hline_colour) +
          geom_vline(xintercept = c(-log2(fcthreshold), log2(fcthreshold)), linetype = input$vline_linetype, colour = input$vline_colour)
      }

      return(p)
    }

    observeEvent(input$apply, {
      rv$plot_init <- plot_interactive()
      rv$plot_labeled <- gglabs_server(NULL, rv$plot_init)
      rv$plot_final <- ggtheme_server(NULL, rv$plot_labeled)
    })
    vsUniversal_server(NULL, id, rv)

  })
}