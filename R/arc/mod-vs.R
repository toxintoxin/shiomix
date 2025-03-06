#' @import shiny
#' 

vsUI <- function(id) {
  ns <- NS(id)
  # layout_sidebar(height = "700px", border = FALSE, class = "p-0",
  #   sidebar = sidebar(width = "200px",
  #     actionButton(ns("nav_fullscreen"), "切换视图进行导航 (still building)"),
  #     radioButtons(ns("nav"), label = "Visualization types",
  #       choiceNames = c("柱状图", "箱线图", "热图", "折线图", "主成分分析", "饼图","散点图", "雷达图", "小提琴图", "火山图", "南丁格尔玫瑰图", "韦恩图"),
  #       choiceValues = c("bar", "box", "heatmap", "line", "pca", "pie", "point", "radar", "violin", "volcano", "nightingale", "venn")
  #     )
  #   )
  # )
}

vsServer <- function(id) {
  moduleServer(id, function(input, output, session) {

    ns <- session$ns

    # lapply(vs_types, function(vs_type) {
    #   get(paste0(vs_type, "Server"))(vs_type)
    # })

    # observeEvent(input$nav, {

    #   showModal(modalDialog(
    #     title = "Navigation",
    #     size = "xl",
    #     div(style = "display: flex; height: 75vh;",
    #       div(id = ns("nav-container"), class = "figure-container", style = "overflow-y: auto; min-width: 400px; margin-right: 10px;",
    #         p("图片测试用，名字是对的"),
    #         # 添加新的画图module的时候在这里添加
    #         div(class = "figure-group",
    #           figure(ns = ns, "vs-types/bar.png", "柱状图"),
    #           figure(ns = ns, "vs-types/box.png", "箱线图"),
    #           figure(ns = ns, "vs-types/heatmap.png", "热图"),
    #           figure(ns = ns, "vs-types/line.png", "折线图"),
    #           figure(ns = ns, "vs-types/pca.png", "主成分分析"),
    #           figure(ns = ns, "vs-types/pie.png", "饼图"),
    #           figure(ns = ns, "vs-types/point.png", "散点图"),
    #           figure(ns = ns, "vs-types/radar.png", "雷达图"),
    #           figure(ns = ns, "vs-types/violin.png", "小提琴图"),
    #           figure(ns = ns, "vs-types/volcano.png", "火山图"),
    #           figure(ns = ns, "vs-types/nightingale.png", "南丁格尔玫瑰图"),
    #           figure(ns = ns, "vs-types/venn.png", "韦恩图"),
    #         ),
    #         p("分类2"),
    #         div(class = "figure-group",
    #           figure(ns = ns, "vs-types/widthTest.png", "宽度测试"),
    #           figure(ns = ns, "vs-types/heightTest.png", "高度测试")
    #         )
    #       ),
    #       uiOutput(ns("readme"), style = "min-width: 600px; overflow-y: auto;"),
    #       # 注意命名空间
    #       tags$script("
    #         // 保证readme的正确渲染
    #         Shiny.addCustomMessageHandler('navENSet', function(value) {
    #           Shiny.setInputValue('vs-navEN', value);
    #         });
    #         // 修改current
    #         Shiny.addCustomMessageHandler('currentSet', function(value) {
    #           Shiny.setInputValue('vs-current', value);
    #         });
    #       ")
    #     ),
    #     footer = tagList(
    #       actionButton(ns("back"), "Cancel"),
    #       actionButton(ns("switch"), "Switch", class = "btn-success")
    #     )
    #   ))

    #   # 再次打开时显示current
    #   addClass(id = input$current, class = 'selected')

    #   # 注意命名空间
    #   runjs("
    #     // 添加事件监听器到父容器
    #     const figureContainer = document.getElementById('vs-nav-container');
    #     figureContainer.addEventListener('click', function(event) {

    #       // 找到被点击的图片
    #       const clickedFigure = event.target.closest('.figure');

    #       // 如果点击的是图片，则将其添加 'selected' 类，并将 id 和 data-value 属性值存入 shiny 的 input
    #       if (clickedFigure) {
    #                   // 移除已选中的图片的 'selected' 类
    #       const selectedFigures = document.querySelectorAll('.figure.selected');
    #       selectedFigures.forEach(figure => figure.classList.remove('selected'));
    #         clickedFigure.classList.add('selected');
    #         const figureIdWithNamespace = clickedFigure.id;
    #         // 去除命名空间
    #         const figureIdWithoutNamespace = figureIdWithNamespace.replace('vs-', '');

    #         const figureValue = clickedFigure.dataset.value;

    #         Shiny.setInputValue('vs-navEN', figureIdWithoutNamespace);
    #         Shiny.setInputValue('vs-navZH', figureValue);
    #       }
    #     });
    #   ")
    # })

    # output$readme <- renderUI({
    #   req(input$navEN)
    #   includeMarkdown(paste0("visualization/types-readme/", input$navEN, ".md"))
    # })

    # observeEvent(input$back, {
    #   if (is.null(input$current)) {
    #     runjs("Shiny.setInputValue('vs-navEN', null)")
    #   } else {
    #     session$sendCustomMessage("navENSet", input$current)
    #   }
    #   removeModal()
    # })

    # observeEvent(input$switch, {
    #   session$sendCustomMessage("currentSet", input$navEN)  # 修改current
    #   updateActionButton(session, "nav", input$navZH)  # 修改显示给用户的文字
    #   removeModal()
    # })

  })
}

vsUniversalUI <- function(id) {
  ns <- NS(id)
  tagList(
    div(style = "display: flex; gap: 20px;",
      actionButton(ns("apply"), label = "Apply & Refresh", width = "200px"),
      numericInputIcon(ns("output_width"), label = NULL, width = "200px", min = 1, max = 30, value = 4, icon = list("Width", "cm")),
      numericInputIcon(ns("output_height"), label = NULL, width = "200px", min = 1, max = 30, value = 4, icon = list("Height", "cm")),
      dropMenu(
        actionButton(ns("output_ggsave"), "导出为", icon = icon("image"), style = "margin-left: auto;"),
        placement = "bottom-end", arrow = FALSE,
        div(
          class = "dropMenu-column",
          downloadButton(ns("output_ggsave_png_72"), label = "PNG (dpi = 72)", icon = NULL),
          downloadButton(ns("output_ggsave_png_300"), label = "PNG (dpi = 300)", icon = NULL),
          downloadButton(ns("output_ggsave_svg"), label = "SVG", icon = NULL)
        )
      )
    ),





      # input_switch(ns("plotly"), label = "Building Plotly"),

      # conditionalPanel(ns = ns, condition = "input.plotly == false",
          # selectInput(ns("output_res"), label = NULL, choices = c(72, 300, 600), selected = 72),
          # textOutput(ns("output_size")),

      # )

    uiOutput(ns("plotoutput_ui")),
    # conditionalPanel(ns = ns, condition = "input.plotly == true", plotlyOutput(ns("plotly"), width = 600, height = 400))
  )
}



vsUniversalServer <- function(id, suffix, rv) {
  moduleServer(id, function(input, output, session) {

    ns <- session$ns

    # width <- reactive({
    #   input$output_width
    # })
    # height <- reactive({
    #   input$output_height
    # })
    # res <- reactive({
    #   as.numeric(input$output_res)
    # })

    output$output_size <- renderText({
      paste0("实际大小: ", round(input$output_width, 3), " x ", round(input$output_height, 3), " cm")
    })

    observe({
      toggleState("output_ggsave", condition = !is.null(rv$plot_final))
    })

    output$output_ggsave_png_72 <- downloadHandler(
      filename = function() {
        paste0(rv$name, "_", suffix, ".png")
      },
      content = function(file) {
        ggsave(file = file, plot = rv$plot_final, device = "png", width = input$output_width, height = input$output_height, unit = "cm", dpi = 72)
      }
    )

    output$output_ggsave_png_300 <- downloadHandler(
      filename = function() {
        paste0(rv$name, "_", suffix, ".png")
      },
      content = function(file) {
        ggsave(file = file, plot = rv$plot_final, device = "png", width = input$output_width, height = input$output_height, unit = "cm", dpi = 300)
      }
    )

    output$output_ggsave_svg <- downloadHandler(
      filename = function() {
        paste0(rv$name, "_", suffix, ".svg")
      },
      content = function(file) {
        ggsave(file = file, plot = rv$plot_final, device = "svg", width = input$output_width, height = input$output_height, unit = "cm")
      }
    )

    output$plotoutput_ui <- renderUI({
      plotOutput(ns("plot"), width = input$output_width/2.54*96, height = input$output_height/2.54*96)
    })
    # shiny render and save solution
    # https://www.tidyverse.org/blog/2020/08/taking-control-of-plot-scaling/
    # observeEvent(input$output_res, {
      output$plot <- renderPlot(
        # width = function() input$output_width/2.54*96,
        # height = function() input$output_height/2.54*96,
        res = 96,  # default paramter is 72, R is 96
        {
          req(rv$plot_final)
          rv$plot_final
        }
      )
    # })

    # output$plotlyoutput_ui <- renderUI({
    #   plotlyOutput(ns("plotlyoutput"))
    # })

    # output$plotlyoutput <- renderPlotly({
    #   req(plot_final)
    #   ggplotly(plot_final, width = width(), height = height())
    # })

  })
}