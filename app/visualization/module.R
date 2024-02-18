vsUI <- function(id) {
  ns <- NS(id)
  layout_sidebar(
    sidebar = sidebar(width = "35%", open = "always",
      actionButton(ns("nav"), "Click to switch"),
      friendlyAct(ns("go"), "Go"),
      lapply(vs_types, function(vs_type) {
        conditionalPanel(ns = ns, condition = paste0("input.current == '", vs_type, "'"), get(paste0(vs_type, "UI"))(ns(vs_type)))
      })
    ),
    layout_sidebar(
      sidebar = sidebar(position = "right",
        uiOutput(ns("readme_in_page"))
      ),
      lapply(vs_types, function(vs_type) {
        conditionalPanel(ns = ns, condition = paste0("input.current == '", vs_type, "'"), vsOutputUI(ns(vs_type)))
      })
    ),
    class = "p-0"
  )
}

vsOutputUI <- function(id) {
  ns <- NS(id)
  tagList(
    layout_columns(col_widths = c(1, 2, 2, -1, 6),
      input_switch(ns("plotly"), "Plotly"),
      numericInputIcon(ns("output_width"), label = NULL, value = 1200, min = 1, max = 3000, icon = list("宽", "px")),
      numericInputIcon(ns("output_height"), label = NULL, value = 800, min = 1, max = 1500, icon = list("高", "px")),
      conditionalPanel(ns = ns, condition = "input.plotly == false",
        layout_columns(col_widths = c(3, 6, 3),
          selectInput(ns("output_res"), label = NULL, choices = c(72, 300, 600), selected = 300),
          textOutput(ns("output_size")),
          dropMenu(
            actionButton(ns("output_ggsave"), "导出为", icon = icon("image")),
            placement = "bottom-end", arrow = FALSE,
            div(
              class = "dropMenu-column",
              downloadButton(ns("output_ggsave_png"), "PNG", icon = NULL),
              downloadButton(ns("output_ggsave_svg"), "SVG", icon = NULL)
            )
          )
        )
      )
    ),
    conditionalPanel(ns = ns, condition = "input.plotly == false", uiOutput(ns("plotoutputUI"))),
    conditionalPanel(ns = ns, condition = "input.plotly == true", uiOutput(ns("plotlyoutputUI")))
  )
}

vsServer <- function(id, rv) {
  moduleServer(id, function(input, output, session) {

    ns <- session$ns

    lapply(vs_types, function(vs_type) {
      vsOutputServer(vs_type, rv)
    })

    observeEvent(input$nav, {

      showModal(modalDialog(
        title = "Navigation",
        size = "xl",
        div(style = "display: flex; height: 75vh;",
          div(id = ns("nav-container"), class = "figure-container", style = "overflow-y: auto; min-width: 400px; margin-right: 10px;",
            p("图片测试用，名字是对的"),
            div(class = "figure-group",
              figure(ns = ns, "vs-types/bar.jpg", "柱状图"),
              figure(ns = ns, "vs-types/box.jpg", "箱线图"),
              figure(ns = ns, "vs-types/heatmap.jpg", "热图"),
              figure(ns = ns, "vs-types/line.jpg", "折线图"),
              figure(ns = ns, "vs-types/pca.jpg", "主成分分析"),
              figure(ns = ns, "vs-types/pie.jpg", "饼图"),
              figure(ns = ns, "vs-types/point.jpg", "散点图"),
              figure(ns = ns, "vs-types/radar.jpg", "雷达图"),
              figure(ns = ns, "vs-types/violin.jpg", "小提琴图"),
              figure(ns = ns, "vs-types/volcano.jpg", "火山图")
            ),
            p("分类2"),
            div(class = "figure-group",
              figure(ns = ns, "vs-types/widthTest.jpg", "宽度测试"),
              figure(ns = ns, "vs-types/heightTest.jpg", "高度测试")
            )
          ),
          uiOutput(ns("readme"), style = "min-width: 600px; overflow-y: auto;"),
          # 注意命名空间
          tags$script("
            // 保证readme的正确渲染
            Shiny.addCustomMessageHandler('navENSet', function(value) {
              Shiny.setInputValue('vs-navEN', value);
            });
            // 修改current
            Shiny.addCustomMessageHandler('currentSet', function(value) {
              Shiny.setInputValue('vs-current', value);
            });
          ")
        ),
        footer = tagList(
          actionButton(ns("back"), "Cancel"),
          actionButton(ns("switch"), "Switch", class = "btn-success")
        )
      ))

      # 再次打开时显示current
      addClass(id = input$current, class = 'selected')

      # 注意命名空间
      runjs("
        // 添加事件监听器到父容器
        const figureContainer = document.getElementById('vs-nav-container');
        figureContainer.addEventListener('click', function(event) {

          // 找到被点击的图片
          const clickedFigure = event.target.closest('.figure');

          // 如果点击的是图片，则将其添加 'selected' 类，并将 id 和 data-value 属性值存入 shiny 的 input
          if (clickedFigure) {
                      // 移除已选中的图片的 'selected' 类
          const selectedFigures = document.querySelectorAll('.figure.selected');
          selectedFigures.forEach(figure => figure.classList.remove('selected'));
            clickedFigure.classList.add('selected');
            const figureIdWithNamespace = clickedFigure.id;
            // 去除命名空间
            const figureIdWithoutNamespace = figureIdWithNamespace.replace('vs-', '');

            const figureValue = clickedFigure.dataset.value;

            Shiny.setInputValue('vs-navEN', figureIdWithoutNamespace);
            Shiny.setInputValue('vs-navZH', figureValue);
          }
        });
      ")
    })

    output$readme <- renderUI({
      req(input$navEN)
      includeMarkdown(paste0("visualization/types-readme/", input$navEN, ".md"))
    })

    observeEvent(input$back, {
      if (is.null(input$current)) {
        runjs("Shiny.setInputValue('vs-navEN', null)")
      } else {
        session$sendCustomMessage("navENSet", input$current)
      }
      removeModal()
    })

    observeEvent(input$switch, {
      session$sendCustomMessage("currentSet", input$navEN)  # 修改current
      updateActionButton(session, "nav", input$navZH)  # 修改按钮文字
      removeModal()
    })

    output$readme_in_page <- renderUI({
      req(input$current)
      includeMarkdown(paste0("visualization/types-readme/", input$current, ".md"))
    })

    friendlyActServer("go", {
      data_ready <- rv$pp$data_o_m_n %>% left_join(rv$pp$data_desc, by = "sample")
      rv$vs[[input$current]] <- get(paste0(input$current, "Server"))(input$current, data_ready)
    })

    # observe({
    #   toggle("control", condition = !is.null(rv$vs[[id]]))
    # })

  })
}

vsOutputServer <- function(id, rv) {
  moduleServer(id, function(input, output, session) {

    ns <- session$ns

    width <- reactive({
      input$output_width
    })
    height <- reactive({
      input$output_height
    })
    res <- reactive({
      as.numeric(input$output_res)
    })

    output$output_size <- renderText({
      paste0("实际大小: ", round(width() / res() * 25.4, 2), " x ", round(height() / res() * 25.4, 2), " mm")
    })

    output$output_ggsave_png <- downloadHandler(
      filename = function() {
        paste0(rv$pp$name, "_", id, ".png")
      },
      content = function(file) {
        ggsave(file, plot = rv$vs[[id]](), device = "png", width = width(), height = height(), unit = "px", dpi = res())
      }
    )

    output$output_ggsave_svg <- downloadHandler(
      filename = function() {
        paste0(rv$pp$name, "_", id, ".svg")
      },
      content = function(file) {
        ggsave(file, plot = rv$vs[[id]](), device = "svg", width = width(), height = height(), unit = "px", dpi = res())
      }
    )

    output$plotoutputUI <- renderUI({
      plotOutput(ns("plotoutput"), width = width(), height = height())
    })

    observeEvent(input$output_res, {
      output$plotoutput <- renderPlot({
        req(rv$vs[[id]])
        rv$vs[[id]]()
      }, res = res())
    })

    output$plotlyoutputUI <- renderUI({
      plotlyOutput(ns("plotlyoutput"))
    })

    output$plotlyoutput <- renderPlotly({
      req(rv$vs[[id]])
      ggplotly(rv$vs[[id]](), width = width(), height = height())
    })

  })
}