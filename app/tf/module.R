Lipidomics <- read_csv("tf/Lipidomics.csv")
Lipidomics_FSH <- read_csv("tf/Lipidomics_FSH.csv")

tfUI <- function(id) {
  ns <- NS(id)
  layout_sidebar(
    sidebar = sidebar(width = "25%", open = "always",
      fileInput(ns("file"), "上传文件", accept = ".csv"),
      selectInput(ns("stdmix"), "内标", choices = c("Lipidomics", "Lipidomics_FSH")),
      numericInput(ns("stdmix_num"), "份数", value = 1, min = 0.1, max = 2, step = 0.1, width = "80px"),
      tableOutput(ns("stdmix_check")),
      friendlyAct(ns("process"), "Process", icon = icon("play"))
    ),
    navset_tab(selected = "pmol",
      nav_panel("提取Compound, Filename, Area", DTOutput(ns("result1"))),
      nav_panel("不具有标准曲线的Compound的Area", DTOutput(ns("result2"))),
      nav_panel("剩下的用外标法转换后", DTOutput(ns("result3"))),
      nav_panel("检查ISTD", DTOutput(ns("result4"))),
      nav_panel("pmol", DTOutput(ns("result5"))),
      nav_spacer(),
      nav_item(
        dropMenu(
          actionButton(ns("result_download"), "Export the Result", icon = icon("file-export")),
          div(class = "dropMenu-column",
            downloadButton(ns("downloadBtn1"), "提取Compound, Filename, Area"),
            downloadButton(ns("downloadBtn2"), "不具有标准曲线的Compound的Area"),
            downloadButton(ns("downloadBtn3"), "剩下的用外标法转换后"),
            downloadButton(ns("downloadBtn4"), "检查ISTD"),
            downloadButton(ns("downloadBtn5"), "pmol")
          ),
          placement = "bottom-start", arrow = FALSE
        )
      )
    )
  )
}

tfServer <- function(id, rv) {
  moduleServer(id, function(input, output, session) {

    ns <- session$ns

    # 内标dropMenu
    output$stdmix_status <- renderUI({
      actionButton(ns("stdmix_status2"), label = paste0("内标：", input$stdmix, ", ", input$stdmix_num, "份"), icon = icon("vial"))
    })

    # 实际应用的内标
    stdmix_apply <- reactive({
      get(input$stdmix) %>% mutate(amount = amount * input$stdmix_num)
    })

    # 内标dropMenu内表格的渲染
    output$stdmix_check <- renderTable({
      stdmix_apply()
    })

    # process按钮的禁用与否
    observeEvent(input$file, {
      rv$tf$file <- input$file
    })

    observe({
      toggleState("process", !is.null(rv$tf$file))
    })

    # process
    friendlyActServer("process", {

      # 提取Compound, Filename, Area
      rv$tf$result1 <- rv$tf$file$datapath %>%
        read_csv(na = "N/F") %>%
        rename("var" = 1) %>%
        subset(subset = `Peak Label` == "T1", select = c("var", "Filename", "Area"))

      # 不具有标准曲线的Compound的Area
      rv$tf$result2 <- rv$tf$result1 %>%
        mutate(class = str_extract(var, "\\S+")) %>%
        anti_join(stdmix_apply(), by = "class") %>%
        select(-"class")

      # 剩下的用外标法转换后
      rv$tf$result3 <- rv$tf$result1 %>%
        mutate(class = str_extract(var, "\\S+")) %>%
        inner_join(stdmix_apply(), by = "class") %>%
        mutate(afterESpmol = 10^((log10(Area) - b) / a))

      # 检查ISTD
      rv$tf$result4 <- rv$tf$result3 %>% filter(grepl("ISTD", var))

      # 再用内标回复到总样品中的pmol
      rv$tf$result5 <- rv$tf$result3 %>%
        left_join(rv$tf$result4[c("Filename", "class", "afterESpmol")], by = c("Filename", "class")) %>%
        mutate(afterESafterISpmol = afterESpmol.x * amount / afterESpmol.y) %>%
        filter(!grepl("ISTD", var))

      # 把结果都转成宽数据
      rv$tf$result1 <- rv$tf$result1 %>% pivot_wider(names_from = "Filename", values_from = "Area")
      rv$tf$result2 <- rv$tf$result2 %>% pivot_wider(names_from = "Filename", values_from = "Area")
      rv$tf$result3 <- rv$tf$result3 %>%
        select(c("var", "Filename", "afterESpmol")) %>%
        pivot_wider(names_from = "Filename", values_from = "afterESpmol")
      rv$tf$result4 <- rv$tf$result4 %>%
        select(c("var", "Filename", "afterESpmol")) %>%
        pivot_wider(names_from = "Filename", values_from = "afterESpmol")
      rv$tf$result5 <- rv$tf$result5 %>%
        select(c("var", "Filename", "afterESafterISpmol")) %>%
        pivot_wider(names_from = "Filename", values_from = "afterESafterISpmol")
    })

    # 渲染表格
    results <- c("result1", "result2", "result3", "result4", "result5")
    lapply(results, function(result) {
      output[[result]] <- renderDT({
        req(rv$tf[[result]])
        datatable(
          rv$tf[[result]],
          extensions = "FixedColumns",
          options = list(
            dom = "ifrt",
            pageLength = -1,
            scrollX = TRUE,
            scrollY = "630px",
            fixedColumns = list(leftColumns = 2)
          )
        ) %>% formatRound(columns = colnames(rv$tf[[result]])[-1], digits = 0)
      })
    })

    # 下载按钮的禁用与否
    observe({
      toggleState("result_download", !is.null(rv$tf$result5))
    })

    # 文件后缀
    file_suffixes <- c(
      "提取Compound, Filename, Area",
      "不具有标准曲线的Compound的Area",
      "剩下的用外标法转换后",
      "检查ISTD",
      "pmol"
    )

    # 下载
    lapply(1:5, function(i) {
      output[[paste0("downloadBtn", i)]] <- downloadHandler(
        filename = function() {
          paste0(str_extract(input$file$name, ".*(?=\\.)"), "_", file_suffixes[i], ".csv")
        },
        content = function(file) {
          write_csv(rv$tf[[paste0("result", i)]], file, na = "")
        }
      )
    })

  })
}
