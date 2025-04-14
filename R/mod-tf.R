#' @import bslib
#' @import DT
#' @import readr
#' @import shinyWidgets
#' @import dplyr
#' @import tidyr
#' @import stringr

tf_ui <- function(id) {
  ns <- NS(id)
  layout_sidebar(
    sidebar = sidebar(width = "25%", open = "always",
      excelInput(ns("data"), header = "Data"),
      selectInput(ns("stdmix"), "内标", choices = stdmix_files_names),
      numericInput(ns("stdmix_num"), "份数", value = 1, min = 0.1, max = 2, step = 0.1, width = "80px"),
      tableOutput(ns("stdmix_check")),
      actionButton(ns("process"), "Process", icon = icon("play"))
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

tf_server <- function(id) {
  moduleServer(id, function(input, output, session) {

    ns <- session$ns

    rv <- reactiveValues()

    data_ls <- excel_server("data", na = "N/F")

    observe({
      rv$data_original <- data_ls$data()
      rv$name <- data_ls$name()
    })

    # 内标dropMenu
    output$stdmix_status <- renderUI({
      actionButton(ns("stdmix_status2"), label = paste0("内标：", input$stdmix, ", ", input$stdmix_num, "份"), icon = icon("vial"))
    })

    # 实际应用的内标
    stdmix_apply <- reactive({
      stdmix_list[[input$stdmix]] %>% mutate(amount = amount * input$stdmix_num)
    })

    # 内标dropMenu内表格的渲染
    output$stdmix_check <- renderTable({
      stdmix_apply()
    })

    # process按钮的禁用与否
    observe({
      shinyjs::toggleState("process", condition = !is.null(rv$data_original))
    })

    # process
    observeEvent(input$process, {

      # 定义如何应用内标
      std_ref <- stdmix_apply() %>%
        pivot_longer(-c(1:4), values_to = "apply") %>%
        drop_na("apply") %>%
        select(-"name")

      # 提取Compound, Filename, Area
      rv$result1 <- rv$data_original %>%
        rename("var" = 1) %>%
        subset(subset = `Peak Label` == "T1", select = c("var", "Filename", "Area"))

      # 不具有标准曲线的Compound的Area
      rv$result2 <- rv$result1 %>%
        mutate(class = str_extract(var, "\\S+")) %>%
        anti_join(std_ref, by = c("class" = "apply")) %>%
        select(-"class")

      # 剩下的用外标法转换后
      rv$result3 <- rv$result1 %>%
        mutate(class = str_extract(var, "\\S+")) %>%
        inner_join(std_ref, by = c("class" = "apply")) %>%
        mutate(pmol_ES = 10^((log10(Area) - b) / a))

      # 检查ISTD
      rv$result4 <- rv$result3 %>% filter(grepl("ISTD", var))

      # 再用内标回复到总样品中的pmol
      rv$result5 <- rv$result3 %>%
        left_join(rv$result4[c("Filename", "compound", "pmol_ES")], by = c("Filename", "compound")) %>%
        mutate(pmol_ESIS = pmol_ES.x * amount / pmol_ES.y) %>%
        filter(!grepl("ISTD", var))

      # 把结果都转成宽数据
      rv$result1 <- rv$result1 %>% pivot_wider(names_from = "Filename", values_from = "Area")
      rv$result2 <- rv$result2 %>% pivot_wider(names_from = "Filename", values_from = "Area")
      rv$result3 <- rv$result3 %>%
        select(c("var", "Filename", "pmol_ES")) %>%
        pivot_wider(names_from = "Filename", values_from = "pmol_ES")
      rv$result4 <- rv$result4 %>%
        select(c("var", "Filename", "pmol_ES")) %>%
        pivot_wider(names_from = "Filename", values_from = "pmol_ES")
      rv$result5 <- rv$result5 %>%
        select(c("var", "Filename", "pmol_ESIS")) %>%
        pivot_wider(names_from = "Filename", values_from = "pmol_ESIS")
    })

    # 渲染表格
    results <- c("result1", "result2", "result3", "result4", "result5")
    lapply(results, function(result) {
      output[[result]] <- renderDT({
        req(rv[[result]])
        datatable(
          rv[[result]],
          extensions = "FixedColumns",
          options = list(
            dom = "ifrt",
            pageLength = -1,
            scrollX = TRUE,
            scrollY = "630px",
            fixedColumns = list(leftColumns = 2)
          )
        ) %>% formatRound(columns = colnames(rv[[result]])[-1], digits = 0)
      })
    })

    # 下载按钮的禁用与否
    observe({
      shinyjs::toggleState("result_download", !is.null(rv$result5))
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
          paste0(rv$name, "_", file_suffixes[i], ".csv")
        },
        content = function(file) {
          write_csv(rv[[paste0("result", i)]], file, na = "")
        }
      )
    })

  })
}
