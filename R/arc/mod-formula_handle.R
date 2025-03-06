formula_handleUI <- function(id) {
  ns <- NS(id)
  tagList(
    layout_columns(col_widths = c(5, 2, 2, 3),
      fileInput(ns("formula_handle_file"), "上传化合物列表", accept = ".xlsx"),
      actionButton(ns("formula_handle_filter"), "添加筛选条件"),
      downloadButton(ns("formula_handle_template"), "下载模板"),
      downloadButton(ns("formula_handle_download"), "下载结果", icon = icon("file-export"))
    ),
    DTOutput(ns("formula_handle_list"))
  )
}

formula_handleServer <- function(id) {
  moduleServer(id, function(input, output, session) {

    ns <- session

    # 提供formula_handle模板
    output$formula_handle_template <- downloadHandler(
      filename = "formula handle template.xlsx",
      content = function(file) {
        formula_handle_template <- data.frame(
          compound = c("not necessary"),
          formula = c(NA),
          stringsAsFactors = FALSE
        )
        write.xlsx(formula_handle_template, file)
      }
    )




    # 添加筛选条件






    formula_handle <- reactive({
      req(input$formula_handle_file)
      formula_handle <- read_xlsx(input$formula_handle_file$datapath, col_names = TRUE)
      # 检查化学式再合并
      formula_handle <- cbind(formula_handle, check_chemform(isotopes, formula_handle$formula)) %>%
        select(c("compound", "formula", "warning", "new_formula"), everything())
      # adduct不在列表中的，warning为TRUE
      #  formula_handle$warning <- ifelse(!formula_handle$adduct %in% adducts$Name, TRUE, formula_handle$warning)
      return(formula_handle)
    })

    output$formula_handle_list <- renderDT({
      datatable(formula_handle())
    })

  })
}