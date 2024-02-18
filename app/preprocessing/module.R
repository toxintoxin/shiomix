ppUI <- function(id) {
  ns <- NS(id)
  layout_sidebar(
    sidebar = sidebar(width = "25%", open = "always",
      accordion(id = ns("sp"), multiple = FALSE,
        accordion_panel("Upload",
          fileInput(ns("data"), label = ".csv or .xlsx", accept = c(".csv", ".xlsx"), multiple = FALSE),
          uiOutput(ns("data_sheetUI")),
          radioButtons(ns("data_format"), label = NULL, choices = c("一般形式", "长数据"), inline = TRUE),
          friendlyAct(ns("data_read"), "读取")
        ),
        accordion_panel("Description",
          radioButtons(ns("desc_source"), label = NULL, choices = c("从命名读取", "从本地导入"), inline = TRUE),
          conditionalPanel(ns = ns, condition = "input.desc_source == '从命名读取'", helpText("以最后一个下划线为分隔符，前为描述，后为计数")),
          conditionalPanel(ns = ns, condition = "input.desc_source == '从本地导入'", excelInput(ns("desc"))),
          actionButton(ns("desc_read"), "读取")
        ),
        accordion_panel("Valuation 1",
          "Valuation 1"
        ),
        accordion_panel("Outlier",
          h4("Step1: "),
          actionButton(ns("outlier_remove_data_var_all_na"), "移去在所有样本中均为NA的变量"),
          br(),
          textOutput(ns("outlier_remove_data_var_all_na_tip"))
        ),
        accordion_panel("Valuation 2",
          "Valuation 2"
        ),
        accordion_panel("Missing value",
          selectInput(ns("missing_value_method"), label = "缺失值处理方法", choices = c("None", "同MetaboAnalyst，先删除只有一个样本有值的变量，NA用该变量在所有样本中的最小值的1/5代替", "NA用该变量在所有样本中的最小值的1/5代替", "NA用该样本的组内的最小值的1/5代替，若该组全为NA，全变成0", "先检查该组中NA的比例，若NA的比例小于等于70%，该组中所有NA用该变量在所在组中的最小值的1/5代替，否则该变量在该组所有样本都变为0")),
          conditionalPanel(ns = ns, condition = "input.missing_value_method == 'NA用该样本的组内的最小值的1/5代替，若该组全为NA，全变成0' || input.missing_value_method == '先检查该组中NA的比例，若NA的比例小于等于70%，该组中所有NA用该变量在所在组中的最小值的1/5代替，否则该变量在该组所有样本都变为0'",
            selectInput(ns("missing_value_group"), "分组依据", choices = c(Choose = ""))
          ),
          conditionalPanel(ns = ns, condition = "input.missing_value_method != 'None'",
            actionButton(ns("missing_value_handle"), "执行")
          )
        ),
        accordion_panel("Normalization",
          selectInput(ns("normalization_select"), label = NULL, width = "100%", choices = c("None", "Sample Specific", "Normalization by sum", "Normalization by median", "Normalization by a reference var", "Normalization by a reference sample (PQN)", "Normalization by a pooled sample from group (group PQN)")),
          conditionalPanel(ns = ns, condition = "input.normalization_select == 'Sample Specific'",
            excelInput(ns("normalization_factor")),
            actionButton(ns("normalization_factor_read"), "读取"),
            DTOutput(ns("normalization_factor_DT"))
          ),
          conditionalPanel(ns = ns, condition = "input.normalization_select == 'Normalization by a reference var' || input.normalization_select == 'Normalization by a reference sample (PQN)' || input.normalization_select == 'Normalization by a pooled sample from group (group PQN)'",
            selectInput(ns("normalization_specify"), label = NULL, choices = c(Choose = ""))
          ),
          conditionalPanel(ns = ns, condition = "input.normalization_select != 'None'",
            actionButton(ns("normalization_submit"), "执行")
          )
        )
      )
    ),
    navset_tab(id = ns("mp"),
      nav_panel("Upload", DTOutput(ns("data"))),
      nav_panel("Description", DTOutput(ns("data_desc"))),
      nav_panel("Valuation 1", h1("multiple plots")),
      nav_panel("Outlier", DTOutput(ns("data_o"))),
      nav_panel("Valuation 2", h1("multiple plots")),
      nav_panel("Missing value", DTOutput(ns("data_o_m"))),
      nav_panel("Normalization", DTOutput(ns("data_o_m_n"))),
      nav_spacer(),
      nav_item(downloadButton(ns("export"), "导出xlsx"))
    )
  )


}

ppServer <- function(id, rv) {
  moduleServer(id, function(input, output, session) {

    ns <- session$ns

    observeEvent(input$sp, {
      nav_select("mp", input$sp)
    })
    observeEvent(input$mp, {
      accordion_panel_open("sp", input$mp)
    })

    # Upload_sidebarPanel
    output$data_sheetUI <- renderUI({
      req(input$data)
      ext <- tools::file_ext(input$data$name)
      if (ext == "xlsx") {
        selectInput(ns("sheet"), "Sheet", choices = excel_sheets(input$data$datapath))
      }
    })
    observeEvent(input$data, {
      rv$pp$file <- input$data
    })
    observe({
      toggleState("data_read", !is.null(rv$pp$file) && !is.null(input$data_format))
    })

    friendlyActServer("data_read", {

      rv$pp$name <- read_excel_name(rv$pp$file, input$data_sheet)
      rv$pp$data <- read_excel_data(rv$pp$file, input$data_sheet)

      if (input$data_format == "一般形式") {
        rv$pp$data <- rv$pp$data %>% rename("var" = 1) %>% pivot_longer(-1, names_to = "sample", values_to = "value") %>% as.data.frame()
      } else if (input$data_format == "长数据") {
        rv$pp$data <- rv$pp$data %>% as.data.frame()
      }

      # 让所有值一样
      rv$pp$data_o <- rv$pp$data
      rv$pp$data_o_m <- rv$pp$data
      rv$pp$data_o_m_n <- rv$pp$data

      # 隐藏控件，显示文件信息

    })

    # Description_sidebarPanel
    excelInputServer("desc", "1")
    observeEvent(input$desc_read, {
      if (input$desc_source == "从命名读取") {
        rv$pp$data_desc <- rv$pp$data %>%
          mutate(group = str_extract(sample, ".*(?=_.*)")) %>%
          select(c("sample", "group")) %>%
          distinct() # 去除重复行记录
      } else if (input$desc_source == "从本地导入") {
        rv$pp$data_desc <- excelInputServer("desc", "data")
      }
      updateSelectInput(session, "missing_value_group", choices = colnames(rv$pp$data_desc)[-1])
    })

    # Valuation 1_sidebarPanel



    # Outlier_sidebarPanel
    observeEvent(input$outlier_remove_data_var_all_na, {
      rv$pp$data_var_all_na <- rv$pp$data %>%
        group_by(var) %>%
        filter(all(is.na(value))) %>%
        ungroup()
      rv$pp$data_o <- rv$pp$data %>%
        group_by(var) %>%
        filter(any(!is.na(value))) %>%
        ungroup()
      rv$pp$data_o_m <- rv$pp$data_o
      rv$pp$data_o_m_n <- rv$pp$data_o
    })

    output$outlier_remove_data_var_all_na_tip <- renderText({
      req(rv$pp$data_var_all_na)
      count <- rv$pp$data_var_all_na %>%
        select("var") %>%
        distinct() %>%
        nrow()
      paste0("已经删除了", count, "个变量，他们在所有样本中均为NA")
    })

    # Valuation 2_sidebarPanel



    # Missing value_sidebarPanel
    observeEvent(input$missing_value_method, {
      if (input$missing_value_method == "None") {
        rv$pp$data_o_m <- rv$pp$data_o
        rv$pp$data_o_m_n <- rv$pp$data_o
      }
    })

    observeEvent(input$missing_value_handle, {
      method <- input$missing_value_method
      df <- rv$pp$data_o %>%
        left_join(rv$pp$data_desc, by = "sample") %>%
        rename("group" = input$missing_value_group)
      if (method == "同MetaboAnalyst，先删除只有一个样本有值的变量，NA用该变量在所有样本中的最小值的1/5代替") {
        df <- df %>%
          group_by(var) %>%
          filter(sum(!is.na(value)) > 1) %>%
          mutate(value = ifelse(is.na(value), min(value, na.rm = TRUE) / 5, value))
      } else if (method == "NA用该变量在所有样本中的最小值的1/5代替") {
        df <- df %>%
          group_by(var) %>%
          mutate(value = ifelse(is.na(value), min(value, na.rm = TRUE) / 5, value))
      } else if (method == "NA用该样本的组内的最小值的1/5代替，若该组全为NA，全变成0") {
        df <- df %>%
          group_by(var, group) %>%
          mutate(
            value = ifelse(
              is.na(value),
              ifelse(all(is.na(value)), 0, min(value, na.rm = TRUE) / 5),
              value
            )
          )
      } else if (method == "先检查该组中NA的比例，若NA的比例小于等于70%，该组中所有NA用该变量在所在组中的最小值的1/5代替，否则该变量在该组所有样本都变为0") {
        df <- df %>%
          group_by(var, group) %>%
          mutate(
            na_percentage = mean(is.na(value)),
            value = ifelse(
              na_percentage <= 0.7,
              ifelse(is.na(value), min(value, na.rm = TRUE) / 5, value),
              ifelse(is.na(value), 0, 0)
            )
          ) %>%
          select(-"na_percentage")
      }
      rv$pp$data_o_m <- df %>%
        ungroup() %>%
        select(1:3)
      rv$pp$data_o_m_n <- rv$pp$data_o_m
    })

    # Normalization_sidebar
    excelInputServer("normalization_factor", "1")
    observeEvent(input$normalization_select, {
      selected <- input$normalization_select
      choices <- NULL
      if (selected == "Sample Specific") {
        observeEvent(input$normalization_factor_read, {
          rv$pp$data_o_m_n_factor <- excelInputServer("normalization_factor", "data")
        })
      } else if (selected == "Normalization by sum") {

      } else if (selected == "Normalization by median") {

      } else if (selected == "Normalization by a reference var") {
        choices <- unique(rv$pp$data$var)
      } else if (selected == "Normalization by a reference sample (PQN)") {
        choices <- unique(rv$pp$data$sample)
      } else if (selected == "Normalization by a pooled sample from group (group PQN)") {
        choices <- rv$pp$data_desc[-1] %>% as.list()
      }
      updateSelectInput(session, "normalization_specify", choices = choices, selected = character(0))
    })

    output$normalization_factor_DT <- renderDT({
      req(rv$pp$data_o_m_n_factor)
      datatable(
        rv$pp$data_o_m_n_factor,
        selection = "none",
        options = list(
          dom = "rt",
          pageLength = -1,
          scrollY = "390px"
        )
      )
    })


    # 执行Normalization
    observeEvent(input$normalization_select, {
      if (input$normalization_select == "None") {
        rv$pp$data_o_m_n <- rv$pp$data_o_m
      }
    })
    observeEvent(input$normalization_submit, {
      if (input$normalization_select == "Sample Specific") {
        factor <- rv$pp$data_o_m_n_factor
        rv$pp$data_o_m_n <- rv$pp$data_o_m %>%
          left_join(factor, by = "sample") %>%
          mutate(value = value / factor) %>%
          select(-c("factor"))
      } else if (input$normalization_select == "Normalization by sum") {
        rv$pp$data_o_m_n <- rv$pp$data_o_m %>%
          group_by(sample) %>%
          mutate(value = value / sum(value, na.rm = TRUE))
      } else if (input$normalization_select == "Normalization by median") {
        rv$pp$data_o_m_n <- rv$pp$data_o_m %>%
          group_by(sample) %>%
          mutate(value = value / median(value, na.rm = TRUE))
      } else if (input$normalization_select == "Normalization by a reference var") {

      } else if (input$normalization_select == "Normalization by a reference sample (PQN)") {

      } else if (input$normalization_select == "Normalization by a pooled sample from group (group PQN)") {

      }
    })




    # Valuation 1_mainPanel


    # Valuation 2_mainPanel




    datas <- c("data", "data_desc", "data_o", "data_o_m", "data_o_m_n")
    lapply(datas, function(data) {
      output[[data]] <- renderDT({
        req(rv$pp[[data]])
        if (data == "data_desc") {
          df <- rv$pp[[data]]
        } else {
          df <- rv$pp[[data]] %>% pivot_wider(names_from = "sample", values_from = "value")
        }
        datatable(
          df,
          selection = "none",
          extensions = "FixedColumns",
          options = list(
            scrollX = TRUE,
            fixedColumns = list(leftColumns = 2)
          )
        )
      })
    })

    output$export <- downloadHandler(
      filename = function() {
        paste0(rv$pp$name, "_Preprocessing", ".xlsx")
      },
      content = function(file) {
        list_of_datasets <- sapply(datas, function(data) {
          if (data == "data_desc") {
            df <- rv$pp[[data]]
          } else {
            df <- rv$pp[[data]] %>% pivot_wider(names_from = "sample", values_from = "value")
          }
          return(df)
        })
        write.xlsx(list_of_datasets, file)
      }
    )

  })
}
