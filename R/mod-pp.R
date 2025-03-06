#' @import bslib
#' @import DT
#' @import readr
#' @import shinyWidgets
#' @import shinyjs
#' @import dplyr
#' @import tidyr
#' @import stringr
#' @import forcats



ppUI <- function(id) {
  ns <- NS(id)
  navset_card_pill(height = "900px",
    nav_panel("Upload",
      layout_sidebar(
        sidebar = sidebar(width = "25%", open = "always",
          actionButton(ns("show_file_guide"), label = "Show file guide"),
          excelInput(ns("data"), header = "Data"),
          actionButton(ns("data_handle"), label = "Handle")
        ),
        DTOutput(ns("data"))
      )
    ),
    nav_panel("Description",
      layout_sidebar(
        sidebar = sidebar(width = "25%", open = "always",
          radioButtons(ns("desc_source"), label = NULL, choices = c("从命名读取", "从本地导入"), inline = TRUE),
          conditionalPanel(ns = ns, condition = "input.desc_source == '从命名读取'",
            helpText("以最后一个下划线为分隔符，前为描述，后为计数")
          ),
          conditionalPanel(ns = ns, condition = "input.desc_source == '从本地导入'",
            fileInput(ns("desc"), label = ".csv or .xlsx", accept = c(".csv", ".xlsx")),
            uiOutput(ns("desc_sheet_ui")),
          ),
          actionButton(ns("desc_read"), label = "Read")
        ),
        DTOutput(ns("desc"))
      )
    ),
    nav_panel("Valuation 1",
      layout_sidebar(
        sidebar = sidebar(width = "25%", open = "always",
          "Widgets of Valuation 1"
        ),
        h1("multiple plots of Valuation 1")
      )
    ),
    nav_panel("Outlier",
      layout_sidebar(
        sidebar = sidebar(width = "25%", open = "always",
          h4("Step1: "),
          actionButton(ns("outlier_remove_data_var_all_na"), label = "移去在所有样本中均为NA的变量"),
          br(),
          textOutput(ns("outlier_remove_data_var_all_na_tip"))
        ),
        DTOutput(ns("data_o"))
      )
    ),
    nav_panel("Valuation 2",
      layout_sidebar(
        sidebar = sidebar(width = "25%", open = "always",
          "Widgets of Valuation 2"
        ),
        h1("multiple plots of Valuation 2")
      )
    ),
    nav_panel("Missing value",
      layout_sidebar(
        sidebar = sidebar(width = "25%", open = "always",
          selectInput(ns("missing_value_method"), label = "缺失值处理方法", choices = c(Choose = "", "None", "同MetaboAnalyst，先删除只有一个样本有值的变量，NA用该变量在所有样本中的最小值的1/5代替", "NA用该变量在所有样本中的最小值的1/5代替", "NA用该样本的组内的最小值的1/5代替，若该组全为NA，全变成0", "先检查该组中NA的比例，若NA的比例小于等于70%，该组中所有NA用该变量在所在组中的最小值的1/5代替，否则该变量在该组所有样本都变为0")),
          conditionalPanel(ns = ns, condition = "['NA用该样本的组内的最小值的1/5代替，若该组全为NA，全变成0', '先检查该组中NA的比例，若NA的比例小于等于70%，该组中所有NA用该变量在所在组中的最小值的1/5代替，否则该变量在该组所有样本都变为0'].indexOf(input.missing_value_method)>-1",
            selectInput(ns("missing_value_group"), label = "分组依据", choices = c(Choose = ""))
          ),
          actionButton(ns("missing_value_handle"), label = "执行")
        ),
        DTOutput(ns("data_o_m"))
      )
    ),
    nav_panel("Normalization",
      layout_sidebar(
        sidebar = sidebar(width = "25%", open = "always",
          selectInput(ns("normalization_method"), label = NULL, width = "100%", choices = c(Choose = "", "None", "Sample Specific", "Normalization by sum", "Normalization by median", "Normalization by a reference var", "Normalization by a reference sample (PQN)", "Normalization by a pooled sample from group (group PQN)")),
          conditionalPanel(ns = ns, condition = "input.normalization_method == 'Sample Specific'",
            fileInput(ns("normalization_factor"), label = ".csv or .xlsx", accept = c(".csv", ".xlsx")),
            uiOutput(ns("normalization_factor_sheet_ui")),
            actionButton(ns("normalization_factor_read"), label = "读取"),
            DTOutput(ns("normalization_factor_DT"))
          ),
          conditionalPanel(ns = ns, condition = "['Normalization by a reference var', 'Normalization by a reference sample (PQN)', 'Normalization by a pooled sample from group (group PQN)'].indexOf(input.normalization_method)>-1",
            selectInput(ns("normalization_specify"), label = NULL, choices = c(Choose = ""))
          ),
          actionButton(ns("normalization_submit"), label = "执行")
        ),
        DTOutput(ns("data_o_m_n"))
      )
    ),
    nav_panel("Export",
      layout_sidebar(
        sidebar = sidebar(width = "25%", open = "always",
          downloadButton(ns("export_multi_csv"), label = "导出多个csv (still building)"),
          downloadButton(ns("export_xlsx"), label = "导出xlsx"),
          downloadButton(ns("export_rmd"), label = "导出Rmarkdown (still building)"),
          hr(),
          downloadButton(ns("export_classed"), label = "导出分类求和的, 仅对用第一个空格前的字符表示类别的数据有效"),
        ),
        "如果是csv，导出名总为originalfilename_preprocessing", br(),
        "如果是xlsx，导出名总为originalfilename_#sheetname#_preprocessing"
      )
    ),
    nav_spacer(),
    nav_item(actionLink(ns("reset"), label = "Restart", icon = icon("arrows-rotate")))
  )
}

ppServer <- function(id) {
  moduleServer(id, function(input, output, session) {

    ns <- session$ns

    rv <- reactiveValues()

    # upload
    observeEvent(input$show_file_guide, {
      showModal(modalDialog(
        size = "xl",
        includeMarkdown("omics/data-preprocessing/file-guide.md")
      ))
    })

    data_ls <- excelServer("data")

    observe({
      rv$data_original <- data_ls$data()
      rv$name <- data_ls$name()
    })

    observeEvent(input$data_handle, {
      dup <- duplicated(rv$data_original[1]) | duplicated(rv$data_original[1], fromLast = TRUE)
      if (any(dup)) {
        showNotification("Same name in first column, check your data", type = "error")
      } else {
        rv$data <- rv$data_original %>% rename("var" = 1) %>% pivot_longer(-1, names_to = "sample", values_to = "value")
      }
    })

    # description
    observe({
      toggleState("desc_read", condition = !is.null(rv$data) && (input$desc_source == "从命名读取" || input$desc_source == "从本地导入" && !is.null(input$desc)))
    })

    desc_ext <- eventReactive(input$desc, {
      tools::file_ext(input$desc$name)
    })

    output$desc_sheet_ui <- renderUI({
      req(desc_ext())
      if (desc_ext() == "xlsx") {
        selectInput(ns("desc_sheet"), label = "Sheet", choices = excel_sheets(input$desc$datapath))
      }
    })

    observeEvent(input$desc_read, {
      if (input$desc_source == "从命名读取") {
        rv$desc <- rv$data %>%
          mutate(group = str_replace(sample, "^(.*)_.*$", "\\1")) %>%  # 返回最后一个下划线前的字符，如果没有下划线，返回全部
          select(c("sample", "group")) %>%
          distinct()
      } else if (input$desc_source == "从本地导入") {
        if (desc_ext() == "csv") {
          rv$desc <- read_csv(input$desc$datapath)
        } else if (desc_ext() == "xlsx") {
          rv$desc <- read_xlsx(input$desc$datapath, input$desc_sheet)
        }
      }
      updateSelectInput(session, "missing_value_group", choices = colnames(rv$desc)[-1])
    })

    # valuation 1



    # outlier
    observeEvent(input$outlier_remove_data_var_all_na, {
      rv$data_var_all_na <- rv$data %>%
        group_by(var) %>%
        filter(all(is.na(value))) %>%
        ungroup()
      rv$data_o <- rv$data %>%
        group_by(var) %>%
        filter(any(!is.na(value))) %>%
        ungroup()
    })

    output$outlier_remove_data_var_all_na_tip <- renderText({
      req(rv$data_var_all_na)
      count <- rv$data_var_all_na %>%
        select("var") %>%
        distinct() %>%
        nrow()
      paste0("已经删除了", count, "个变量，他们在所有样本中均为NA")
    })

    # valuation 2



    # missing value
    observe({
      toggleState("missing_value_handle", condition = !is.null(rv$data_o) && input$missing_value_method != "")
    })

    observeEvent(input$missing_value_handle, {
      method <- input$missing_value_method
      df <- rv$data_o %>%
        left_join(rv$desc, by = "sample") %>%
        rename("group" = input$missing_value_group)
      if (method == "None") {
        rv$data_o_m <- rv$data_o
      } else if (method == "同MetaboAnalyst，先删除只有一个样本有值的变量，NA用该变量在所有样本中的最小值的1/5代替") {
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
      rv$data_o_m <- df %>%
        ungroup() %>%
        select(1:3)
      rv$data_o_m_n <- rv$data_o_m
    })

    # normalization
    observe({
      toggleState("normalization_submit", condition = !is.null(rv$data_o_m) && (
        input$normalization_method == "Sample Specific" && !is.null(rv$data_o_m_n_factor) ||
        input$normalization_method %in% c("Normalization by sum", "Normalization by median") ||
        input$normalization_method %in% c("Normalization by a reference var", "Normalization by a reference sample (PQN)", "Normalization by a pooled sample from group (group PQN)") && input$normalization_specify != ""
      ))
    })

    observe({
      toggleState("normalization_factor_read", condition = !is.null(rv$data_o_m) && !is.null(input$normalization_factor))
    })

    normalization_factor_ext <- eventReactive(input$normalization_factor, {
      tools::file_ext(input$normalization_factor$name)
    })

    output$normalization_factor_sheet_ui <- renderUI({
      req(normalization_factor_ext())
      if (normalization_factor_ext() == "xlsx") {
        selectInput(ns("normalization_factor_sheet"), label = "Sheet", choices = excel_sheets(input$normalization_factor$datapath))
      }
    })

    observeEvent(input$normalization_factor_read, {
      if (normalization_factor_ext() == "csv") {
        rv$data_o_m_n_factor <- read_csv(input$normalization_factor$datapath)
      } else if (normalization_factor_ext() == "xlsx") {
        rv$data_o_m_n_factor <- read_xlsx(input$normalization_factor$datapath, input$normalization_factor_sheet)
      }
    })

    observeEvent(input$normalization_method, {
      method <- input$normalization_method
      choices <- NULL
      if (method == "Normalization by a reference var") {
        choices <- unique(rv$data_o_m$var)
      } else if (method == "Normalization by a reference sample (PQN)") {
        choices <- unique(rv$data_o_m$sample)
      } else if (method == "Normalization by a pooled sample from group (group PQN)") {
        choices <- rv$desc[-1] %>% as.list()
      }
      updateSelectInput(session, "normalization_specify", choices = choices, selected = character(0))
    })

    output$normalization_factor_DT <- renderDT({
      req(rv$data_o_m_n_factor)
      datatable(
        rv$data_o_m_n_factor,
        selection = "none",
        options = list(
          dom = "rt",
          pageLength = -1,
          scrollY = "390px"
        )
      )
    })

    observeEvent(input$normalization_submit, {
      method <- input$normalization_method
      if (method == "None") {
        rv$data_o_m_n <- rv$data_o_m
      } else if (method == "Sample Specific") {
        factor <- rv$data_o_m_n_factor
        rv$data_o_m_n <- rv$data_o_m %>%
          left_join(factor, by = "sample") %>%
          mutate(value = value / factor) %>%
          select(-c("factor"))
      } else if (method == "Normalization by sum") {
        rv$data_o_m_n <- rv$data_o_m %>%
          group_by(sample) %>%
          mutate(value = value / sum(value))
      } else if (method == "Normalization by median") {
        rv$data_o_m_n <- rv$data_o_m %>%
          group_by(sample) %>%
          mutate(value = value / median(value))
      } else if (method == "Normalization by a reference var") {
        showNotification("nothing changed, still building, sorry", type = "error")
      } else if (method == "Normalization by a reference sample (PQN)") {
        showNotification("nothing changed, still building, sorry", type = "error")
      } else if (method == "Normalization by a pooled sample from group (group PQN)") {
        showNotification("nothing changed, still building, sorry", type = "error")
      }
    })

    # export
    datas <- c("data", "desc", "data_o", "data_o_m", "data_o_m_n")
    output$export_xlsx <- downloadHandler(
      filename = function() {
        paste0(rv$name, "_preprocessing", ".xlsx")
      },
      content = function(file) {
        list_of_datasets <- sapply(datas, function(data) {
          if (data == "desc") {
            df <- rv[[data]]
          } else {
            df <- rv[[data]] %>% pivot_wider(names_from = "sample", values_from = "value")
          }
          return(df)
        })
        openxlsx::write.xlsx(list_of_datasets, file)
      }
    )

    # DT render
    lapply(datas, function(data) {
      output[[data]] <- renderDT({
        req(rv[[data]])
        if (data == "desc") {
          df <- rv[[data]]
        } else {
          df <- rv[[data]] %>% pivot_wider(names_from = "sample", values_from = "value")
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


    # classed
    output$export_classed <- downloadHandler(
      filename = function() {
        paste0(rv$name, "_preprocessing_o_m_n_SumClassed", ".csv")
      },
      content = function(file) {
        rv$data_o_m_n_classed <- rv$data_o_m_n %>%
          mutate(class = str_extract(var, "\\S+")) %>%
          mutate(sample = fct_inorder(sample), class = fct_inorder(class)) %>%
          group_by(sample, class) %>%
          summarise(sum = sum(value)) %>%
          pivot_wider(names_from = "sample", values_from = "sum")
        write_csv(rv$data_o_m_n_classed, file)
      }
    )

  })
}