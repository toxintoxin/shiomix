aggrUI <- function(id) {
  ns <- NS(id)
  layout_sidebar(
    sidebar = sidebar(width = "25%", open = "always",
      accordion(id = ns("sp"), multiple = FALSE,
        accordion_panel("Upload",
          fileInput(ns("data"), label = ".csv or .xlsx", accept = c(".csv", ".xlsx"), multiple = FALSE),
          uiOutput(ns("data_sheetUI")),
          radioButtons(ns("data_format"), label = NULL, choices = c("一般形式", "长数据"), inline = TRUE),
          friendlyAct(ns("data_read"), "读取"),
        ),
        accordion_panel("Label",
          selectInput(ns("label_column"), "Column", choices = c(Choose = "")),
          selectInput(ns("label_pattern"), "Pattern", choices = c(Choose = "", "最后一个空格前的字符", "最后一个下划线前的字符", "测试错误用")),
          textInput(ns("label_name"), "New Column"),
          actionButton(ns("label_add"), "Add"),
          tableOutput(ns("label_info"))
        ),
        accordion_panel("Filter",
          textAreaInput(ns("filter_code"), label = NULL, value = "rv$aggr$data_l_f <- rv$aggr$data_l %>% \n    ", rows = 4, width = "calc(100% - 15px)", resize = "vertical"),
          dropMenu(
            actionLink(ns("filter_code_help"), label = NULL, icon = icon("handshake-angle")),
            includeMarkdown("aggregate/code_help.md"),
            placement = "right-start", hideOnClick = "toggle"
          ),
          actionButton(ns("filter_code_run"), "Run")
        ),
        accordion_panel("Summarise",
          div(style = "display: flex; margin-bottom: 5px; gap: 5px;",
            tags$span("Group by", style = "font-weight: 700;"),
            actionLink(ns("summarise_groupby_q"), label = NULL, icon = icon("circle-question"))
          ),
          div(style = "display: flex; align-items: flex-start; gap: 30px;",
            selectInput(ns("summarise_groupby"), label = NULL, choices = c(Choose = ""), multiple = TRUE),
            actionButton(ns("summarise_groupby_ok"), "OK"),
            hidden(actionButton(ns("summarise_reset"), label = NULL, icon = icon("arrow-rotate-right")))
          ),
          hidden(selectInput(ns("summarise_fun"), "Function ('na.rm = TRUE' is set up)", choices = c(Choose = "", "n", "sum", "mean", "median", "max", "min"))),
          hidden(actionButton(ns("summarise_add"), "Summarise"))
        )
      )
    ),
    navset_tab(id = ns("mp"),
      nav_panel("Upload", DTOutput(ns("data"))),
      nav_panel("Label", DTOutput(ns("data_l"))),
      nav_panel("Filter", DTOutput(ns("data_l_f"))),
      nav_panel("Summarise", DTOutput(ns("data_l_f_s"))),
      nav_spacer(),
      nav_item(downloadButton(ns("export"), "导出xlsx"))
    )
  )
}

aggrServer <- function(id, rv) {
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
        selectInput(ns("data_sheet"), "Sheet", choices = excel_sheets(input$data$datapath))
      }
    })
    observeEvent(input$data, {
      rv$aggr$file <- input$data
    })
    observe({
      toggleState("data_read", !is.null(rv$aggr$file) && !is.null(input$data_format))
    })

    friendlyActServer("data_read", {

      rv$aggr$name <- read_excel_name(rv$aggr$file, input$data_sheet)
      rv$aggr$data <- read_excel_data(rv$aggr$file, input$data_sheet)

      if (input$data_format == "一般形式") {
        rv$aggr$data <- rv$aggr$data %>% pivot_longer(-1, names_to = "sample", values_to = "value") %>% as.data.frame()
      } else if (input$data_format == "长数据") {
        rv$aggr$data <- rv$aggr$data %>% as.data.frame()
      }

      # 让所有值一样
      rv$aggr$data_l <- rv$aggr$data
      rv$aggr$data_l_f <- rv$aggr$data

      # 隐藏控件，显示文件信息

      updateSelectInput(session, "label_column", choices = c(Choose = "", colnames(rv$aggr$data_l)))
      updateSelectInput(session, "summarise_groupby", choices = c(Choose = "", colnames(rv$aggr$data_l_f)))

    })

    observe({
      toggleState("label_add", nchar(input$label_column) > 0 && nchar(input$label_pattern) > 0 && nchar(input$label_name) > 0)
    })

    observeEvent(input$label_add, {
      col <- input$label_column
      pattern <- input$label_pattern
      name <- input$label_name
      if (pattern == "最后一个空格前的字符") {
        rv$aggr$data_l[[name]] <- rv$aggr$data_l[[col]] %>% str_extract(".*(?=\\s.*)")
      } else if (pattern == "最后一个下划线前的字符") {
        rv$aggr$data_l[[name]] <- rv$aggr$data_l[[col]] %>% str_extract(".*(?=_.*)")
      } else if (pattern == "测试错误用") {
        rv$aggr$data_l[[name]] <- rv$aggr$data_l$columndoesntexist %>% str_extract(".*(?=_.*)")
      }
      rv$aggr$data_l_f <- rv$aggr$data_l
      rv$aggr$data_l_info <- data.frame(Column = NULL, Pattern = NULL, `New Column` = NULL, check.names = FALSE)
      new_label <- data.frame(
        Column = col,
        Pattern = pattern,
        `New Column` = name,
        check.names = FALSE
      )
      rv$aggr$data_l_info <- rbind(rv$aggr$data_l_info, new_label)
      updateSelectInput(session, "label_column", choices = c(Choose = "", colnames(rv$aggr$data_l)))
      updateSelectInput(session, "summarise_groupby", choices = c(Choose = "", colnames(rv$aggr$data_l_f)))
      reset("label_pattern")
      reset("label_name")
    })

    output$label_info <- renderTable({
      rv$aggr$data_l_info
    })

    # Filter
    observeEvent(input$filter_code_run, {
      eval(parse(text = input$filter_code))
      updateSelectInput(session, "summarise_groupby", choices = c(Choose = "", colnames(rv$aggr$data_l_f)))
    })

    observe({
      toggleState("summarise_groupby_ok", !is.null(input$summarise_groupby))
    })

    observeEvent(input$summarise_groupby_ok, {
      groupby <- input$summarise_groupby
      rv$aggr$data_l_f_grouped <- rv$aggr$data_l_f %>% group_by(across(all_of(groupby)))
      disable("summarise_groupby")
      shinyjs::hide("summarise_groupby_ok")
      shinyjs::show("summarise_fun")
      shinyjs::show("summarise_add")
      shinyjs::show("summarise_reset")
    })

    observe({
      toggleState("summarise_add", nchar(input$summarise_fun) > 0)
    })

    observeEvent(input$summarise_add, {
      fun <- input$summarise_fun

      if (fun == "n") {
        para <- "n = n()"
      } else if (fun == "sum") {
        para <- "sum = sum(value, na.rm = TRUE)"
      } else if (fun == "mean") {
        para <- "mean = mean(value, na.rm = TRUE)"
      } else if (fun == "median") {
        para <- "median = median(value, na.rm = TRUE)"
      } else if (fun == "max") {
        para <- "max = max(value, na.rm = TRUE)"
      } else if (fun == "min") {
        para <- "min = min(value, na.rm = TRUE)"
      }
      rv$aggr$summarise_para = data.frame(summarise = character())
      rv$aggr$summarise_para <- rv$aggr$summarise_para %>%
        filter(summarise != para) %>%
        bind_rows(data.frame(summarise = para))

      rv$aggr$data_l_f_s <- rv$aggr$data_l_f_grouped %>%
        summarise(!!!rlang::parse_exprs(rv$aggr$summarise_para$summarise)) %>%
        rename("value" = 3)

      reset("summarise_fun")
    })

    observeEvent(input$summarise_reset, {
      rv$aggr$data_l_f_grouped <- NULL
      shinyjs::hide("summarise_fun")
      reset("summarise_fun")
      updateSelectInput(session, "summarise_groupby", choices = c(Choose = "", colnames(rv$aggr$data_l_f)))
      reset("summarise_groupby")
      enable("summarise_groupby")
      shinyjs::hide("summarise_reset")
      shinyjs::show("summarise_groupby_ok")
    })

    datas <- c("data", "data_l", "data_l_f", "data_l_f_s")
    lapply(datas, function(data) {
      output[[data]] <- renderDT({
        req(rv$aggr[[data]])
        datatable(
          rv$aggr[[data]],
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
        paste0(rv$aggr$name, "_Aggregate", ".xlsx")
      },
      content = function(file) {
        list_of_datasets <- sapply(datas, function(data_name) rv$aggr[[data_name]])
        write.xlsx(list_of_datasets, file)
      }
    )

  })
}