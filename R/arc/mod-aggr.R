aggr_ui <- function(id) {
  ns <- NS(id)
  navset_card_pill(height = "820px",
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
    nav_panel("Label",
      selectInput(ns("label_column"), "Column", choices = c(Choose = "")),
      selectInput(ns("label_pattern"), "Pattern", choices = c(Choose = "", "最后一个空格前的字符", "最后一个下划线前的字符", "测试错误用")),
      textInput(ns("label_name"), "New Column"),
      actionButton(ns("label_add"), "Add"),
      tableOutput(ns("label_info"))
    )

  )
  layout_sidebar(
    sidebar = sidebar(width = "25%", open = "always",
      accordion(id = ns("sp"), multiple = FALSE,
        accordion_panel("Upload",
          excelInput(ns("data"), "Data"),
          fileInput(ns("data"), label = ".csv or .xlsx", accept = c(".csv", ".xlsx"), multiple = FALSE),
          uiOutput(ns("data_sheet_ui")),
          radioButtons(ns("data_format"), label = NULL, choices = c("一般形式", "长数据"), inline = TRUE),
          friendlyAct(ns("data_read"), "读取"),
        ),
        accordion_panel("Label",

        ),
        accordion_panel("Filter",
          textAreaInput(ns("filter_code"), label = NULL, value = "rv$data_l_f <- rv$data_l %>% \n    ", rows = 4, width = "calc(100% - 15px)", resize = "vertical"),
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

aggr_server <- function(id) {
  moduleServer(id, function(input, output, session) {

    ns <- session$ns

    rv <- reactiveValues()

    # upload
    observeEvent(input$show_file_guide, {
      showModal(modalDialog(
        size = "xl",
        includeMarkdown("data-science/aggregate/file-guide.md")
      ))
    })

    data_ls <- excel_server("data")

    observe({
      rv$data_original <- data_ls$data()
      rv$name <- data_ls$name()
    })

    observeEvent(input$handle, {
      dup <- duplicated(rv$data_original[, 1]) | duplicated(rv$data_original[, 1], fromLast = TRUE)
      if (any(dup)) {
        showNotification("Same name in first column, check your data", type = "error")
      } else {
        rv$data <- rv$data_original %>% rename("var" = 1) %>% pivot_longer(-1, names_to = "sample", values_to = "value")
      }
      rv$data_l <- rv$data
      rv$data_l_f <- rv$data
    })

    # label
    observe({toggleState("label_add", nchar(input$label_column) > 0 && nchar(input$label_pattern) > 0 && nchar(input$label_name) > 0)})

    observeEvent(input$label_add, {
      col <- input$label_column
      pattern <- input$label_pattern
      name <- input$label_name
      if (pattern == "最后一个空格前的字符") {
        rv$data_l[[name]] <- rv$data_l[[col]] %>% str_extract(".*(?=\\s.*)")
      } else if (pattern == "最后一个下划线前的字符") {
        rv$data_l[[name]] <- rv$data_l[[col]] %>% str_extract(".*(?=_.*)")
      } else if (pattern == "测试错误用") {
        rv$data_l[[name]] <- rv$data_l$columndoesntexist %>% str_extract(".*(?=_.*)")
      }
      rv$data_l_f <- rv$data_l
      rv$data_l_info <- data.frame(Column = NULL, Pattern = NULL, `New Column` = NULL, check.names = FALSE)
      new_label <- data.frame(
        Column = col,
        Pattern = pattern,
        `New Column` = name,
        check.names = FALSE
      )
      rv$data_l_info <- rbind(rv$data_l_info, new_label)
      updateSelectInput(session, "label_column", choices = c(Choose = "", colnames(rv$data_l)))
      updateSelectInput(session, "summarise_groupby", choices = c(Choose = "", colnames(rv$data_l_f)))
      reset("label_pattern")
      reset("label_name")
    })

    output$label_info <- renderTable({
      rv$data_l_info
    })

    # filter
    observeEvent(input$filter_code_run, {
      eval(parse(text = input$filter_code))
      updateSelectInput(session, "summarise_groupby", choices = c(Choose = "", colnames(rv$data_l_f)))
    })

    # summarise
    observe({
      toggleState("summarise_groupby_ok", !is.null(input$summarise_groupby))
    })

    observeEvent(input$summarise_groupby_ok, {
      groupby <- input$summarise_groupby
      rv$data_l_f_grouped <- rv$data_l_f %>% group_by(across(all_of(groupby)))
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
        params <- "n = n()"
      } else if (fun == "sum") {
        params <- "sum = sum(value, na.rm = TRUE)"
      } else if (fun == "mean") {
        params <- "mean = mean(value, na.rm = TRUE)"
      } else if (fun == "median") {
        params <- "median = median(value, na.rm = TRUE)"
      } else if (fun == "max") {
        params <- "max = max(value, na.rm = TRUE)"
      } else if (fun == "min") {
        params <- "min = min(value, na.rm = TRUE)"
      }
      rv$summarise_params = data.frame(summarise = character())
      rv$summarise_params <- rv$summarise_params %>%
        filter(summarise != params) %>%
        bind_rows(data.frame(summarise = params))

      rv$data_l_f_s <- rv$data_l_f_grouped %>%
        summarise(!!!rlang::parse_exprs(rv$summarise_params$summarise)) %>%
        rename("value" = 3)

      reset("summarise_fun")
    })

    observeEvent(input$summarise_reset, {
      rv$data_l_f_grouped <- NULL
      shinyjs::hide("summarise_fun")
      reset("summarise_fun")
      updateSelectInput(session, "summarise_groupby", choices = c(Choose = "", colnames(rv$data_l_f)))
      reset("summarise_groupby")
      enable("summarise_groupby")
      shinyjs::hide("summarise_reset")
      shinyjs::show("summarise_groupby_ok")
    })

    # export
    datas <- c("data", "data_l", "data_l_f", "data_l_f_s")
    output$export <- downloadHandler(
      filename = function() {
        paste0(rv$name, "_aggregate", ".xlsx")
      },
      content = function(file) {
        list_of_datasets <- sapply(datas, function(data_name) {
          rv[[data_name]]
        })
        write_xlsx(list_of_datasets, file, format_headers = FALSE)
      }
    )

    # DT render
    lapply(datas, function(data) {
      output[[data]] <- renderDT({
        req(rv[[data]])
        datatable(
          rv[[data]],
          selection = "none",
          extensions = "FixedColumns",
          options = list(
            scrollX = TRUE,
            fixedColumns = list(leftColumns = 2)
          )
        )
      })
    })

  })
}