data(adducts)
data(isotopes)

enviPatUI <- function(id) {
  ns <- NS(id)
  fluidRow(
    column(width = 5, style = "padding: 5px;",
      div(style = "display: flex; gap: 1%;",
        textInput(ns("compound_formula"), "Formula 检查大小写是必要的"),
        pickerInput(ns("adduct_select"), "Adduct", choices = c(adducts$Name, "M-H2O+H"), choicesOpt = list(icon = c(rep("glyphicon-minus", 18), rep("glyphicon-plus", 34))), selected = "M+H", options = list(`live-search` = TRUE)),
        actionButton(ns("compound_details"), "View the Details")
      ),
      div(
        "Adduct_mz",
        verbatimTextOutput(ns("adduct_mz"))
      )
    ),
    column(width = 7, style = "padding: 5px;",
      accordion(open = "form_calc",
        accordion_panel("form_handle",
          fluidRow(
            column(width = 3,
              fileInput(ns("form_handle_file"), "上传化合物列表", accept = ".xlsx")
            ),
            column(width = 2, style = "margin-top: 25px",
              actionButton(ns("form_handle_filter"), "添加筛选条件")
            ),
            column(width = 1, style = "margin-top: 25px; text-align: right;",
              downloadButton(ns("form_handle_template"), "下载模板")
            ),
            column(width = 6, style = "margin-top: 25px; text-align: right;",
              downloadButton(ns("form_handle_download"), "下载结果", icon = icon("file-export"))
            )
          ),
          fluidRow(
            DTOutput(ns("form_handle_list"))
          )
        ),
        accordion_panel("form_calc",
          fluidRow(
            column(width = 3,
              fileInput(ns("compound_list_file"), "上传化合物列表", accept = ".xlsx")
            ),
            column(width = 2, style = "margin-top: 25px",
              actionButton(ns("compound_list_mz_calc"), "计算adduct")
            ),
            column(width = 1, style = "margin-top: 25px; text-align: right;",
              downloadButton(ns("compound_list_template"), "下载模板")
            ),
            column(width = 6, style = "margin-top: 25px; text-align: right;",
              downloadButton(ns("compound_mz_list_download"), "下载结果", icon = icon("file-export"))
            )
          ),
          fluidRow(
            DTOutput(ns("compound_mz_list"))
          )
        )
      )
    )
  )
}

enviPatServer <- function(id, rv) {
  moduleServer(id, function(input, output, session) {

    ns <- session$ns

    # 扩展adducts
    adducts_H2O_H <- list("M-H2O+H", NA, 1, 1, NA, "positive", "H1", "H2O1", 1)
    adducts <- rbind(adducts, adducts_H2O_H)

    # 单个的
    compound_formula <- reactive({
      # 检查化学式
      compound_formula_checked <- check_chemform(isotopes, input$compound_formula)[1, 2]
      ## adduct不在列表中的或者warning为TRUE的报警
      # compound_list$warning <- ifelse(!compound_list$adduct %in% adducts$Name, TRUE, compound_list$warning)
      compound_formula_checked
    })

    observeEvent(input$compound_details, {
      # 乘
      compound_formula_calc1 <- multiform(compound_formula(), filter(adducts, Name == input$adduct_select) %>% pull(Multi))
      # 加
      if (filter(adducts, Name == input$adduct_select) %>% pull(Formula_add) == FALSE) {
        compound_formula_calc2 <- compound_formula_calc1
      } else {
        compound_formula_calc2 <- mergeform(compound_formula_calc1, filter(adducts, Name == input$adduct_select) %>% pull(Formula_add))
      }
      # 减
      if (filter(adducts, Name == input$adduct_select) %>% pull(Formula_ded) == FALSE) {
        compound_formula_calc3 <- compound_formula_calc2
      } else {
        compound_formula_calc3 <- subform(compound_formula_calc2, filter(adducts, Name == input$adduct_select) %>% pull(Formula_ded))
      }

      # 计算adduct_mz
      pattern <- isopattern(
        isotopes,
        compound_formula_calc3,
        threshold = 0.01,
        charge = filter(adducts, Name == input$adduct_select) %>% pull(Charge),
        emass = 0.00054857990924,
        plotit = FALSE,
        algo = 1
      )
      output$adduct_mz <- renderPrint({
        pattern
      })
      # adduct_mz <- lapply(pattern, function(matrix) {
      #       # 从矩阵中提取 abundance 列为 100 的行
      #       subset_matrix <- matrix[matrix[, "abundance"] == 100, ]
      #       return(subset_matrix)
      #     })
      # compound_mz_list <- cbind(compound_list, adduct_mz = sapply(adduct_mz, function(x) x[1]) %>% unname())
    })









    # 提供form_handle模板
    output$form_handle_template <- downloadHandler(
      filename = "form handle template.xlsx",
      content = function(file) {
        form_handle_template <- data.frame(
          compound = c("not necessary"),
          formula = c(NA),
          stringsAsFactors = FALSE
        )
        write.xlsx(form_handle_template, file)
      }
    )




    # 添加筛选条件






    form_handle <- reactive({
      req(input$form_handle_file)
      form_handle <- read_xlsx(input$form_handle_file$datapath, col_names = TRUE)
      # 检查化学式再合并
      form_handle <- cbind(form_handle, check_chemform(isotopes, form_handle$formula)) %>%
        select(c("compound", "formula", "warning", "new_formula"), everything())
      # adduct不在列表中的，warning为TRUE
      #  form_handle$warning <- ifelse(!form_handle$adduct %in% adducts$Name, TRUE, form_handle$warning)
      form_handle
    })

    output$form_handle_list <- renderDT({
      datatable(form_handle())
    })


    # 提供compound_list模板
    output$compound_list_template <- downloadHandler(
      filename = "compound list template.xlsx",
      content = function(file) {
        compound_list_template <- data.frame(
          compound = c("not necessary", rep(NA, length(adducts$Name))),
          formula = rep(NA, length(adducts$Name) + 1),
          adduct = c("should be the following", adducts$Name),
          stringsAsFactors = FALSE
        )
        write.xlsx(compound_list_template, file)
      }
    )


    compound_list <- reactive({
      req(input$compound_list_file)
      compound_list <- read_xlsx(input$compound_list_file$datapath, col_names = TRUE)
      # 检查化学式再合并
      compound_list <- cbind(compound_list, check_chemform(isotopes, compound_list$formula))
      # adduct不在列表中的，warning为TRUE
      compound_list$warning <- ifelse(!compound_list$adduct %in% adducts$Name, TRUE, compound_list$warning)
      compound_list
    })

    observeEvent(compound_list(), {
      # adduct不对的，化学式不对的，筛选出来
      if (any(compound_list()$warning)) {
        output$compound_mz_list <- renderDT({
          datatable(subset(compound_list(), warning == TRUE),
            options = list(
              initComplete = JS(
                "function(settings, json) {",
                "$(this.api().table().body()).css({'background-color': '#E6492B'});",
                "}"
              )
            )
          )
        })
      } else {
        observeEvent(input$compound_list_mz_calc, {
          # 按adduct合并
          compound_list <- left_join(compound_list(), adducts, by = c("adduct" = "Name"), keep = TRUE)
          # 乘
          compound_list <- compound_list %>% mutate(formula_calc1 = mapply(function(x, y) {
            if (is.na(y)) {
              NA
            } else {
              multiform(x, y)
            }
          }, new_formula, Multi))
          # 加
          compound_list <- compound_list %>% mutate(formula_calc2 = mapply(function(x, y) {
            if (is.na(y)) {
              NA
            } else if (y == "FALSE") {
              x
            } else {
              mergeform(x, y)
            }
          }, formula_calc1, Formula_add))
          # 减
          compound_list <- compound_list %>% mutate(formula_calc3 = mapply(function(x, y) {
            if (is.na(y)) {
              NA
            } else if (y == "FALSE") {
              x
            } else {
              subform(x, y)
            }
          }, formula_calc2, Formula_ded))
          # 计算adduct_mz
          pattern <- isopattern(
            isotopes,
            compound_list$formula_calc3,
            threshold = 0.01,
            charge = compound_list$Charge,
            emass = 0.00054857990924,
            plotit = FALSE,
            algo = 1
          )
          adduct_mz <- lapply(pattern, function(matrix) {
            # 从矩阵中提取 abundance 列为 100 的行
            subset_matrix <- matrix[matrix[, "abundance"] == 100, ]
            return(subset_matrix)
          })
          compound_mz_list <- cbind(compound_list, adduct_mz = sapply(adduct_mz, function(x) x[1]) %>% unname())

          # 输出表格
          output$compound_mz_list <- renderDT({
            datatable(
              compound_mz_list %>% select(c("compound", "formula", "adduct", "Ion_mode", "adduct_mz"))
            )
          })

          # 下载compound_mz_list
          output$compound_mz_list_download <- downloadHandler(
            filename = function() {
              "compound_mz_list.csv"
            },
            content = function(file) {
              write_csv(compound_mz_list %>% select(c("compound", "formula", "adduct", "Ion_mode", "adduct_mz")), file, na = "")
            }
          )
        })
      }
    })





  })
}
