formula_calcUI <- function(id) {
  ns <- NS(id)
  tagList(
    layout_columns(col_widths = c(5, 2, 2, 3),
      fileInput(ns("compound_list_file"), "上传化合物列表", accept = ".xlsx"),
      actionButton(ns("compound_list_mz_calc"), "计算adduct"),
      downloadButton(ns("compound_list_template"), "下载模板"),
      downloadButton(ns("compound_mz_list_download"), "下载结果", icon = icon("file-export"))
    ),
    DTOutput(ns("compound_mz_list"))
  )
}

formula_calcServer <- function(id) {
  moduleServer(id, function(input, output, session) {

    ns <- session$ns

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
        write_xlsx(compound_list_template, file, format_headers = FALSE)
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