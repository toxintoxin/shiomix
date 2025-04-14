formula_calc_single_ui <- function(id) {
  ns <- NS(id)
  tagList(
    layout_columns(col_widths = c(5, 2, 2),
      textInput(ns("compound_formula"), "Formula 检查大小写是必要的"),
      pickerInput(ns("adduct_select"), "Adduct", choices = adducts$Name, choicesOpt = list(icon = c(rep("glyphicon-minus", 18), rep("glyphicon-plus", 34))), selected = "M+H", options = list(`live-search` = TRUE)),
      actionButton(ns("compound_details"), "View the Details")
    ),
    layout_columns(col_widths = c(6, 6),
      verbatimTextOutput(ns("adduct_mz")),
      plotOutput(ns("adduct_pattern"))
    )
  )
}

formula_calc_single_server <- function(id) {
  moduleServer(id, function(input, output, session) {

    ns <- session$ns

    # 单个的
    compound_formula <- reactive({
      # 检查化学式
      compound_formula_checked <- check_chemform(isotopes, input$compound_formula)[1, 2]
      ## adduct不在列表中的或者warning为TRUE的报警
      # compound_list$warning <- ifelse(!compound_list$adduct %in% adducts$Name, TRUE, compound_list$warning)
      return(compound_formula_checked)
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
      output$adduct_pattern <- renderPlot({
        ggplot(pattern[[1]], aes(x = `m/z`, y = abundance)) + geom_segment(aes(xend = `m/z`, yend = 0)) + geom_text(aes(label = `m/z`))
      })
      # adduct_mz <- lapply(pattern, function(matrix) {
      #       # 从矩阵中提取 abundance 列为 100 的行
      #       subset_matrix <- matrix[matrix[, "abundance"] == 100, ]
      #       return(subset_matrix)
      #     })
      # compound_mz_list <- cbind(compound_list, adduct_mz = sapply(adduct_mz, function(x) x[1]) %>% unname())
    })

  })
}