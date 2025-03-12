ttestUI <- function(id) {
  ns <- NS(id)
  layout_sidebar(
    sidebar = sidebar(
      excelInput(ns("data"), header = "Data"),
      radioButtons(ns("pheno_source"), label = "Description", choices = c("From sample's name", "From table")),
      conditionalPanel(ns = ns, condition = "input.pheno_source == 'From table'",
        excelInput(ns("phenoData"), header = "phenoData")
      ),
      actionButton(ns("check"), label = "Check"),
      selectInput(ns("group1"), label = "Group 1", choices = c(Choose = "")),
      selectInput(ns("group2"), label = "Group 2", choices = c(Choose = "")),
      selectInput(ns("paired"), label = "Paired", choices = c(Choose = "", "FALSE" = FALSE)),
      selectInput(ns("var.equal"), label = "Variance", choices = c(Choose = "", "Equal" = TRUE)),
      selectInput(ns("p.adjust.method"), label = "Method of p.adjust", choices = c(Choose = "", "BH")),
      actionButton(ns("handle"), label = "Handle")
    ),
    downloadButton(ns("export_xlsx"), label = "xlsx"),
    DTOutput(ns("DT"))
  )
}

ttestServer <- function(id) {
  moduleServer(id, function(input, output, session) {

    ns <- session$ns

    rv <- reactiveValues()

    data_ls <- excelServer("data")

    observe({
      rv$data_original <- data_ls$data()
      rv$name <- data_ls$name()
    })

    phenoData_ls <- excelServer("phenoData")

    observe({
      rv$phenoData <- phenoData_ls$data()
    })

    observeEvent(input$check, {
      if (input$pheno_source == "From table") {
        rv$data_long <- rv$data_original %>%
          rename("var" = 1) %>%
          pivot_longer(-1, names_to = "sample", values_to = "value") %>%
          left_join(rv$phenoData, by = "sample")
      } else {
        rv$data_long <- rv$data_original %>%
          rename("var" = 1) %>%
          pivot_longer(-1, names_to = "sample", values_to = "value") %>%
          mutate(group = str_replace(sample, "^(.*)_.*$", "\\1"))
      }
      updateSelectInput(session, "group1", choices = c(Choose = "", unique(rv$data_long$group)))
      updateSelectInput(session, "group2", choices = c(Choose = "", unique(rv$data_long$group)))
    })

    observeEvent(input$handle, {

      rv$data_long <- rv$data_long %>%
        filter(group %in% c(input$group1, input$group2))
print(head(rv$data_long %>% pivot_wider(names_from = "sample", values_from = "value"), 100))
      # base
      ttest <- rv$data_long %>%
        mutate(var = fct_inorder(var)) %>%
        group_by(var) %>%
        summarise(out = tidy(t.test(value ~ group, paired = as.logical(input$paired), var.equal = as.logical(input$var.equal)))) %>%
        ungroup() %>%
        unnest(out) %>%
        select(c("var", "statistic", "estimate1", "estimate2", "p.value")) %>%
        rename("mean.1" = "estimate1", "mean.2" = "estimate2")

      ttest_p_adj <- ttest %>%
        mutate(p.adj = p.adjust(p.value, method = input$p.adjust.method))

      rv$result <- ttest_p_adj
    })



    output$export_xlsx <- downloadHandler(
      filename = function() {
        paste0(rv$name, "_ttest", ".xlsx")
      },
      content = function(file) {
        list_of_datasets <- list(
          data = rv$data_original,
          result = rv$result,
          phenoData = rv$phenoData
        )
        write_xlsx(list_of_datasets, file, format_headers = FALSE)
      }
    )

    output$DT <- renderDT({
      req(rv$result)
      datatable(rv$result)
    })

  })
}