md5UI <- function(id) {
  ns <- NS(id)
  tagList(
    actionButton(ns("readme"), "教程"),
    fluidRow(
      column(width = 5,
        fileInput(ns("file_tobecheck"), "上传需要检验的md5列表", accept = ".xlsx")
      ),
      column(width = 5,
        fileInput(ns("file_true"), "上传md5sum", accept = ".md5")
      ),
      column(width = 2, style = "margin-top: 25px;",
        actionButton(ns("check"), "开始检验")
      )
    ),
    fluidRow(
      DTOutput(ns("result"))
    )
  )
}

md5Server <- function(id) {
  moduleServer(id, function(input, output, session) {

    ns <- session$ns

    # mac MD5 check guide
    observeEvent(input$readme, {
      showModal(modalDialog(
        "打开终端Terminal", tags$br(),
        "输入find ./|xargs md5 (将.替换成你需要检查的目录)",
        footer = modalButton("关闭")
      ))
    })

    # 读取文件的路径及其MD5值
    md5_tobecheck <- reactive({
      req(input$file_tobecheck)
      read_xlsx(input$file_tobecheck$datapath, col_names = FALSE)
    })

    # 读取md5sum
    md5_true <- reactive({
      req(input$file_true)
      read.table(input$file_true$datapath, header = FALSE, stringsAsFactors = FALSE, col.names = c("md5_true", "filename"))
    })

    # 检验
    observeEvent(input$check, {
      md5_tobecheck <- separate(md5_tobecheck(), 1, into = c("md5_tobecheck", "filepath"), sep = "\\s")
      md5_tobecheck$filename <- sapply(strsplit(md5_tobecheck$filepath, "/"), function(x) tail(x, n = 1))

      check <- left_join(md5_tobecheck, md5_true(), by = "filename")
      check$result <- ifelse(check$md5_tobecheck == check$md5_true, 1, 0)

      output$result <- renderDT({
        datatable(
          check,
          options = list(pageLength = -1, dom = "frtip")
        ) %>% formatStyle("result", backgroundColor = styleEqual(c(1, 0), c("#00ff00", "#ff0000")))
      })
    })



  })
}
