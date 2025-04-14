#' @import shiny
#' @import readr
#' @import readxl
#' @import stringr

excelInput <- function(id, header) {
  ns <- NS(id)
  tagList(
    shinyjs::useShinyjs(),
    tags$b(header),
    fileInput(ns("file"), label = ".csv or .xlsx", accept = c(".csv", ".xlsx")),
    uiOutput(ns("sheet_ui")),
    bslib::input_task_button(ns("read"), label = "Read")
    # actionButton(ns("read"), label = "Read")
  )
}

excel_server <- function(id, na = c("", "NA")) {
  moduleServer(id, function(input, output, session) {

    ns <- session$ns

    ext <- eventReactive(input$file, {
      tools::file_ext(input$file$name)
    })

    output$sheet_ui <- renderUI({
      req(ext())
      if (ext() == "xlsx") {
        selectInput(ns("sheet"), "Sheet", choices = excel_sheets(input$file$datapath))
      }
    })

    Data <- reactiveVal()
    Name <- reactiveVal()

    observe({shinyjs::toggleState("read", condition = !is.null(input$file))})

    observeEvent(input$read, {
      if (ext() == "csv") {
        df <- read_csv(input$file$datapath, na = na) %>% as.data.frame()
        Data(df)
        Name(str_extract(input$file$name, ".*(?=\\.)"))
      } else if (ext() == "xlsx") {
        df <- read_xlsx(input$file$datapath, input$sheet, na = na) %>% as.data.frame()
        Data(df)
        Name(paste0(str_extract(input$file$name, ".*(?=\\.)"), "_#", input$sheet, "#"))
      }
    })

    return(list(data = Data, name = Name))

  })
}























friendlyAct <- function(id, label, icon = NULL, class = "btn-default") {
  ns <- NS(id)
  actionButton(id, label, icon = icon, class = class)
}

friendlyAct_server <- function(id, expr) {
  moduleServer(NULL, function(input, output, session) {
    ns <- session$ns
    observeEvent(input[[id]], {
      tryCatch(
        withCallingHandlers(
          expr = expr,
          message = function(m) showNotification(m$message, type = "message"),
          warning = function(w) showNotification(w$message, type = "warning")
        ),
        error = function(e) showNotification(e$message, type = "error")
      )
    })
  })
}


figure <- function(img_src, data_value, ns = NS(NULL)) {
  id <- tools::file_path_sans_ext(basename(img_src))
  div(
    class = "figure",
    id = ns(id),
    "data-value" = data_value,
    img(src = img_src),
    span(data_value)
  )
}

# https://github.com/daattali/advanced-shiny/blob/master/busy-indicator/helpers.R
withBusyIndicatorCSS <- "
.btn-loading-container {
display: inline-block;  /* 留出固定宽度*/
width: 20px;  /* 留出固定宽度*/
font-size: 1.2em;
}
.btn-done-indicator {
color: green;
}
.btn-err {
margin-top: 10px;
color: red;
}


.panel-new-failed .panel-heading::after {
  content: '';
  width: 16px;
  height: 16px;
  border-radius: 50%;
  background-color: #ff0000;
}
"


withBusyIndicator_ui <- function(button) {
  id <- button[['attribs']][['id']]
  div(
    shinyjs::useShinyjs(),
    singleton(tags$head(
      tags$style(withBusyIndicatorCSS)
    )),
    `data-for-btn` = id,
    button,
    span(
      class = "btn-loading-container",
      shinyjs::hidden(
        icon("spinner", class = "btn-loading-indicator fa-spin"),
        icon("check", class = "btn-done-indicator")
      )
    ),
    shinyjs::hidden(
      div(class = "btn-err",
        div(icon("exclamation-circle"),
          tags$b("Error: "),
          span(class = "btn-err-msg")
        )
      )
    )
  )
}

# Call this function from the server with the button id that is clicked and the
# expression to run when the button is clicked
withBusyIndicator_server <- function(buttonId, expr) {
  # UX stuff: show the "busy" message, hide the other messages, disable the button
  loadingEl <- sprintf("[data-for-btn=%s] .btn-loading-indicator", buttonId)
  doneEl <- sprintf("[data-for-btn=%s] .btn-done-indicator", buttonId)
  errEl <- sprintf("[data-for-btn=%s] .btn-err", buttonId)
  shinyjs::disable(buttonId)
  shinyjs::show(selector = loadingEl)
  shinyjs::hide(selector = doneEl)
  shinyjs::hide(selector = errEl)
  on.exit({
    shinyjs::enable(buttonId)
    shinyjs::hide(selector = loadingEl)
  })
  
  # Try to run the code when the button is clicked and show an error message if
  # an error occurs or a success message if it completes
  tryCatch({
    value <- expr
    shinyjs::show(selector = doneEl)
    shinyjs::delay(2000, shinyjs::hide(selector = doneEl, anim = TRUE, animType = "fade", time = 0.5))
    value
  }, error = function(err) { errorFunc(err, buttonId) })
}

# When an error happens after a button click, show the error
errorFunc <- function(err, buttonId) {
  errEl <- sprintf("[data-for-btn=%s] .btn-err", buttonId)
  errElMsg <- sprintf("[data-for-btn=%s] .btn-err-msg", buttonId)
  errMessage <- gsub("^ddpcr: (.*)", "\\1", err$message)
  shinyjs::html(html = errMessage, selector = errElMsg)
  shinyjs::show(selector = errEl, anim = TRUE, animType = "fade")
}



# 更新指定bsCollapse中某个panel的style
withBusyIndicator_serverUpdatecollapse <- function(buttonId, bsCollapseId, panel, session, expr) {
  # UX stuff: show the "busy" message, hide the other messages, disable the button
  loadingEl <- sprintf("[data-for-btn=%s] .btn-loading-indicator", buttonId)
  doneEl <- sprintf("[data-for-btn=%s] .btn-done-indicator", buttonId)
  errEl <- sprintf("[data-for-btn=%s] .btn-err", buttonId)
  panelSelector <- sprintf("#%s [value='%s']", bsCollapseId, panel)
  success <- list(x = "success")
  names(success) <- panel
  shinyjs::disable(buttonId)
  shinyjs::show(selector = loadingEl)
  shinyjs::hide(selector = doneEl)
  shinyjs::hide(selector = errEl)
  on.exit({
    shinyjs::enable(buttonId)
    shinyjs::hide(selector = loadingEl)
  })
  
  # Try to run the code when the button is clicked and show an error message if
  # an error occurs or a success message if it completes
  tryCatch({
    value <- expr
    shinyjs::show(selector = doneEl)
    shinyjs::delay(2000, shinyjs::hide(selector = doneEl, anim = TRUE, animType = "fade", time = 0.5))
    shinyjs::removeClass(selector = panelSelector, class = "panel-new-failed")
    updateCollapse(session, bsCollapseId, style = success)
    value
  }, error = function(err) {
    errorFunc(err, buttonId)
    shinyjs::addClass(selector = panelSelector, class = "panel-new-failed")
  })
}