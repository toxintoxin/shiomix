#' @import shiny

customRadioButtons <- function(inputId, label, choices, separators = NULL, selected = NULL, inline = FALSE, width = NULL) {
  # 生成单选按钮
  options <- mapply(function(value, name) {
    tags$div(
      class = "radio",
      tags$label(
        tags$input(type = "radio", name = inputId, value = value, checked = if (value == selected) "checked"),
        tags$span(name)
      )
    )
  }, names(choices), choices, SIMPLIFY = FALSE, USE.NAMES = FALSE)
  
  # 插入分隔符
  if (!is.null(separators)) {
    options <- unlist(lapply(seq_along(options), function(i) {
      if (i == 1 && length(separators) >= i) {
        # 在第一个选项之前插入分隔符
        list(tags$div(class = "separator", separators[i]), options[[i]])
      } else if (i <= length(separators)) {
        # 在其他选项之间插入分隔符
        list(options[[i]], tags$div(class = "separator", separators[i]))
      } else {
        list(options[[i]])
      }
    }), recursive = FALSE)
  }
  
  # 构建最终的HTML
  tags$div(
    id = inputId,
    class = "form-group shiny-input-radiogroup shiny-input-container shinyjs-resettable",
    role = "radiogroup",
    `aria-labelledby` = paste0(inputId, "-label"),
    tags$label(class = "control-label", id = paste0(inputId, "-label"), label),
    tags$div(class = "shiny-options-group",
      tag$div(class = "radio", HTML('<label><input type="radio" name="radio_my" value="A" checked="checked"><span>Option 1</span></label>'))
    )
  )
}



ui <- fillPage(
  # tags$div(HTML('<div id="radio_my" class="form-group shiny-input-radiogroup shiny-input-container shiny-bound-input" role="radiogroup" aria-labelledby="radio_my-label">
  #   <label class="control-label" id="radio_my-label">Radio buttons my</label>
  #   <div class="shiny-options-group">
  #     <div class="radio">
  #       <label>
  #         <input type="radio" name="radio_my" value="A" checked="checked">
  #         <span>Option 1</span>
  #       </label>
  #     </div>
  #     <div class="separator">Separator 1</div>
  #     <div class="radio">
  #       <label>
  #         <input type="radio" name="radio_my" value="B">
  #         <span>Option 2</span>
  #       </label>
  #     </div>
  #     <div class="separator">Separator 2</div>
  #     <div class="radio">
  #       <label>
  #         <input type="radio" name="radio_my" value="C">
  #         <span>Option 3</span>
  #       </label>
  #     </div>
  #   </div>
  # </div>')),
  customRadioButtons(
    inputId = "radio_my",
    label = "my",
    choices = c("A" = "Option 1", "B" = "Option 2", "C" = "Option 3"),
    selected = "A"
  ),
  textOutput("selectedOption")

)

server <- function(input, output, session) {
  output$selectedOption <- renderText({
    paste("You selected:", input$radio_my)
  })
}

shinyApp(ui = ui, server = server)