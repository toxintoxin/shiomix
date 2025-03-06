#' @import shiny
#' @import ggplot2
gglabsUI <- function(id) {
  ns <- NS(id)
  tagList(
    textInput(ns("labs_x"), label = "x轴标题"),
    textInput(ns("labs_y"), label = "y轴标题"),
    textInput(ns("labs_title"), label = "标题"),
    textInput(ns("labs_subtitle"), label = "副标题"),
    textInput(ns("labs_caption"), label = "说明"),
    textInput(ns("labs_tag"), label = "标签")
  )
}

gglabsServer <- function(id, ggobj) {
  moduleServer(id, function(input, output, session){
    p_return <- ggobj + labs(
      title = if(input$labs_title == "") {NULL} else{input$labs_title},
      subtitle = if(input$labs_subtitle == "") {NULL} else{input$labs_subtitle},
      caption = if(input$labs_caption == "") {NULL} else{input$labs_caption},
      tag = if(input$labs_tag == "") {NULL} else{input$labs_tag},
      x = if(input$labs_x == "") {NULL} else{input$labs_x},
      y = if(input$labs_y == "") {NULL} else{input$labs_y}
    )
    return(p_return)
  })
}