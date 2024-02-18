gglabsUI <- function(type) {
  ns <- NS(type)
  div(
    textInput(ns("labs_x"), label = "x轴标题"),
    textInput(ns("labs_y"), label = "y轴标题"),
    textInput(ns("labs_title"), label = "标题"),
    textInput(ns("labs_subtitle"), label = "副标题"),
    textInput(ns("labs_caption"), label = "说明"),
    textInput(ns("labs_tag"), label = "标签")
  )
}


labs_custom <- function(labs_title, labs_subtitle, labs_caption, labs_tag, labs_x, labs_y) {
  labs_custom <- labs(
    title = if(labs_title == "") {NULL} else{labs_title},
    subtitle = if(labs_subtitle == "") {NULL} else{labs_subtitle},
    caption = if(labs_caption == "") {NULL} else{labs_caption},
    tag = if(labs_tag == "") {NULL} else{labs_tag},
    x = if(labs_x == "") {NULL} else{labs_x},
    y = if(labs_y == "") {NULL} else{labs_y}
  )
}