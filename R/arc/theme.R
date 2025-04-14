# ggtheme_ui <- function(id) {
#   ns <- NS(id)
#   tagList(
#     layout_columns(
#       selectInput(ns("axis.title.x_family"), label = "x轴标题", choices = c("sans", "serif", "mono")),
#       numericInput(ns("axis.title.x_size"), label = "Font size", value = 11, min = 8, max = 22),
#       checkboxGroupButtons(ns("axis.title.x_face"), label = "Font face", c(`<i class="fa fa-bold"></i>` = "B", `<i class="fa fa-italic"></i>`= "I"), justified = TRUE),
#       colorPickr(ns("axis.title.x_colour"), label = "Color", selected = "#000000"),
#       numericInput(ns("axis.title.x_hjust"), label = "hjust", value = 0.5, min = 0, max = 1, step = 0.5),
#       numericInput(ns("axis.title.x_angle"), label = "angle", value = 0, min = -90, max = 90)
#     ),
#     layout_columns(
#       selectInput(ns("axis.title.y_family"), label = "y轴标题", choices = c("sans", "serif", "mono")),
#       numericInput(ns("axis.title.y_size"), label = "Font size", value = 11, min = 8, max = 22),
#       checkboxGroupButtons(ns("axis.title.y_face"), label = "Font face", c(`<i class="fa fa-bold"></i>` = "B", `<i class="fa fa-italic"></i>`= "I"), justified = TRUE),
#       colorPickr(ns("axis.title.y_colour"), label = "Color", selected = "#000000"),
#       numericInput(ns("axis.title.y_hjust"), label = "hjust", value = 0.5, min = 0, max = 1, step = 0.5),
#       numericInput(ns("axis.title.y_angle"), label = "angle", value = 90, min = -90, max = 90)
#     ),
#     layout_columns(
#       selectInput(ns("plot.title_family"), label = "标题", choices = c("sans", "serif", "mono")),
#       numericInput(ns("plot.title_size"), label = "Font size", value = 13.2, min = 8, max = 22),
#       checkboxGroupButtons(ns("plot.title_face"), label = "Font face", choices = c(`<i class="fa fa-bold"></i>` = "B", `<i class="fa fa-italic"></i>`= "I"), justified = TRUE),
#       colorPickr(ns("plot.title_colour"), label = "Color", selected = "#000000"),
#       numericInput(ns("plot.title_hjust"), label = "hjust", value = 0, min = 0, max = 1, step = 0.5),
#       numericInput(ns("plot.title_angle"), label = "angle", value = 0, min = -90, max = 90)
#     ),
#     layout_columns(
#       selectInput(ns("plot.subtitle_family"), label = "副标题", choices = c("sans", "serif", "mono")),
#       numericInput(ns("plot.subtitle_size"), label = "Font size", value = 11, min = 8, max = 22),
#       checkboxGroupButtons(ns("plot.subtitle_face"), label = "Font face", c(`<i class="fa fa-bold"></i>` = "B", `<i class="fa fa-italic"></i>`= "I"), justified = TRUE),
#       colorPickr(ns("plot.subtitle_colour"), label = "Color", selected = "#000000"),
#       numericInput(ns("plot.subtitle_hjust"), label = "hjust", value = 0, min = 0, max = 1, step = 0.5),
#       numericInput(ns("plot.subtitle_angle"), label = "angle", value = 0, min = -90, max = 90)
#     ),
#     layout_columns(
#       selectInput(ns("plot.caption_family"), label ="说明", choices = c("sans", "serif", "mono")),
#       numericInput(ns("plot.caption_size"), label = "Font size", value = 8.8, min = 8, max = 22),
#       checkboxGroupButtons(ns("plot.caption_face"), label = "Font face", c(`<i class="fa fa-bold"></i>` = "B", `<i class="fa fa-italic"></i>`= "I"), justified = TRUE),
#       colorPickr(ns("plot.caption_colour"), label = "Color", selected = "#000000"),
#       numericInput(ns("plot.caption_hjust"), label = "hjust", value = 1, min = 0, max = 1, step = 0.5),
#       numericInput(ns("plot.caption_angle"), label = "angle", value = 0, min = -90, max = 90)
#     ),
#     layout_columns(
#       selectInput(ns("plot.tag_family"), label = "标签", choices = c("sans", "serif", "mono")),
#       numericInput(ns("plot.tag_size"), label = "Font size", value = 13.2, min = 8, max = 22),
#       checkboxGroupButtons(ns("plot.tag_face"), label = "Font face", c(`<i class="fa fa-bold"></i>` = "B", `<i class="fa fa-italic"></i>`= "I"), justified = TRUE),
#       colorPickr(ns("plot.tag_colour"), label = "Color", selected = "#000000"),
#       numericInput(ns("plot.tag_hjust"), label = "hjust", value = 0.5, min = 0, max = 1, step = 0.5),
#       numericInput(ns("plot.tag_angle"), label = "angle", value = 0, min = -90, max = 90)
#     ),
#     layout_columns(
#       layout_columns(col_widths = c(12, 12),
#         input_switch(ns("panel.grid.major_x"), label = "x主网格", value = FALSE),
#         layout_columns(id = ns("panel.grid.major_x_custom"),
#           selectInput(ns("panel.grid.major.x_linetype"), label = NULL, choices = c("solid", "dashed", "dotted", "dotdash", "longdash", "twodash")),
#           numericInput(ns("panel.grid.major.x_linewidth"), label = NULL, value = 0.5, min = 0.25, max = 2, step = 0.25),
#           colorPickr(ns("panel.grid.major.x_colour"), label = NULL, selected = "#FFFFFF")
#         )
#       ),
#       layout_columns(col_widths = c(12, 12),
#         input_switch(ns("panel.grid.minor_x"), label = "x次网格", value = FALSE),
#         layout_columns(id = ns("panel.grid.minor_x_custom"),
#           selectInput(ns("panel.grid.minor.x_linetype"), label = NULL, choices = c("solid", "dashed", "dotted", "dotdash", "longdash", "twodash")),
#           numericInput(ns("panel.grid.minor.x_linewidth"), label = NULL, value = 0.25, min = 0.25, max = 2, step = 0.25),
#           colorPickr(ns("panel.grid.minor.x_colour"), label = NULL, selected = "#FFFFFF")
#         )
#       )
#     ),
#     layout_columns(
#       layout_columns(col_widths = c(12, 12),
#         input_switch(ns("panel.grid.major_y"), label = "y主网格", value = FALSE),
#         layout_columns(id = ns("panel.grid.major_y_custom"),
#           selectInput(ns("panel.grid.major.y_linetype"), label = NULL, choices = c("solid", "dashed", "dotted", "dotdash", "longdash", "twodash")),
#           numericInput(ns("panel.grid.major.y_linewidth"), label = NULL, value = 0.5, min = 0.25, max = 2, step = 0.25),
#           colorPickr(ns("panel.grid.major.y_colour"), label = NULL, selected = "#FFFFFF")
#         )
#       ),
#       layout_columns(col_widths = c(12, 12),
#         input_switch(ns("panel.grid.minor_y"), label = "y次网格", value = FALSE),
#         layout_columns(id = ns("panel.grid.minor_y_custom"),
#           selectInput(ns("panel.grid.minor.y_linetype"), label = NULL, choices = c("solid", "dashed", "dotted", "dotdash", "longdash", "twodash")),
#           numericInput(ns("panel.grid.minor.y_linewidth"), label = NULL, value = 0.25, min = 0.25, max = 2, step = 0.25),
#           colorPickr(ns("panel.grid.minor.y_colour"), label = NULL, selected = "#FFFFFF")
#         )
#       )
#     ),
#     layout_columns(
#       layout_columns(col_widths = c(12, 4),
#         input_switch(ns("panel.background"), label = "绘图区背景", value = TRUE) %>% tooltip("关闭会使此矢量元素消失，或者你可以简单的设置为白色"),
#         layout_columns(id = ns("panel.background_custom"), colorPickr(ns("panel.background_fill"), label = NULL, selected = "#EBEBEB"))
#       ),
#       layout_columns(col_widths = c(12, 12),
#         input_switch(ns("panel.border_enable"), label = "绘图区边框", value = FALSE),
#         layout_columns(id = ns("panel.border_custom"),
#           selectInput(ns("panel.border_linetype"), label = NULL, choices = c("solid", "dashed", "dotted", "dotdash", "longdash", "twodash")),
#           numericInput(ns("panel.border_linewidth"), label = NULL, value = 0.5, min = 0.1, max = 2, step = 0.1),
#           colorPickr(ns("panel.border_colour"), label = NULL, selected = "#000000")
#         )
#       )
#     ),
#     layout_columns(
#       layout_columns(col_widths = c(12, 4),
#         input_switch(ns("plot.background"), label = "图片背景", value = TRUE) %>% tooltip("关闭会使此矢量元素消失，或者你可以简单的设置为白色"),
#         layout_columns(id = ns("plot.background_custom"), colorPickr(ns("plot.background_fill"), label = NULL, selected = "#FFFFFF"))
#       ),
#       layout_columns(id = ns("plot.background_border"), col_widths = c(12, 12),
#         input_switch(ns("plot.background_border_enable"), label = "图片边框", value = FALSE) %>% tooltip("你必须先打开图片背景"),
#         layout_columns(id = ns("plot.background_border_custom"),
#           selectInput(ns("plot.background_linetype"), label = NULL, choices = c("solid", "dashed", "dotted", "dotdash", "longdash", "twodash")),
#           numericInput(ns("plot.background_linewidth"), label = NULL, value = 0.5, min = 0.1, max = 2, step = 0.1),
#           colorPickr(ns("plot.background_colour"), label = NULL, selected = "#000000")
#         )
#       )
#     )



#   )
# }

# face2 <- function(face) {if ("B" %in% face && "I" %in% face) {return("bold.italic")} else if ("B" %in% face) {return("bold")} else if ("I" %in% face) {return("italic")} else {return("plain")}}

# ggtheme_server <- function(id, ggobj) {
#   moduleServer(id, function(input, output, session) {
#     p_return <- ggobj + theme(

#       axis.title.x = if(input$axis.title.x_size <= 0) {element_blank()} else{element_text(family = input$axis.title.x_family, face = input$axis.title.x_face %>% face2(), colour = input$axis.title.x_colour, size = input$axis.title.x_size, hjust = input$axis.title.x_hjust, angle = input$axis.title.x_angle)},
#       axis.title.y = if(input$axis.title.y_size <= 0) {element_blank()} else{element_text(family = input$axis.title.y_family, face = input$axis.title.y_face %>% face2(), colour = input$axis.title.y_colour, size = input$axis.title.y_size, hjust = input$axis.title.y_hjust, angle = input$axis.title.y_angle)},
#       plot.title = if(input$plot.title_size <= 0) {element_blank()} else{element_text(family = input$plot.title_family, face = input$plot.title_face %>% face2(), colour = input$plot.title_colour, size = input$plot.title_size, hjust = input$plot.title_hjust, angle = input$plot.title_angle)},
#       plot.subtitle = if(input$plot.subtitle_size <= 0) {element_blank()} else{element_text(family = input$plot.subtitle_family, face = input$plot.subtitle_face %>% face2(), colour = input$plot.subtitle_colour, size = input$plot.subtitle_size, hjust = input$plot.subtitle_hjust, angle = input$plot.subtitle_angle)},
#       plot.caption = if(input$plot.caption_size <= 0) {element_blank()} else{element_text(family = input$plot.caption_family, face = input$plot.caption_face %>% face2(), colour = input$plot.caption_colour, size = input$plot.caption_size, hjust = input$plot.caption_hjust, angle = input$plot.caption_angle)},
#       plot.tag = if(input$plot.tag_size <= 0) {element_blank()} else{element_text(family = input$plot.tag_family, face = input$plot.tag_face %>% face2(), colour = input$plot.tag_colour, size = input$plot.tag_size, hjust = input$plot.tag_hjust, angle = input$plot.tag_angle)},

#       panel.grid.major.x = if(input$panel.grid.major_x == FALSE || input$panel.grid.major.x_linewidth <= 0) {element_blank()} else{element_line(colour = input$panel.grid.major.x_colour, linewidth = input$panel.grid.major.x_linewidth, linetype = input$panel.grid.major.x_linetype)},
#       panel.grid.major.y = if(input$panel.grid.major_y == FALSE || input$panel.grid.major.y_linewidth <= 0) {element_blank()} else{element_line(colour = input$panel.grid.major.y_colour, linewidth = input$panel.grid.major.y_linewidth, linetype = input$panel.grid.major.y_linetype)},
#       panel.grid.minor.x = if(input$panel.grid.minor_x == FALSE || input$panel.grid.minor.x_linewidth <= 0) {element_blank()} else{element_line(colour = input$panel.grid.minor.x_colour, linewidth = input$panel.grid.minor.x_linewidth, linetype = input$panel.grid.minor.x_linetype)},
#       panel.grid.minor.y = if(input$panel.grid.minor_y == FALSE || input$panel.grid.minor.y_linewidth <= 0) {element_blank()} else{element_line(colour = input$panel.grid.minor.y_colour, linewidth = input$panel.grid.minor.y_linewidth, linetype = input$panel.grid.minor.y_linetype)},

#       panel.background = if(input$panel.background == FALSE) {element_blank()} else{element_rect(fill = input$panel.background_fill)},
#       panel.border = if(input$panel.border_enable == FALSE || input$panel.border_linewidth <= 0) {element_blank()} else{element_rect(fill = NA, colour = input$panel.border_colour, linewidth = input$panel.border_linewidth, linetype = input$panel.border_linetype)},
#       plot.background = if(input$plot.background == FALSE) {element_blank()} else{if(input$plot.background_border_enable == FALSE || input$plot.background_linewidth <= 0) {element_rect(fill = input$plot.background_fill, colour = "NA")} else{element_rect(fill = input$plot.background_fill, colour = input$plot.background_colour, linewidth = input$plot.background_linewidth, linetype = input$plot.background_linetype)}},
#       # legend.background = if(input$legend.background == FALSE) {element_blank()} else{if(input$legend.background_border_enable == FALSE || input$legend.background_linewidth <= 0) {element_rect(fill = input$legend.background_fill)} else{element_rect(fill = input$legend.background_fill, colour = input$legend.background_colour, linewidth = input$legend.background_linewidth, linetype = input$legend.background_linetype)}},
#       # legend.key = if(input$legend.key == FALSE) {element_blank()} else{if(input$legend.key_border_enable == FALSE || input$legend.key_linewidth <= 0) {element_rect(fill = input$legend.key_fill)} else{element_rect(fill = input$legend.key_fill, colour = input$legend.key_colour, linewidth = input$legend.key_linewidth, linetype = input$legend.key_linetype)}},
#     ) # +
#     # theme(
#     #   axis.line.x = if(input$axis_line_x == FALSE) {element_blank()} else{element_line(colour = input$axis.line.x_colour, linetype = input$axis.line.x_linetype, arrow = if(input$axis_line_x_arrow == FALSE || input$axis.line.x_arrow_length == 0) {FALSE} else{arrow(angle = input$axis.line.x_arrow_angle, length = unit(input$axis.line.x_arrow_length, "mm"))})},
#     #   # axis.line.y = if(input$axis_line_y == FALSE) {element_blank()} else{element_line(colour = input$axis.line.y_colour, linetype = input$axis.line.y_linetype, arrow = if(input$axis_line_y_arrow == FALSE || input$axis.line.y_arrow_length == 0) {FALSE} else{arrow(angle = input$axis.line.y_arrow_angle, length = unit(input$axis.line.y_arrow_length, "mm"))})},
#     #   # axis.text.x = if(input$axis_text_x == FALSE || input$axis.text.x_size <= 0) {element_blank()} else{element_text(family = input$axis.text.x_family, face = input$axis.text.x_face %>% face2(), colour = input$axis.text.x_colour, size = input$axis.text.x_size, angle = input$axis.text.x_angle)},
#     #   # axis.text.y = if(input$axis_text_y == FALSE || input$axis.text.y_size <= 0) {element_blank()} else{element_text(family = input$axis.text.y_family, face = input$axis.text.y_face %>% face2(), colour = input$axis.text.y_colour, size = input$axis.text.y_size, angle = input$axis.text.y_angle)},
#     #   # axis.ticks.x = if(input$axis_ticks_x == FALSE || input$axis.ticks.x_linewidth <= 0 || input$axis.ticks.length.x_length == 0) {element_blank()} else{element_line(colour = input$axis.ticks.x_colour, linewidth = input$axis.ticks.x_linewidth)},
#     #   # axis.ticks.y = if(input$axis_ticks_y == FALSE || input$axis.ticks.y_linewidth <= 0 || input$axis.ticks.length.y_length == 0) {element_blank()} else{element_line(colour = input$axis.ticks.y_colour, linewidth = input$axis.ticks.y_linewidth)}
#     # )
#     # if(input$axis_ticks_x == TRUE && input$axis.ticks.x_linewidth > 0 && input$axis.ticks.length.x_length != 0) {p_return <- p_return + theme(axis.ticks.length.x = unit(input$axis.ticks.length.x_length, "mm"))} else { }
#     # if(input$axis_ticks_y == TRUE && input$axis.ticks.y_linewidth > 0 && input$axis.ticks.length.y_length != 0) {p_return <- p_return + theme(axis.ticks.length.y = unit(input$axis.ticks.length.y_length, "mm"))} else { }

#     return(p_return)
#   })
# }