ggthemeUI <- function(type) {
  ns <- NS(type)
  div(
    div(style = "display: flex; align-items: flex-end; gap: 1%;",
      div(style = "width: 16%;", selectInput(ns("axis.title.x_family"), label = "x轴标题", choices = c("sans", "serif", "mono"))),
      div(style = "width: 11%;", numericInput(ns("axis.title.x_size"), label = NULL, value = 11, min = 8, max = 22)),
      div(style = "width: 11%;", checkboxGroupButtons(ns("axis.title.x_face"), label = NULL, c(`<i class="fa fa-bold"></i>` = "B", `<i class="fa fa-italic"></i>`= "I"), justified = TRUE)),
      div(style = "width: 14%;", colorPickr(ns("axis.title.x_colour"), label = NULL, selected = "#000000")),
      div(style = "width: 11%;", numericInput(ns("axis.title.x_hjust"), label = "hjust", value = 0.5, min = 0, max = 1, step = 0.5)),
      div(style = "width: 11%;", numericInput(ns("axis.title.x_angle"), label = "angle", value = 0, min = -90, max = 90))
    ),
    div(style = "display: flex; align-items: flex-end; gap: 1%;",
      div(style = "width: 16%;", selectInput(ns("axis.title.y_family"), label = "y轴标题", choices = c("sans", "serif", "mono"))),
      div(style = "width: 11%;", numericInput(ns("axis.title.y_size"), label = NULL, value = 11, min = 8, max = 22)),
      div(style = "width: 11%;", checkboxGroupButtons(ns("axis.title.y_face"), label = NULL, c(`<i class="fa fa-bold"></i>` = "B", `<i class="fa fa-italic"></i>`= "I"), justified = TRUE)),
      div(style = "width: 14%;", colorPickr(ns("axis.title.y_colour"), label = NULL, selected = "#000000")),
      div(style = "width: 11%;", numericInput(ns("axis.title.y_hjust"), label = "hjust", value = 0.5, min = 0, max = 1, step = 0.5)),
      div(style = "width: 11%;", numericInput(ns("axis.title.y_angle"), label = "angle", value = 90, min = -90, max = 90))
    ),
    div(style = "display: flex; align-items: flex-end; gap: 1%;",
      div(style = "width: 16%;", selectInput(ns("plot.title_family"), label = "标题", choices = c("sans", "serif", "mono"))),
      div(style = "width: 11%;", numericInput(ns("plot.title_size"), label = NULL, value = 13.2, min = 8, max = 22)),
      div(style = "width: 11%;", checkboxGroupButtons(ns("plot.title_face"), label = NULL, choices = c(`<i class="fa fa-bold"></i>` = "B", `<i class="fa fa-italic"></i>`= "I"), justified = TRUE)),
      div(style = "width: 14%;", colorPickr(ns("plot.title_colour"), label = NULL, selected = "#000000")),
      div(style = "width: 11%;", numericInput(ns("plot.title_hjust"), label = "hjust", value = 0, min = 0, max = 1, step = 0.5)),
      div(style = "width: 11%;", numericInput(ns("plot.title_angle"), label = "angle", value = 0, min = -90, max = 90))
    ),
    div(style = "display: flex; align-items: flex-end; gap: 1%;",
      div(style = "width: 16%;", selectInput(ns("plot.subtitle_family"), label = "副标题", choices = c("sans", "serif", "mono"))),
      div(style = "width: 11%;", numericInput(ns("plot.subtitle_size"), label = NULL, value = 11, min = 8, max = 22)),
      div(style = "width: 11%;", checkboxGroupButtons(ns("plot.subtitle_face"), label = NULL, c(`<i class="fa fa-bold"></i>` = "B", `<i class="fa fa-italic"></i>`= "I"), justified = TRUE)),
      div(style = "width: 14%;", colorPickr(ns("plot.subtitle_colour"), label = NULL, selected = "#000000")),
      div(style = "width: 11%;", numericInput(ns("plot.subtitle_hjust"), label = "hjust", value = 0, min = 0, max = 1, step = 0.5)),
      div(style = "width: 11%;", numericInput(ns("plot.subtitle_angle"), label = "angle", value = 0, min = -90, max = 90))
    ),
    div(style = "display: flex; align-items: flex-end; gap: 1%;",
      div(style = "width: 16%;", selectInput(ns("plot.caption_family"), label ="说明", choices = c("sans", "serif", "mono"))),
      div(style = "width: 11%;", numericInput(ns("plot.caption_size"), label = NULL, value = 8.8, min = 8, max = 22)),
      div(style = "width: 11%;", checkboxGroupButtons(ns("plot.caption_face"), label = NULL, c(`<i class="fa fa-bold"></i>` = "B", `<i class="fa fa-italic"></i>`= "I"), justified = TRUE)),
      div(style = "width: 14%;", colorPickr(ns("plot.caption_colour"), label = NULL, selected = "#000000")),
      div(style = "width: 11%;", numericInput(ns("plot.caption_hjust"), label = "hjust", value = 1, min = 0, max = 1, step = 0.5)),
      div(style = "width: 11%;", numericInput(ns("plot.caption_angle"), label = "angle", value = 0, min = -90, max = 90))
    ),
    div(style = "display: flex; align-items: flex-end; gap: 1%;",
      div(style = "width: 16%;", selectInput(ns("plot.tag_family"), label = "标签", choices = c("sans", "serif", "mono"))),
      div(style = "width: 11%;", numericInput(ns("plot.tag_size"), label = NULL, value = 13.2, min = 8, max = 22)),
      div(style = "width: 11%;", checkboxGroupButtons(ns("plot.tag_face"), label = NULL, c(`<i class="fa fa-bold"></i>` = "B", `<i class="fa fa-italic"></i>`= "I"), justified = TRUE)),
      div(style = "width: 14%;", colorPickr(ns("plot.tag_colour"), label = NULL, selected = "#000000")),
      div(style = "width: 11%;", numericInput(ns("plot.tag_hjust"), label = "hjust", value = 0.5, min = 0, max = 1, step = 0.5)),
      div(style = "width: 11%;", numericInput(ns("plot.tag_angle"), label = "angle", value = 0, min = -90, max = 90))
    ),
    div(style = "display: flex;",
      div(style = "width: 50%;",
        div(style = "width: 50%; height: 25px;", input_switch(ns("panel.grid.major_x"), label = "x主网格", value = FALSE)),
        div(style = "display: flex; gap: 2%", id = ns("panel.grid.major_x_custom"),
          div(style = "width: 32%;", selectInput(ns("panel.grid.major.x_linetype"), label = NULL, choices = c("solid", "dashed", "dotted", "dotdash", "longdash", "twodash"))),
          div(style = "width: 22%;", numericInput(ns("panel.grid.major.x_linewidth"), label = NULL, value = 0.5, min = 0.25, max = 2, step = 0.25)),
          div(style = "width: 28%;", colorPickr(ns("panel.grid.major.x_colour"), label = NULL, selected = "#EBEBEB"))
        )
      ),
      div(style = "width: 50%;",
        div(style = "width: 50%; height: 25px;", input_switch(ns("panel.grid.minor_x"), label = "x次网格", value = FALSE)),
        div(style = "display: flex; gap: 2%", id = ns("panel.grid.minor_x_custom"),
          div(style = "width: 32%;", selectInput(ns("panel.grid.minor.x_linetype"), label = NULL, choices = c("solid", "dashed", "dotted", "dotdash", "longdash", "twodash"))),
          div(style = "width: 22%;", numericInput(ns("panel.grid.minor.x_linewidth"), label = NULL, value = 0.25, min = 0.25, max = 2, step = 0.25)),
          div(style = "width: 28%;", colorPickr(ns("panel.grid.minor.x_colour"), label = NULL, selected = "#EBEBEB"))
        )
      )
    ),
    div(style = "display: flex;",
      div(style = "width: 50%;",
        div(style = "width: 50%; height: 25px;", input_switch(ns("panel.grid.major_y"), label = "y主网格", value = FALSE)),
        div(style = "display: flex; gap: 2%", id = ns("panel.grid.major_y_custom"),
          div(style = "width: 32%;", selectInput(ns("panel.grid.major.y_linetype"), label = NULL, choices = c("solid", "dashed", "dotted", "dotdash", "longdash", "twodash"))),
          div(style = "width: 22%;", numericInput(ns("panel.grid.major.y_linewidth"), label = NULL, value = 0.5, min = 0.25, max = 2, step = 0.25)),
          div(style = "width: 28%;", colorPickr(ns("panel.grid.major.y_colour"), label = NULL, selected = "#EBEBEB"))
        )
      ),
      div(style = "width: 50%;",
        div(style = "width: 50%; height: 25px;", input_switch(ns("panel.grid.minor_y"), label = "y次网格", value = FALSE)),
        div(style = "display: flex; gap: 2%", id = ns("panel.grid.minor_y_custom"),
          div(style = "width: 32%;", selectInput(ns("panel.grid.minor.y_linetype"), label = NULL, choices = c("solid", "dashed", "dotted", "dotdash", "longdash", "twodash"))),
          div(style = "width: 22%;", numericInput(ns("panel.grid.minor.y_linewidth"), label = NULL, value = 0.25, min = 0.25, max = 2, step = 0.25)),
          div(style = "width: 28%;", colorPickr(ns("panel.grid.minor.y_colour"), label = NULL, selected = "#EBEBEB"))
        )
      )
    ),
    div(style = "display: flex;",
      div(style = "width: 20%;",
        div(style = "height: 25px;", input_switch(ns("panel.background"), label = "绘图区背景", value = FALSE) %>% tooltip("现在是透明的，打开后会产生额外的矢量元素供你选择填充颜色")),
        div(style = "width: 70%;", id = ns("panel.background_custom"), colorPickr(ns("panel.background_fill"), label = NULL, selected = "#FFFFFF"))
      ),
      div(style = "width: 80%;",
        div(style = "height: 25px;", input_switch(ns("panel.border_enable"), label = "绘图区边框", value = FALSE)),
        div(style = "display: flex; gap: 1.25%", id = ns("panel.border_custom"),
          div(style = "width: 20%;", selectInput(ns("panel.border_linetype"), label = NULL, choices = c("solid", "dashed", "dotted", "dotdash", "longdash", "twodash"))),
          div(style = "width: 13.75%;", numericInput(ns("panel.border_linewidth"), label = NULL, value = 0.5, min = 0.1, max = 2, step = 0.1)),
          div(style = "width: 17.5%;", colorPickr(ns("panel.border_colour"), label = NULL, selected = "#000000"))
        )
      )
    ),
    div(style = "display: flex;",
      div(style = "width: 20%;",
        div(style = "height: 25px;", input_switch(ns("plot.background"), label = "图片背景", value = FALSE) %>% tooltip("现在是透明的，打开后会产生额外的矢量元素供你选择填充颜色")),
        div(style = "width: 70%;", id = ns("plot.background_custom"), colorPickr(ns("plot.background_fill"), label = NULL, selected = "#FFFFFF"))
      ),
      div(style = "width: 80%;", id = ns("plot.background_border"),
        div(style = "height: 25px; width: 25%;", input_switch(ns("plot.background_border_enable"), label = "图片边框", value = FALSE) %>% tooltip("你必须先打开图片背景")),
        div(style = "display: flex; gap: 1.25%", id = ns("plot.background_border_custom"),
          div(style = "width: 20%;", selectInput(ns("plot.background_linetype"), label = NULL, choices = c("solid", "dashed", "dotted", "dotdash", "longdash", "twodash"))),
          div(style = "width: 13.75%;", numericInput(ns("plot.background_linewidth"), label = NULL, value = 0.5, min = 0.1, max = 2, step = 0.1)),
          div(style = "width: 17.5%;", colorPickr(ns("plot.background_colour"), label = NULL, selected = "#000000"))
        )
      )
    )





  )
}


theme_custom <- function(
  axis.title.x_family, axis.title.x_face, axis.title.x_colour, axis.title.x_size, axis.title.x_hjust, axis.title.x_angle,
  axis.title.y_family, axis.title.y_face, axis.title.y_colour, axis.title.y_size, axis.title.y_hjust, axis.title.y_angle,
  plot.title_family, plot.title_face, plot.title_colour, plot.title_size, plot.title_hjust, plot.title_angle,
  plot.subtitle_family, plot.subtitle_face, plot.subtitle_colour, plot.subtitle_size, plot.subtitle_hjust, plot.subtitle_angle,
  plot.caption_family, plot.caption_face, plot.caption_colour, plot.caption_size, plot.caption_hjust, plot.caption_angle,
  plot.tag_family, plot.tag_face, plot.tag_colour, plot.tag_size, plot.tag_hjust, plot.tag_angle,
  panel.grid.major_x, panel.grid.major.x_colour, panel.grid.major.x_linewidth, panel.grid.major.x_linetype,
  panel.grid.major_y, panel.grid.major.y_colour, panel.grid.major.y_linewidth, panel.grid.major.y_linetype,
  panel.grid.minor_x, panel.grid.minor.x_colour, panel.grid.minor.x_linewidth, panel.grid.minor.x_linetype,
  panel.grid.minor_y, panel.grid.minor.y_colour, panel.grid.minor.y_linewidth, panel.grid.minor.y_linetype,
  panel.background, panel.background_fill,
  panel.border_enable, panel.border_colour, panel.border_linewidth, panel.border_linetype,
  plot.background, plot.background_fill, plot.background_border_enable, plot.background_colour, plot.background_linewidth, plot.background_linetype,

  legend.background, legend.background_fill, legend.background_border_enable, legend.background_colour, legend.background_linewidth, legend.background_linetype,
  legend.key, legend.key_fill, legend.key_border_enable, legend.key_colour, legend.key_linewidth, legend.key_linetype,
  # legend.key.size,
  # legend.key.height,
  # legend.key.width,
  # legend.text,
  # legend.text.align,
  # legend.title,
  # legend.title.align,
  # legend.position,
  # legend.direction,
  # legend.justification,
  # legend.box,
  # legend.box.just,
  # legend.box.margin,
  # legend.box.background,
  # legend.box.spacing,

  axis_line_x, axis.line.x_colour, axis.line.x_linetype, axis_line_x_arrow, axis.line.x_arrow_angle, axis.line.x_arrow_length,
  axis_line_y, axis.line.y_colour, axis.line.y_linetype, axis_line_y_arrow, axis.line.y_arrow_angle, axis.line.y_arrow_length,
  axis_text_x, axis.text.x_family, axis.text.x_face, axis.text.x_colour, axis.text.x_size, axis.text.x_angle,
  axis_text_y, axis.text.y_family, axis.text.y_face, axis.text.y_colour, axis.text.y_size, axis.text.y_angle,
  axis_ticks_x, axis.ticks.x_colour, axis.ticks.x_linewidth,
  axis_ticks_y, axis.ticks.y_colour, axis.ticks.y_linewidth,
  axis.ticks.length.x_length,
  axis.ticks.length.y_length
) {
  face2 <- function(face) {if ("B" %in% face && "I" %in% face) {return("bold.italic")} else if ("B" %in% face) {return("bold")} else if ("I" %in% face) {return("italic")} else {return("plain")}}
  axis.title.x_face2 <- face2(axis.title.x_face)
  axis.title.y_face2 <- face2(axis.title.y_face)
  axis.text.x_face2 <- face2(axis.text.x_face)
  axis.text.y_face2 <- face2(axis.text.y_face)
  plot.title_face2 <- face2(plot.title_face)
  plot.subtitle_face2 <- face2(plot.subtitle_face)
  plot.caption_face2 <- face2(plot.caption_face)
  plot.tag_face2 <- face2(plot.tag_face)

  theme_custom <- theme(
    axis.title.x = if(axis.title.x_size <= 0) {element_blank()} else{element_text(family = axis.title.x_family, face = axis.title.x_face2, colour = axis.title.x_colour, size = axis.title.x_size, hjust = axis.title.x_hjust, angle = axis.title.x_angle)},
    axis.title.y = if(axis.title.y_size <= 0) {element_blank()} else{element_text(family = axis.title.y_family, face = axis.title.y_face2, colour = axis.title.y_colour, size = axis.title.y_size, hjust = axis.title.y_hjust, angle = axis.title.y_angle)},
    plot.title = if(plot.title_size <= 0) {element_blank()} else{element_text(family = plot.title_family, face = plot.title_face2, colour = plot.title_colour, size = plot.title_size, hjust = plot.title_hjust, angle = plot.title_angle)},
    plot.subtitle = if(plot.subtitle_size <= 0) {element_blank()} else{element_text(family = plot.subtitle_family, face = plot.subtitle_face2, colour = plot.subtitle_colour, size = plot.subtitle_size, hjust = plot.subtitle_hjust, angle = plot.subtitle_angle)},
    plot.caption = if(plot.caption_size <= 0) {element_blank()} else{element_text(family = plot.caption_family, face = plot.caption_face2, colour = plot.caption_colour, size = plot.caption_size, hjust = plot.caption_hjust, angle = plot.caption_angle)},
    plot.tag = if(plot.tag_size <= 0) {element_blank()} else{element_text(family = plot.tag_family, face = plot.tag_face2, colour = plot.tag_colour, size = plot.tag_size, hjust = plot.tag_hjust, angle = plot.tag_angle)},
    panel.grid.major.x = if(panel.grid.major_x == FALSE || panel.grid.major.x_linewidth <= 0) {element_blank()} else{element_line(colour = panel.grid.major.x_colour, linewidth = panel.grid.major.x_linewidth, linetype = panel.grid.major.x_linetype)},
    panel.grid.major.y = if(panel.grid.major_y == FALSE || panel.grid.major.y_linewidth <= 0) {element_blank()} else{element_line(colour = panel.grid.major.y_colour, linewidth = panel.grid.major.y_linewidth, linetype = panel.grid.major.y_linetype)},
    panel.grid.minor.x = if(panel.grid.minor_x == FALSE || panel.grid.minor.x_linewidth <= 0) {element_blank()} else{element_line(colour = panel.grid.minor.x_colour, linewidth = panel.grid.minor.x_linewidth, linetype = panel.grid.minor.x_linetype)},
    panel.grid.minor.y = if(panel.grid.minor_y == FALSE || panel.grid.minor.y_linewidth <= 0) {element_blank()} else{element_line(colour = panel.grid.minor.y_colour, linewidth = panel.grid.minor.y_linewidth, linetype = panel.grid.minor.y_linetype)},
    panel.background = if(panel.background == FALSE) {element_blank()} else{element_rect(fill = panel.background_fill)},
    panel.border = if(panel.border_enable == FALSE || panel.border_linewidth <= 0) {element_blank()} else{element_rect(fill = NA, colour = panel.border_colour, linewidth = panel.border_linewidth, linetype = panel.border_linetype)},
    plot.background = if(plot.background == FALSE) {element_blank()} else{if(plot.background_border_enable == FALSE || plot.background_linewidth <= 0) {element_rect(fill = plot.background_fill, colour = "NA")} else{element_rect(fill = plot.background_fill, colour = plot.background_colour, linewidth = plot.background_linewidth, linetype = plot.background_linetype)}},
    legend.background = if(legend.background == FALSE) {element_blank()} else{if(legend.background_border_enable == FALSE || legend.background_linewidth <= 0) {element_rect(fill = legend.background_fill)} else{element_rect(fill = legend.background_fill, colour = legend.background_colour, linewidth = legend.background_linewidth, linetype = legend.background_linetype)}},
    legend.key = if(legend.key == FALSE) {element_blank()} else{if(legend.key_border_enable == FALSE || legend.key_linewidth <= 0) {element_rect(fill = legend.key_fill)} else{element_rect(fill = legend.key_fill, colour = legend.key_colour, linewidth = legend.key_linewidth, linetype = legend.key_linetype)}},
  ) + theme(
    axis.line.x = if(axis_line_x == FALSE) {element_blank()} else{element_line(colour = axis.line.x_colour, linetype = axis.line.x_linetype, arrow = if(axis_line_x_arrow == FALSE || axis.line.x_arrow_length == 0) {FALSE} else{arrow(angle = axis.line.x_arrow_angle, length = unit(axis.line.x_arrow_length, "mm"))})},
    axis.line.y = if(axis_line_y == FALSE) {element_blank()} else{element_line(colour = axis.line.y_colour, linetype = axis.line.y_linetype, arrow = if(axis_line_y_arrow == FALSE || axis.line.y_arrow_length == 0) {FALSE} else{arrow(angle = axis.line.y_arrow_angle, length = unit(axis.line.y_arrow_length, "mm"))})},
    axis.text.x = if(axis_text_x == FALSE || axis.text.x_size <= 0) {element_blank()} else{element_text(family = axis.text.x_family, face = axis.text.x_face2, colour = axis.text.x_colour, size = axis.text.x_size, angle = axis.text.x_angle)},
    axis.text.y = if(axis_text_y == FALSE || axis.text.y_size <= 0) {element_blank()} else{element_text(family = axis.text.y_family, face = axis.text.y_face2, colour = axis.text.y_colour, size = axis.text.y_size, angle = axis.text.y_angle)},
    axis.ticks.x = if(axis_ticks_x == FALSE || axis.ticks.x_linewidth <= 0 || axis.ticks.length.x_length == 0) {element_blank()} else{element_line(colour = axis.ticks.x_colour, linewidth = axis.ticks.x_linewidth)},
    axis.ticks.y = if(axis_ticks_y == FALSE || axis.ticks.y_linewidth <= 0 || axis.ticks.length.y_length == 0) {element_blank()} else{element_line(colour = axis.ticks.y_colour, linewidth = axis.ticks.y_linewidth)}
  )

  if(axis_ticks_x == TRUE && axis.ticks.x_linewidth > 0 && axis.ticks.length.x_length != 0) {theme_custom <- theme_custom + theme(axis.ticks.length.x = unit(axis.ticks.length.x_length, "mm"))} else { }
  if(axis_ticks_y == TRUE && axis.ticks.y_linewidth > 0 && axis.ticks.length.y_length != 0) {theme_custom <- theme_custom + theme(axis.ticks.length.y = unit(axis.ticks.length.y_length, "mm"))} else { }

  return(theme_custom)
}