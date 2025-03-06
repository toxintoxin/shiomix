
    # #_legend
    # output[[paste0(ggtype, "_legend")]] <- renderUI({
    #   div(
    #     div(style = "display: flex;",
    #       div(style = "width: 20%;",
    #         div(style = "height: 25px;", tipify(input_switch(inputId = paste0(ggtype, "_legend.key"), label = "图例项背景", value = FALSE), "现在是透明的，打开后会产生额外的矢量元素供你选择填充颜色")),
    #         div(style = "width: 70%;", id = paste0(ggtype, "_legend.key_custom"), colorPickr(inputId = paste0(ggtype, "_legend.key_fill"), label = NULL, selected = "#F2F2F2"))
    #       ),
    #       div(style = "width: 80%;", id = paste0(ggtype, "_legend.key_border"),
    #         div(style = "height: 25px; width: 25%;", tipify(input_switch(inputId = paste0(ggtype, "_legend.key_border_enable"), label = "图例项边框", value = FALSE), "你必须先打开图例背景")),
    #         div(style = "display: flex; gap: 1.25%", id = paste0(ggtype, "_legend.key_border_custom"),
    #           div(style = "width: 20%;", selectInput(inputId = paste0(ggtype, "_legend.key_linetype"), label = NULL, choices = c("solid", "dashed", "dotted", "dotdash", "longdash", "twodash"))),
    #           div(style = "width: 13.75%;", numericInput(inputId = paste0(ggtype, "_legend.key_linewidth"), label = NULL, value = 0.5, min = 0.1, max = 2, step = 0.1)),
    #           div(style = "width: 17.5%;", colorPickr(inputId = paste0(ggtype, "_legend.key_colour"), label = NULL, selected = "#000000"))
    #         )
    #       )
    #     ),
    #     div(style = "display: flex;",
    #       div(style = "width: 20%;",
    #         div(style = "height: 25px;", tipify(input_switch(inputId = paste0(ggtype, "_legend.background"), label = "图例背景", value = FALSE), "现在是透明的，打开后会产生额外的矢量元素供你选择填充颜色")),
    #         div(style = "width: 70%;", id = paste0(ggtype, "_legend.background_custom"), colorPickr(inputId = paste0(ggtype, "_legend.background_fill"), label = NULL, selected = "#FFFFFF"))
    #       ),
    #       div(style = "width: 80%;", id = paste0(ggtype, "_legend.background_border"),
    #         div(style = "height: 25px; width: 25%;", tipify(input_switch(inputId = paste0(ggtype, "_legend.background_border_enable"), label = "图例边框", value = FALSE), "你必须先打开图例背景")),
    #         div(style = "display: flex; gap: 1.25%", id = paste0(ggtype, "_legend.background_border_custom"),
    #           div(style = "width: 20%;", selectInput(inputId = paste0(ggtype, "_legend.background_linetype"), label = NULL, choices = c("solid", "dashed", "dotted", "dotdash", "longdash", "twodash"))),
    #           div(style = "width: 13.75%;", numericInput(inputId = paste0(ggtype, "_legend.background_linewidth"), label = NULL, value = 0.5, min = 0.1, max = 2, step = 0.1)),
    #           div(style = "width: 17.5%;", colorPickr(inputId = paste0(ggtype, "_legend.background_colour"), label = NULL, selected = "#000000"))
    #         )
    #       )
    #     )



    #   )
    # })

    # observe({toggleState(id = paste0(ggtype, "_legend.key_custom"), condition = input[[paste0(ggtype, "_legend.key")]])})
    # observe({toggleState(id = paste0(ggtype, "_legend.key_border_custom"), condition = input[[paste0(ggtype, "_legend.key_border_enable")]])})

    # observe({toggleState(id = paste0(ggtype, "_legend.background_custom"), condition = input[[paste0(ggtype, "_legend.background")]])})
    # observe({toggleState(id = paste0(ggtype, "_legend.background_border_custom"), condition = input[[paste0(ggtype, "_legend.background_border_enable")]])})