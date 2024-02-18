

    # # axis
    # output[[paste0(ggtype, "_axis")]] <- renderUI({
    #   div(
    #     div(style = "display: flex;",
    #       div(style = "width: 10%; height: 222px; background-color: lightblue; justify-content: center; align-items: center;", "X轴"),
    #       div(style = "width: 90%;",
    #         div(style = "height: 74px; display: flex; gap:1.111%;",
    #           div(style = "width: 48%;",
    #             div(style = "height: 25px;", input_switch(inputId = paste0(ggtype, "_axis_line_x"), label = "轴线", value = TRUE)),
    #             conditionalPanel(condition = paste0("input.", ggtype, "_axis_line_x"), style = "display: flex; gap: 2.314%;",
    #               div(style = "width: 37.037%;", selectInput(inputId = paste0(ggtype, "_axis.line.x_linetype"), label = NULL, choices = c("solid", "dashed", "dotted", "dotdash", "longdash", "twodash"))),
    #               div(style = "width: 32.407%;", colorPickr(inputId = paste0(ggtype, "_axis.line.x_colour"), label = NULL, selected = "#000000"))
    #             )
    #           ),
    #           conditionalPanel(condition = paste0("input.", ggtype, "_axis_line_x"), style = "width: 48%;",
    #             div(style = "height: 25px;", input_switch(inputId = paste0(ggtype, "_axis_line_x_arrow"), label = "轴端箭头", value = FALSE)),
    #             conditionalPanel(condition = paste0("input.", ggtype, "_axis_line_x_arrow"), style = "display: flex; gap: 2.314%;",
    #               div(style = "width: 25.462%;", numericInput(inputId = paste0(ggtype, "_axis.line.x_arrow_length"), label = NULL, value = 2.5, min = 0, max = 5, step = 0.5)),
    #               div(style = "width: 25.462%;", numericInput(inputId = paste0(ggtype, "_axis.line.x_arrow_angle"), label = NULL, value = 30, min = 0, max = 90))
    #             )
    #           )
    #         ),
    #         div(style = "height: 74px;",
    #           div(style = "height: 25px;", input_switch(inputId = paste0(ggtype, "_axis_ticks_x"), label = "轴刻度", value = TRUE)),
    #           conditionalPanel(condition = paste0("input.", ggtype, "_axis_ticks_x"), style = "display: flex; gap: 1.111%;",
    #             div(style = "width: 12.222%;", numericInput(inputId = paste0(ggtype, "_axis.ticks.length.x_length"), label = NULL, value = 1, min = 0.5, max = 4, step = 0.5)),
    #             div(style = "width: 12.222%;", numericInput(inputId = paste0(ggtype, "_axis.ticks.x_linewidth"), label = NULL, value = 0.5, min = 0, max = 5, step = 0.5)),
    #             div(style = "width: 15.555%;", colorPickr(inputId = paste0(ggtype, "_axis.ticks.x_colour"), label = NULL, selected = "#333333"))
    #           )
    #         ),
    #         div(style = "height: 74px;",
    #           div(style = "height: 25px;", input_switch(inputId = paste0(ggtype, "_axis_text_x"), label = "轴文本", value = TRUE)),
    #           conditionalPanel(condition = paste0("input.", ggtype, "_axis_text_x"), style = "display: flex; gap: 1.111%;",
    #             div(style = "width: 17.777%;", selectInput(inputId = paste0(ggtype, "_axis.text.x_family"), label = NULL, choices = c("sans", "serif", "mono"))),
    #             div(style = "width: 12.222%;", numericInput(inputId = paste0(ggtype, "_axis.text.x_size"), label = NULL, value = 8.8, min = 8, max = 22)),
    #             div(style = "width: 12.222%;", checkboxGroupButtons(inputId = paste0(ggtype, "_axis.text.x_face"), label = NULL, c(`<i class="fa fa-bold"></i>` = "B", `<i class="fa fa-italic"></i>`= "I"), justified = TRUE)),
    #             div(style = "width: 15.555%;", colorPickr(inputId = paste0(ggtype, "_axis.text.x_colour"), label = NULL, selected = "#000000")),
    #             div(style = "width: 12.222%;", numericInput(inputId = paste0(ggtype, "_axis.text.x_angle"), label = NULL, value = 0, min = -90, max = 90))
    #           )
    #         )
    #       )
    #     ),
    #     div(style = "display: flex;",
    #       div(style = "width: 10%; height: 222px; background-color: lightblue; justify-content: center; align-items: center;", "Y轴"),
    #       div(style = "width: 90%;",
    #         div(style = "height: 74px; display: flex; gap:1.111%;",
    #           div(style = "width: 48%;",
    #             div(style = "height: 25px;", input_switch(inputId = paste0(ggtype, "_axis_line_y"), label = "轴线", value = TRUE)),
    #             conditionalPanel(condition = paste0("input.", ggtype, "_axis_line_y"), style = "display: flex; gap: 2.314%;",
    #               div(style = "width: 37.037%;", selectInput(inputId = paste0(ggtype, "_axis.line.y_linetype"), label = NULL, choices = c("solid", "dashed", "dotted", "dotdash", "longdash", "twodash"))),
    #               div(style = "width: 32.407%;", colorPickr(inputId = paste0(ggtype, "_axis.line.y_colour"), label = NULL, selected = "#000000"))
    #             )
    #           ),
    #           conditionalPanel(condition = paste0("input.", ggtype, "_axis_line_y"), style = "width: 48%;",
    #             div(style = "height: 25px;", input_switch(inputId = paste0(ggtype, "_axis_line_y_arrow"), label = "轴端箭头", value = FALSE)),
    #             conditionalPanel(condition = paste0("input.", ggtype, "_axis_line_y_arrow"), style = "display: flex; gap: 2.314%;",
    #               div(style = "width: 25.462%;", numericInput(inputId = paste0(ggtype, "_axis.line.y_arrow_length"), label = NULL, value = 2.5, min = 0, max = 5, step = 0.5)),
    #               div(style = "width: 25.462%;", numericInput(inputId = paste0(ggtype, "_axis.line.y_arrow_angle"), label = NULL, value = 30, min = 0, max = 90))
    #             )
    #           )
    #         ),
    #         div(style = "height: 74px;",
    #           div(style = "height: 25px;", input_switch(inputId = paste0(ggtype, "_axis_ticks_y"), label = "轴刻度", value = TRUE)),
    #           conditionalPanel(condition = paste0("input.", ggtype, "_axis_ticks_y"), style = "display: flex; gap: 1.111%;",
    #             div(style = "width: 12.222%;", numericInput(inputId = paste0(ggtype, "_axis.ticks.length.y_length"), label = NULL, value = 1, min = 0.5, max = 4, step = 0.5)),
    #             div(style = "width: 12.222%;", numericInput(inputId = paste0(ggtype, "_axis.ticks.y_linewidth"), label = NULL, value = 0.5, min = 0, max = 5, step = 0.5)),
    #             div(style = "width: 15.555%;", colorPickr(inputId = paste0(ggtype, "_axis.ticks.y_colour"), label = NULL, selected = "#333333"))
    #           )
    #         ),
    #         div(style = "height: 74px;",
    #           div(style = "height: 25px;", input_switch(inputId = paste0(ggtype, "_axis_text_y"), label = "轴文本", value = TRUE)),
    #           conditionalPanel(condition = paste0("input.", ggtype, "_axis_text_y"), style = "display: flex; gap: 1.111%;",
    #             div(style = "width: 17.777%;", selectInput(inputId = paste0(ggtype, "_axis.text.y_family"), label = NULL, choices = c("sans", "serif", "mono"))),
    #             div(style = "width: 12.222%;", numericInput(inputId = paste0(ggtype, "_axis.text.y_size"), label = NULL, value = 8.8, min = 8, max = 22)),
    #             div(style = "width: 12.222%;", checkboxGroupButtons(inputId = paste0(ggtype, "_axis.text.y_face"), label = NULL, c(`<i class="fa fa-bold"></i>` = "B", `<i class="fa fa-italic"></i>`= "I"), justified = TRUE)),
    #             div(style = "width: 15.555%;", colorPickr(inputId = paste0(ggtype, "_axis.text.y_colour"), label = NULL, selected = "#000000")),
    #             div(style = "width: 12.222%;", numericInput(inputId = paste0(ggtype, "_axis.text.y_angle"), label = NULL, value = 0, min = -90, max = 90))
    #           )
    #         )
    #       )
    #     ),




    #   )
    # })


    # observe({toggleState(id = paste0(ggtype, "_panel.grid.major_x_custom"), condition = input[[paste0(ggtype, "_panel.grid.major_x")]])})
    # observe({toggleState(id = paste0(ggtype, "_panel.grid.minor_x_custom"), condition = input[[paste0(ggtype, "_panel.grid.minor_x")]])})
    # observe({toggleState(id = paste0(ggtype, "_panel.grid.major_y_custom"), condition = input[[paste0(ggtype, "_panel.grid.major_y")]])})
    # observe({toggleState(id = paste0(ggtype, "_panel.grid.minor_y_custom"), condition = input[[paste0(ggtype, "_panel.grid.minor_y")]])})

    # observe({toggleState(id = paste0(ggtype, "_panel.background_custom"), condition = input[[paste0(ggtype, "_panel.background")]])})
    # observe({toggleState(id = paste0(ggtype, "_panel.border_custom"), condition = input[[paste0(ggtype, "_panel.border_enable")]])})

    # observe({toggleState(id = paste0(ggtype, "_plot.background_custom"), condition = input[[paste0(ggtype, "_plot.background")]])})
    # observe({toggleState(id = paste0(ggtype, "_plot.background_border_custom"), condition = input[[paste0(ggtype, "_plot.background_border_enable")]])})
