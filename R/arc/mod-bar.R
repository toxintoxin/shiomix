barUI <- function(id) {
  ns <- NS(id)
    card(
    card_header(
      class = "d-flex justify-content-between",
      "柱状图",
      actionButton(ns("restart"), "Restart (still building)")
    ),
    layout_sidebar(border_radius = FALSE, class = "p-0",
      sidebar = sidebar(width = "700px", open = "always",
        navset_pill_list(widths = c(2, 10),
          nav_panel("Upload",
            excelInput(ns("data"), header = "Data"),
            excelInput(ns("phenoData"), header = "phenoData")
          ),
          nav_panel("Main",
            plotOutput(ns("plot_test")),
            dropMenu(
              actionButton(ns("data_summary_go"), "Summary", icon = icon("table")),
              tableOutput(ns("data_summary")),
              placement = "bottom-start", arrow = FALSE
            ),
            div(style = "display: flex;",
              div(style = "width: 50%;", selectInput(ns("position"), "柱分布", choices = c("无边框并紧贴", "有边框有空隙", "堆叠", "堆叠并填充到1"))),
              conditionalPanel(style = "display: flex;",
                ns = ns,
                condition = "input.position == '无边框并紧贴' || input.position == '有边框有空隙'",
                selectInput(ns("dodge_type"), "Type", choices = c("Mean with SEM", "Mean with SD"))
              )
            ),
            div(style = "width: 50%;", selectInput(ns("x"), "x轴", choices = c("group", "var"))),
            div(style = "display: flex;",
              div(style = "width: 50%;", multiInput(ns("x_multi"), label = NULL, choices = "", options = list(search_placeholder = "搜索...",non_selected_header = "待选：", selected_header = "已选："))),
              div(style = "width: 50%;", multiInput(ns("fill_multi"), label = NULL, choices = "", options = list(search_placeholder = "展示...",non_selected_header = "待选：", selected_header = "已选：")))
            ),
            div(style = "display: flex;",
              div(style = "width: 11%; font-weight: 700;", "颜色方案"),
              radioButtons(ns("aes_fill"), label = NULL, choices = c("预设", "自定义"), inline = TRUE)
            ),
            uiOutput(ns("aes_fill_plan")),
            div(style = "display: flex; gap: 2%;",
              div(style = "width: 11%;", numericInput(ns("width"), "每组柱宽度", value = 0.9, min = 0.1, max = 0.9, step = 0.1)),
              div(style = "width: 11%;", numericInput(ns("alpha"), "柱透明度", value = 1, min = 0.05, max = 1, step = 0.05)),
              conditionalPanel(
                ns = ns,
                condition = "input.position == '有边框有空隙'", style = "display: flex; width: 74%; gap: 2.7%;",
                div(style = "width: 14.865%;", numericInput(ns("linewidth"), "柱边框", value = 0.5, min = 0.1, max = 2, step = 0.1)),
                div(style = "width: 14.865%;", numericInput(ns("padding"), "柱间距", value = 0.1, min = 0.1, max = 0.9, step = 0.1)),
                div(style = "width: 70%;", radioButtons(ns("colour"), "颜色", choices = c("黑色", "颜色方案")))
              )
            ),
            conditionalPanel(
              ns = ns,
              condition = "input.position == '无边框并紧贴' || input.position == '有边框有空隙'",
              div(style = "display: flex;",
                div(style = "width: 26%; height: 85px;", input_switch(ns("errorbar"), label = "errorbar", value = TRUE)),
                conditionalPanel(
                  ns = ns,
                  condition = "input.errorbar", style = "display: flex; width: 74%; gap: 2.7%;",
                  div(style = "width: 14.865%;", numericInput(ns("errorbar_width"), "宽度", value = 0.5, min = 0.1, max = 0.9, step = 0.1)),
                  div(style = "width: 14.865%;", radioButtons(ns("errorbar_shape"), "外观", choices = c("完全", "一半"))),
                  div(style = "width: 70%;", radioButtons(ns("errorbar_colour"), "颜色", choices = c("黑色", "颜色方案")))
                )
              ),
              div(style = "display: flex;",
                div(style = "width: 26%; height: 85px;", input_switch(ns("jitter"), label = "数据点", value = TRUE)),
                conditionalPanel(
                  ns = ns,
                  condition = "input.jitter", style = "display: flex; width: 74%; gap: 2.7%;",
                  div(style = "width: 14.865%;", numericInput(ns("jitter_width"), "宽度", value = 2, min = 0.5, max = 4)),
                  div(style = "width: 14.865%;", numericInput(ns("jitter_size"), "大小", value = 2, min = 0.5, max = 4)),
                  div(style = "width: 70%;", radioButtons(ns("jitter_colour"), "颜色", choices = c("黑色", "颜色方案")))
                )
              )
            )
          ),
          nav_panel("Labels", gglabsUI(ns(NULL))),
          nav_panel("Theme", ggthemeUI(ns(NULL)))
        )
      ),
      layout_sidebar(border = FALSE, style = "background: #eeeddd",
        sidebar = sidebar(width = "300px", position = "right",
          includeMarkdown(paste0("data-science/visualization/types-readme/", id, ".md")),
        ),
        vsUniversalUI(ns(NULL))
      )
    )
  )




}

barServer <- function(id) {
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


    # 选项更新
    observeEvent(input$x, {

      if (input$x == "group") {
        x_multi_choices <- unique(rv$df$group)
      } else if (input$x == "var") {
        x_multi_choices <- unique(rv$df$var)
      }

      if (input$x == "group") {
        fill_multi_choices <- unique(rv$df$var)
      } else if (input$x == "var") {
        fill_multi_choices <- unique(rv$df$group)
      }

      updateSelectInput(session, "x_multi", choices = x_multi_choices, selected = x_multi_choices)
      updateSelectInput(session, "fill_multi", choices = fill_multi_choices, selected = fill_multi_choices)
    })


    # 柱状图色彩方案
    output$aes_fill_plan <- renderUI({
      if (input$aes_fill == "预设") {
        selectInput("aes_fill_palette", label = NULL, choices = c("Default"))
      } else if (input$aes_fill == "自定义") {
        colorPickr_list <- map(input$x_multi, function(label) {
          colorPickr(inputId = ns(paste0("aes_fill_", gsub(":", "_", label))), label = label, selected = "#000000", width = "100px")
        })
        div(style = "display: flex; flex-wrap: wrap; gap: 2%;", colorPickr_list)
      }
    })





    observeEvent(input$apply, {
      rv$data_long <- rv$data_original %>%
        rename("var" = 1) %>%
        pivot_longer(-1, names_to = "sample", values_to = "value") %>%
        left_join(rv$phenoData, by = "sample")

        # 只绘制选中的，并映射
        if (input$x == "group") {p <- rv$data_long %>% filter(group %in% input$x_multi) %>% filter(var %in% input$fill_multi) %>% ggplot(aes(x = group, y = value, fill = var))}
        else if (input$x == "var") {p <- rv$data_long %>% filter(var %in% input$x_multi) %>% filter(group %in% input$fill_multi) %>% ggplot(aes(x = var, y = value, fill = group))}

        # 判断positon
        if (input$position == "无边框并紧贴") {
          # bar
          p <- p + stat_summary(geom = "bar", fun = "mean", alpha = input$alpha, position = position_dodge(input$width, preserve = "single"), width = input$width)

          # errorbar
          if (input$errorbar == TRUE) {
            if (input$dodge_type == "Mean with SEM") {
              if (input$errorbar_colour == "黑色") {
                p <- p + stat_summary(geom = if (input$errorbar_shape == "完全") {"errorbar"} else if (input$errorbar_shape == "一半") {"uperrorbar"}, fun.data = "mean_se", position = position_dodge(input$width), width = input$errorbar_width, show.legend = FALSE)
              } else if (input$errorbar_colour == "颜色方案") {
                p <- p + stat_summary(geom = if (input$errorbar_shape == "完全") {"errorbar"} else if (input$errorbar_shape == "一半") {"uperrorbar"}, fun.data = "mean_se", position = position_dodge(input$width), width = input$errorbar_width, aes(colour = if (input$x == "group") {var} else if (input$x == "var") {group}), show.legend = FALSE)
              }
            } else if (input$dodge_type == "Mean with SD") {
              if (input$errorbar_colour == "黑色") {
                p <- p + stat_summary(geom = if (input$errorbar_shape == "完全") {"errorbar"} else if (input$errorbar_shape == "一半") {"uperrorbar"}, fun.data = "mean_sdl", fun.args = list(mult = 1), position = position_dodge(input$width), width = input$errorbar_width, show.legend = FALSE)
              } else if (errorbar_colour == "颜色方案") {
                p <- p + stat_summary(geom = if (input$errorbar_shape == "完全") {"errorbar"} else if (input$errorbar_shape == "一半") {"uperrorbar"}, fun.data = "mean_sdl", fun.args = list(mult = 1), position = position_dodge(input$width), width = input$errorbar_width, aes(colour = if (input$x == "group") {var} else if (input$x == "var") {group}), show.legend = FALSE)
              }
            }
          }

          # jitter
          if (input$jitter == TRUE) {
            if (input$jitter_colour == "黑色") {
              p <- p + geom_jitter(position = position_dodge(input$width), size = input$jitter_size, show.legend = FALSE)
            } else if (input$jitter_colour == "颜色方案") {
              p <- p + geom_jitter(position = position_dodge(input$width), size = input$jitter_size, aes(colour = if (input$x == "group") {var} else if (input$x == "var") {group}), show.legend = FALSE)
            }
          }


        } else if (input$position == "有边框有空隙") {
          # 追加映射
          if (input$x == "group") {p <- p + aes(colour = var)}
          else if (input$x == "var") {p <- p + aes(colour = group)}

          # bar
          if (input$colour == "黑色") {
            p <- p + stat_summary(geom = "bar", fun = "mean", alpha = input$alpha, position = position_dodge2(input$width, padding = input$padding, preserve = "single"), width = input$width, linewidth = input$linewidth, colour = "#000000")
          } else if (input$colour == "颜色方案") {
            p <- p + stat_summary(geom = "bar", fun = "mean", alpha = input$alpha, position = position_dodge2(input$width, padding = input$padding, preserve = "single"), width = input$width, linewidth = input$linewidth)
          }

          # errorbar
          if (input$errorbar == TRUE) {
            if (input$dodge_type == "Mean with SEM") {
              if (input$errorbar_colour == "黑色") {
                p <- p + stat_summary(geom = if (input$errorbar_shape == "完全") {"errorbar"} else if (input$errorbar_shape == "一半") {"uperrorbar"}, fun.data = "mean_se", position = position_dodge2(input$width, padding = input$padding, preserve = "single"), width = input$errorbar_width, show.legend = FALSE, colour = "#000000")
              } else if (input$errorbar_colour == "颜色方案") {
                p <- p + stat_summary(geom = if (input$errorbar_shape == "完全") {"errorbar"} else if (input$errorbar_shape == "一半") {"uperrorbar"}, fun.data = "mean_se", position = position_dodge2(input$width, padding = input$padding, preserve = "single"), width = input$errorbar_width, show.legend = FALSE)
              }
            } else if (input$dodge_type == "Mean with SD") {
              if (input$errorbar_colour == "黑色") {
                p <- p + stat_summary(geom = if (input$errorbar_shape == "完全") {"errorbar"} else if (input$errorbar_shape == "一半") {"uperrorbar"}, fun.data = "mean_sdl", fun.args = list(mult = 1), position = position_dodge2(input$width, padding = input$padding, preserve = "single"), width = input$errorbar_width, show.legend = FALSE, colour = "#000000")
              } else if (input$errorbar_colour == "颜色方案") {
                p <- p + stat_summary(geom = if (input$errorbar_shape == "完全") {"errorbar"} else if (input$errorbar_shape == "一半") {"uperrorbar"}, fun.data = "mean_sdl", fun.args = list(mult = 1), position = position_dodge2(input$width, padding = input$padding, preserve = "single"), width = input$errorbar_width, show.legend = FALSE)
              }
            }
          }

          # jitter
          if (input$jitter == TRUE) {
            if (input$jitter_colour == "黑色") {
              p <- p + geom_jitter(position = position_dodge2(input$width, padding = input$padding, preserve = "single"), size = input$jitter_size, show.legend = FALSE, colour = "#000000")
            } else if (input$jitter_colour == "颜色方案") {
              p <- p + geom_jitter(position = position_dodge2(input$width, padding = input$padding, preserve = "single"), size = input$jitter_size, show.legend = FALSE)
            }
          }
        } else if (input$position == "堆叠") {
          p <- p + stat_summary(geom = "bar", fun = "mean", alpha = input$alpha, position = position_stack(), width = input$width)
        } else if (input$position == "堆叠并填充到1") {
          p <- p + stat_summary(geom = "bar", fun = "mean", alpha = input$alpha, position = position_fill(), width = input$width)
        }

      rv$plot_init <- p
      rv$plot_labeled <- gglabsServer(NULL, rv$plot_init)
      rv$plot_final <- ggthemeServer(NULL, rv$plot_labeled)
    })

    vsUniversalServer(NULL, id, rv)










    # # # 计算mean, SD, SEM，并展示给用户
    # # rv$df_summary <- rv$df %>% group_by(group, var) %>% summarise(
    # #   count = n(), Mean = mean(value), SD = sd(value), SEM = sd(value) / sqrt(n())
    # # )
    # # output$data_summary <- renderTable({rv$df_summary})

    # ggobj <- reactive({


    #   # 只绘制选中的，并映射
    #   if (input$x == "group") {p <- rv$df %>% filter(group %in% input$x_multi) %>% filter(var %in% input$fill_multi) %>% ggplot(aes(x = group, y = value, fill = var))}
    #   else if (input$x == "var") {p <- rv$df %>% filter(var %in% input$x_multi) %>% filter(group %in% input$fill_multi) %>% ggplot(aes(x = var, y = value, fill = group))}

    #   # 判断positon
    #   if (input$position == "无边框并紧贴") {
    #     # bar
    #     p <- p + stat_summary(geom = "bar", fun = "mean", alpha = input$alpha, position = position_dodge(input$width, preserve = "single"), width = input$width)

    #     # errorbar
    #     if (input$errorbar == TRUE) {
    #       if (input$dodge_type == "Mean with SEM") {
    #         if (input$errorbar_colour == "黑色") {
    #           p <- p + stat_summary(geom = if (input$errorbar_shape == "完全") {"errorbar"} else if (input$errorbar_shape == "一半") {"uperrorbar"}, fun.data = "mean_se", position = position_dodge(input$width), width = input$errorbar_width, show.legend = FALSE)
    #         } else if (input$errorbar_colour == "颜色方案") {
    #           p <- p + stat_summary(geom = if (input$errorbar_shape == "完全") {"errorbar"} else if (input$errorbar_shape == "一半") {"uperrorbar"}, fun.data = "mean_se", position = position_dodge(input$width), width = input$errorbar_width, aes(colour = if (input$x == "group") {var} else if (input$x == "var") {group}), show.legend = FALSE)
    #         }
    #       } else if (input$dodge_type == "Mean with SD") {
    #         if (input$errorbar_colour == "黑色") {
    #           p <- p + stat_summary(geom = if (input$errorbar_shape == "完全") {"errorbar"} else if (input$errorbar_shape == "一半") {"uperrorbar"}, fun.data = "mean_sdl", fun.args = list(mult = 1), position = position_dodge(input$width), width = input$errorbar_width, show.legend = FALSE)
    #         } else if (errorbar_colour == "颜色方案") {
    #           p <- p + stat_summary(geom = if (input$errorbar_shape == "完全") {"errorbar"} else if (input$errorbar_shape == "一半") {"uperrorbar"}, fun.data = "mean_sdl", fun.args = list(mult = 1), position = position_dodge(input$width), width = input$errorbar_width, aes(colour = if (input$x == "group") {var} else if (input$x == "var") {group}), show.legend = FALSE)
    #         }
    #       }
    #     }

    #     # jitter
    #     if (input$jitter == TRUE) {
    #       if (input$jitter_colour == "黑色") {
    #         p <- p + geom_jitter(position = position_dodge(input$width), size = input$jitter_size, show.legend = FALSE)
    #       } else if (input$jitter_colour == "颜色方案") {
    #         p <- p + geom_jitter(position = position_dodge(input$width), size = input$jitter_size, aes(colour = if (input$x == "group") {var} else if (input$x == "var") {group}), show.legend = FALSE)
    #       }
    #     }


    #   } else if (input$position == "有边框有空隙") {
    #     # 追加映射
    #     if (input$x == "group") {p <- p + aes(colour = var)}
    #     else if (input$x == "var") {p <- p + aes(colour = group)}

    #     # bar
    #     if (input$colour == "黑色") {
    #       p <- p + stat_summary(geom = "bar", fun = "mean", alpha = input$alpha, position = position_dodge2(input$width, padding = input$padding, preserve = "single"), width = input$width, linewidth = input$linewidth, colour = "#000000")
    #     } else if (input$colour == "颜色方案") {
    #       p <- p + stat_summary(geom = "bar", fun = "mean", alpha = input$alpha, position = position_dodge2(input$width, padding = input$padding, preserve = "single"), width = input$width, linewidth = input$linewidth)
    #     }

    #     # errorbar
    #     if (input$errorbar == TRUE) {
    #       if (input$dodge_type == "Mean with SEM") {
    #         if (input$errorbar_colour == "黑色") {
    #           p <- p + stat_summary(geom = if (input$errorbar_shape == "完全") {"errorbar"} else if (input$errorbar_shape == "一半") {"uperrorbar"}, fun.data = "mean_se", position = position_dodge2(input$width, padding = input$padding, preserve = "single"), width = input$errorbar_width, show.legend = FALSE, colour = "#000000")
    #         } else if (input$errorbar_colour == "颜色方案") {
    #           p <- p + stat_summary(geom = if (input$errorbar_shape == "完全") {"errorbar"} else if (input$errorbar_shape == "一半") {"uperrorbar"}, fun.data = "mean_se", position = position_dodge2(input$width, padding = input$padding, preserve = "single"), width = input$errorbar_width, show.legend = FALSE)
    #         }
    #       } else if (input$dodge_type == "Mean with SD") {
    #         if (input$errorbar_colour == "黑色") {
    #           p <- p + stat_summary(geom = if (input$errorbar_shape == "完全") {"errorbar"} else if (input$errorbar_shape == "一半") {"uperrorbar"}, fun.data = "mean_sdl", fun.args = list(mult = 1), position = position_dodge2(input$width, padding = input$padding, preserve = "single"), width = input$errorbar_width, show.legend = FALSE, colour = "#000000")
    #         } else if (input$errorbar_colour == "颜色方案") {
    #           p <- p + stat_summary(geom = if (input$errorbar_shape == "完全") {"errorbar"} else if (input$errorbar_shape == "一半") {"uperrorbar"}, fun.data = "mean_sdl", fun.args = list(mult = 1), position = position_dodge2(input$width, padding = input$padding, preserve = "single"), width = input$errorbar_width, show.legend = FALSE)
    #         }
    #       }
    #     }

    #     # jitter
    #     if (input$jitter == TRUE) {
    #       if (input$jitter_colour == "黑色") {
    #         p <- p + geom_jitter(position = position_dodge2(input$width, padding = input$padding, preserve = "single"), size = input$jitter_size, show.legend = FALSE, colour = "#000000")
    #       } else if (input$jitter_colour == "颜色方案") {
    #         p <- p + geom_jitter(position = position_dodge2(input$width, padding = input$padding, preserve = "single"), size = input$jitter_size, show.legend = FALSE)
    #       }
    #     }
    #   } else if (input$position == "堆叠") {
    #     p <- p + stat_summary(geom = "bar", fun = "mean", alpha = input$alpha, position = position_stack(), width = input$width)
    #   } else if (input$position == "堆叠并填充到1") {
    #     p <- p + stat_summary(geom = "bar", fun = "mean", alpha = input$alpha, position = position_fill(), width = input$width)
    #   }

    #   return(p)
    # })

    # return(ggobj())
  })
}