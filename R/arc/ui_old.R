# navradio_cat <- function(label) {
#   tags$span(label, style = "font-weight: bolder; color: #ff0000;")
# }

# navradio_div <- function(value, label, checked = NULL) {
#   tags$div(
#     class = "navradio", style = "padding-left: 10px;",
#     tags$label(tags$input(type = "radio", name = "nav", value = value, tags$span(label), checked = checked))
#   )
# }

# ui <- tagList(
#   tags$head(
#     tags$link(rel = "shortcut icon", href = "favicon.ico"),
#     tags$link(rel = "stylesheet", type = "text/css", href = "style.css"),
#   ),
#   useShinyjs(),
#   page_fillable(
#     title = "Shiomix",
#     padding = 0,
#     layout_sidebar(
#       sidebar = sidebar(open = "always",
#         tags$div(id = "nav", class = "shiny-input-radiogroup shiny-input-container",
#           navradio_div(value = "app_intro", label = HTML("<b>Shiomix</b>"), checked = "checked"),
#           navradio_cat("Data Science"),
#           tags$div(style = "padding-left: 10px",
#             navradio_div(value = "datascience_intro", label = "Introduction"),
#             tags$b("Statistics"),
#             navradio_div(value = "ttest", label = "ttest"),
#             tags$b("Visualization"),
#             navradio_div(value = "饼图", label = "饼图"),
#             navradio_div(value = "柱状图", label = "柱状图"),
#             navradio_div(value = "PCA主成分分析", label = "PCA主成分分析"),
#             navradio_div(value = "火山图", label = "火山图"),
#             navradio_div(value = "热图", label = "热图"),
#             navradio_div(value = "南丁格尔玫瑰图", label = "南丁格尔玫瑰图"),
#           ),
#           navradio_cat("Omics"),
#           tags$div(style = "padding-left: 10px",
#             navradio_div(value = "omics_intro", label = "Introduction"),
#             tags$b("Universal"),
#             navradio_div(value = "Data Preprocessing", label = "Data Preprocessing"),
#           ),
#           navradio_cat("Mass tools"),
#           tags$div(style = "padding-left: 10px",
#             navradio_div(value = "masstools_intro", label = "Introduction"),
#             tags$b("enviPat"),
#             navradio_div(value = "formula_handle", label = "formula_handle"),
#             navradio_div(value = "formula_calc_single", label = "formula_calc_single"),
#             navradio_div(value = "formula_calc", label = "formula_calc"),
#             tags$b("FSH pipeline"),
#             navradio_div(value = "TraceFinder export to Matrix", label = "TraceFinder export to Matrix"),
#           ),
#           navradio_cat("Toolkits"),
#           tags$div(style = "padding-left: 10px",
#             navradio_div(value = "toolkits_intro", label = "Introduction"),
#             navradio_div(value = "MD5 Check", label = "MD5 Check")
#           )
#         )
#       ),
#       navset_hidden(
#         id = "nav_main",
#         nav_panel_hidden("app_intro", includeMarkdown("homepage.md")),
#         # data science
#         nav_panel_hidden("datascience_intro", includeMarkdown("data-science/data-science.md")),
#         nav_panel_hidden("ttest", ttest_ui("ttest")),

#         nav_panel_hidden("饼图", pie_ui("pie")),
#         nav_panel_hidden("柱状图", bar_ui("bar")),
#         nav_panel_hidden("PCA主成分分析", pca_ui("pca")),
#         nav_panel_hidden("火山图", volcano_ui("volcano")),
#         nav_panel_hidden("热图", heatmap_ui("heatmap")),
#         nav_panel_hidden("南丁格尔玫瑰图", nightingale_ui("nightingale")),
#         # omics
#         nav_panel_hidden("omics_intro", includeMarkdown("omics/omics.md")),


#         nav_panel_hidden("Data Preprocessing", pp_ui("pp")),



#         # mass tools
#         nav_panel_hidden("masstools_intro", includeMarkdown("mass-tools/mass-tools.md")),

#         nav_panel_hidden("formula_handle", formula_handle_ui("formula_handle")),
#         nav_panel_hidden("formula_calc_single", formula_calc_single_ui("formula_calc_single")),
#         nav_panel_hidden("formula_calc", formula_calc_ui("formula_calc")),

#         # toolkits
#         nav_panel_hidden("toolkits_intro", includeMarkdown("toolkits/toolkits.md")),
#         nav_panel_hidden("MD5 Check", md5_ui("md5"))
#       )
#     )






#     # navset_pill_list(widths = c(2, 10),
#     #   nav_panel("Introduction", includeMarkdown("homepage.md")),
#     #   "Data Science",
#     #   nav_panel("Introduction", includeMarkdown("data-science/data-science.md")),
#     #   # nav_panel("Aggregate", aggr_ui("aggr")),
#     #   nav_menu("Statistics", align = "right",
#     #     nav_panel("ttest", ttest_ui("ttest"))
#     #   ),
#     #   nav_menu("Visualization", align = "right",
#     #     nav_panel("饼图", pie_ui("pie")),
#     #     nav_panel("柱状图", bar_ui("bar")),
#     #     nav_panel("火山图", volcano_ui("volcano")),
#     #     nav_panel("南丁格尔玫瑰图", nightingale_ui("nightingale"))
#     #   ),
#     #   "Omics",
#     #   nav_panel("Introduction", includeMarkdown("omics/omics.md")),
#     #   nav_panel("Data Preprocessing", pp_ui("pp")),
#     #   "Mass tools",
#     #   nav_panel("Introduction", includeMarkdown("mass-tools/mass-tools.md")),
#     #   nav_menu("enviPat", align = "right",
#     #     nav_panel("formula_handle", formula_handle_ui("formula_handle")),
#     #     nav_panel("formula_calc_single", formula_calc_single_ui("formula_calc_single")),
#     #     nav_panel("formula_calc", formula_calc_ui("formula_calc"))
#     #   ),
#     #   "Toolkits",
#     #   nav_panel("Introduction", includeMarkdown("toolkits/toolkits.md")),
#     #   nav_panel("MD5 Check", md5_ui("md5"))
#     # )
#   )
# )