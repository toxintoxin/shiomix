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
#         nav_panel_hidden("ttest", ttestUI("ttest")),

#         nav_panel_hidden("饼图", pieUI("pie")),
#         nav_panel_hidden("柱状图", barUI("bar")),
#         nav_panel_hidden("PCA主成分分析", pcaUI("pca")),
#         nav_panel_hidden("火山图", volcanoUI("volcano")),
#         nav_panel_hidden("热图", heatmapUI("heatmap")),
#         nav_panel_hidden("南丁格尔玫瑰图", nightingaleUI("nightingale")),
#         # omics
#         nav_panel_hidden("omics_intro", includeMarkdown("omics/omics.md")),


#         nav_panel_hidden("Data Preprocessing", ppUI("pp")),



#         # mass tools
#         nav_panel_hidden("masstools_intro", includeMarkdown("mass-tools/mass-tools.md")),

#         nav_panel_hidden("formula_handle", formula_handleUI("formula_handle")),
#         nav_panel_hidden("formula_calc_single", formula_calc_singleUI("formula_calc_single")),
#         nav_panel_hidden("formula_calc", formula_calcUI("formula_calc")),

#         nav_panel_hidden("TraceFinder export to Matrix", tfUI("tf")),
#         # toolkits
#         nav_panel_hidden("toolkits_intro", includeMarkdown("toolkits/toolkits.md")),
#         nav_panel_hidden("MD5 Check", md5UI("md5"))
#       )
#     )






#     # navset_pill_list(widths = c(2, 10),
#     #   nav_panel("Introduction", includeMarkdown("homepage.md")),
#     #   "Data Science",
#     #   nav_panel("Introduction", includeMarkdown("data-science/data-science.md")),
#     #   # nav_panel("Aggregate", aggrUI("aggr")),
#     #   nav_menu("Statistics", align = "right",
#     #     nav_panel("ttest", ttestUI("ttest"))
#     #   ),
#     #   nav_menu("Visualization", align = "right",
#     #     nav_panel("饼图", pieUI("pie")),
#     #     nav_panel("柱状图", barUI("bar")),
#     #     nav_panel("火山图", volcanoUI("volcano")),
#     #     nav_panel("南丁格尔玫瑰图", nightingaleUI("nightingale"))
#     #   ),
#     #   "Omics",
#     #   nav_panel("Introduction", includeMarkdown("omics/omics.md")),
#     #   nav_panel("Data Preprocessing", ppUI("pp")),
#     #   "Mass tools",
#     #   nav_panel("Introduction", includeMarkdown("mass-tools/mass-tools.md")),
#     #   nav_menu("enviPat", align = "right",
#     #     nav_panel("formula_handle", formula_handleUI("formula_handle")),
#     #     nav_panel("formula_calc_single", formula_calc_singleUI("formula_calc_single")),
#     #     nav_panel("formula_calc", formula_calcUI("formula_calc"))
#     #   ),
#     #   nav_panel("TraceFinder export to Matrix", tfUI("tf")),
#     #   "Toolkits",
#     #   nav_panel("Introduction", includeMarkdown("toolkits/toolkits.md")),
#     #   nav_panel("MD5 Check", md5UI("md5"))
#     # )
#   )
# )